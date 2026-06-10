# ============================================================================
# 01_build.R  —  HMDA SAS -> partitioned Parquet panel  (v3: merged build)
# ----------------------------------------------------------------------------
# Merges the proven elements of the prior 1_Build_HMDA_Parquet_File.R (v2)
# with the typed-harmonization layer of this pipeline:
#   FROM v2 : deterministic schema (all-character fallback for unlisted cols),
#             atomic tmp->move writes, zstd + dictionary encoding, per-file
#             ok/skip/fail result tracking with timings, file logging,
#             error tolerance (one bad year doesn't kill the batch),
#             sequential reads (HMDA LAR too large for parallel reads).
#   FROM v1 : typed analysis columns at rest (no cast at read time),
#             "Exempt" captured as explicit *_exempt flags, hive partitioning
#             by data_year, schema-presence drift report.
# Deterministic typing rule (drift-proof): every column gets exactly one of
#   NUMERIC_FROM_CHAR -> double (+ _exempt flag) | KEEP_CHAR -> character |
#   already numeric -> double | Date -> Date | anything else -> character.
# ============================================================================

# Character-stored fields that are logically NUMERIC (regulatory-file names).
NUMERIC_FROM_CHAR <- c(
  "interest_rate", "rate_spread", "discount_points", "lender_credits",
  "orig_charges", "tot_ln_costs", "tot_points_fees", "property_value",
  "ltv_combined_orig", "income_orig", "intro_rate_period",
  "prepayment_penalty", "multi_fam_affordale_units"
)

# Identifiers / geography / free text kept as CHARACTER.
KEEP_CHAR <- c(
  "uli", "lei", "nmlsr_identifier", "property_census_tract", "property_state",
  "property_zip", "property_city", "cu_name", "name", "address", "city",
  "state", "zip", "contact_name", "contact_email", "contact_phone",
  "ein_1", "taxid", "submission_date", "debt_to_inc",
  "denial5", "app_race6", "app_race7", "app_race8", "app_ethnic6",
  "coapp_race6", "coapp_race7", "coapp_race8", "coapp_ethnic6",
  "credit_score_model_app1", "credit_score_model_co1",
  "auto_undrwrting_sys6", "auto_undrwrting_sys_rslt6"
)

# Modeling fields forced to NUMERIC every year (parse_num_char handles years
# where they arrive as character). Everything not in an explicit list below
# becomes CHARACTER unconditionally -- the v2 "all_character" fallback, which
# is what makes the schema identical across years no matter how types drift.
KNOWN_NUMERIC <- c(
  "action_type", "loan_type", "loan_purpose", "lien", "occupancy_type",
  "construction_method", "tot_units", "ln_term", "loan_amt", "income",
  "ltv_combined", "preapproval", "hoepa", "heloc", "reverse_mrtg",
  "business_ln", "balloon_payment", "i_only_payment", "neg_amortization",
  "non_amort_features", "open_end_credit", "purchaser_type",
  "application_submission", "payable_to_institution",
  "property_county_fip", "lar_count", "cu_number", "cu_type", "agency",
  paste0("app_race", 1:5), paste0("coapp_race", 1:5),
  paste0("app_ethnic", 1:5), paste0("coapp_ethnic", 1:5),
  "gender_app", "gender_co", "age_app", "age_co",
  paste0("auto_undrwrting_sys", 1:5), paste0("auto_undrwrting_sys_rslt", 1:5),
  paste0("denial", 1:4),
  grep("^assets", c("assets_mil"), value = TRUE)
)

# Date fields: keep Date if haven converted; else parse from SAS epoch.
KNOWN_DATE <- c("action_date", "app_date")

# ---- logging (logger if installed; message() fallback) ----------------------
.has_logger <- requireNamespace("logger", quietly = TRUE)
.log_file   <- NULL
init_build_log <- function(cfg) {
  log_dir <- file.path(cfg$paths$out_dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  .log_file <<- file.path(log_dir,
    sprintf("build_parquet_%s.log", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (.has_logger) {
    logger::log_appender(logger::appender_tee(.log_file))
    logger::log_threshold(logger::INFO)
  }
  invisible(.log_file)
}
log_info  <- function(...) { msg <- paste0(...)
  if (.has_logger) logger::log_info(msg) else {
    message("[INFO] ", msg)
    if (!is.null(.log_file)) cat(format(Sys.time()), "[INFO]", msg, "\n",
                                 file = .log_file, append = TRUE) } }
log_error <- function(...) { msg <- paste0(...)
  if (.has_logger) logger::log_error(msg) else {
    message("[ERROR] ", msg)
    if (!is.null(.log_file)) cat(format(Sys.time()), "[ERROR]", msg, "\n",
                                 file = .log_file, append = TRUE) } }

# ---- source resolution + format-dispatching reader ---------------------------
#' Path to a year's source file: per-year override first, then source mode.
source_path <- function(year, cfg) {
  y  <- as.character(year)
  ov <- cfg$source_override
  if (!is.null(ov) && y %in% names(ov)) return(ov[[y]])
  if (identical(cfg$source_mode, "stata"))
    cfg$raw_files[[y]]
  else
    file.path(cfg$paths$sas_dir, cfg$sas_files[[y]])
}

#' Diagnose whether each year's source can be OPENED at all (1 row, 1 col --
#' allocation-at-open problems like oversized strL tables fail even this).
diagnose_sources <- function(cfg) {
  rbindlist(lapply(cfg$years, function(y) {
    p <- source_path(y, cfg)
    r <- tryCatch({ read_source(p, n_max = 1, col_select = "uli"); "ok" },
                  error = function(e) conditionMessage(e))
    data.table(year = y, source = basename(p),
               status = if (identical(r, "ok")) "ok" else "FAIL",
               detail = if (identical(r, "ok")) "" else r)
  }))
}

#' Read one source file; dispatch on extension (.dta Stata / .sas7bdat SAS).
#' Replaces the SAS PROC IMPORT stage: raw Stata is read directly, so the
#' .dta -> sas7bdat hop (and its duplicate disk copy) is eliminated.
#' `skip` + `n_max` enable chunked reads of national-scale files.
#' QUIRK: for .dta, ReadStat treats row_limit = 0 as NO LIMIT, so a naive
#' n_max = 0 "header scan" reads the ENTIRE file (OOM on big years). We
#' therefore floor header scans at one row.
#' col_select prunes columns AT the read; any_of() tolerates absent columns.
read_source <- function(path, n_max = Inf, col_select = NULL, skip = 0) {
  ext <- tolower(tools::file_ext(path))
  if (n_max == 0) n_max <- 1   # ReadStat row_limit-0 = unlimited; see QUIRK
  if (ext == "dta") {
    df <- if (is.null(col_select))
      haven::read_dta(path, n_max = n_max, skip = skip)
    else
      haven::read_dta(path, n_max = n_max, skip = skip,
                      col_select = tidyselect::any_of(col_select))
    haven::zap_formats(haven::zap_labels(df))   # plain vectors, not labelled
  }
  else if (ext == "sas7bdat") {
    if (is.null(col_select))
      haven::read_sas(path, n_max = n_max, skip = skip)
    else
      haven::read_sas(path, n_max = n_max, skip = skip,
                      col_select = tidyselect::any_of(col_select))
  }
  else stop("Unsupported source format: ", path)
}

#' Parquet writer wrapper (separable so tests can override the writer).
.write_parquet <- function(df, path, cfg) {
  arrow::write_parquet(df, path,
                       compression       = cfg$build$compression,
                       compression_level = cfg$build$compression_level,
                       use_dictionary    = TRUE)
}

# ---- typing helpers ----------------------------------------------------------
parse_num_char <- function(x, na_strings = cfg$sentinels$na_strings) {
  if (is.numeric(x)) return(as.double(x))
  s <- trimws(as.character(x))
  s[s %in% na_strings] <- NA_character_
  s <- gsub("[,$%]", "", s)
  suppressWarnings(as.double(s))
}

flag_exempt <- function(x) {
  s <- trimws(as.character(x))
  grepl("^exempt$", s, ignore.case = TRUE) |
    s == as.character(cfg$sentinels$exempt_code)
}

blank_codes <- function(x, codes) {
  x <- suppressWarnings(as.double(x)); x[x %in% codes] <- NA_real_; x
}

#' Harmonize one year to a DETERMINISTIC schema (see header rule).
harmonize_year <- function(dt, year) {
  setDT(dt)
  setnames(dt, tolower(names(dt)))

  for (v in intersect(NUMERIC_FROM_CHAR, names(dt))) {
    dt[, (paste0(v, "_exempt")) := flag_exempt(get(v))]
    dt[, (v) := parse_num_char(get(v))]
  }
  for (v in intersect(c("credit_score_app", "credit_score_co"), names(dt)))
    dt[, (v) := blank_codes(get(v), c(7777, 8888, 9999, 1111))]
  for (v in intersect(c("age_app", "age_co"), names(dt)))
    dt[, (v) := blank_codes(get(v), cfg$sentinels$age_na)]
  for (v in intersect(KEEP_CHAR, names(dt)))
    dt[, (v) := as.character(get(v))]

  # Known modeling numerics: force to double regardless of arrival type.
  for (v in setdiff(intersect(KNOWN_NUMERIC, names(dt)),
                    c("credit_score_app", "credit_score_co", "age_app", "age_co")))
    dt[, (v) := parse_num_char(get(v))]

  # Known dates: Date if haven converted; else SAS epoch numeric / character.
  for (v in intersect(KNOWN_DATE, names(dt))) {
    x <- dt[[v]]
    if (inherits(x, "Date")) next
    if (inherits(x, "POSIXt")) { dt[, (v) := as.Date(x)]; next }
    dt[, (v) := as.Date(suppressWarnings(as.double(as.character(x))),
                        origin = "1960-01-01")]
  }

  # EVERYTHING ELSE -> character unconditionally (v2 all_character fallback).
  # This is what guarantees an identical schema across years no matter how a
  # residual column's type drifts in the source SAS files.
  handled <- c(NUMERIC_FROM_CHAR, paste0(NUMERIC_FROM_CHAR, "_exempt"),
               KEEP_CHAR, KNOWN_NUMERIC, KNOWN_DATE,
               "credit_score_app", "credit_score_co", "age_app", "age_co",
               "data_year")
  for (v in setdiff(names(dt), handled))
    dt[, (v) := as.character(get(v))]
  dt[, data_year := as.integer(year)]
  dt[]
}

# ---- atomic per-year conversion (v2 convert_one, hive-partitioned) ----------
# ---- chunked per-year conversion (streams national-scale files) -------------
# all_agency files run ~15M+ rows/year; a single read_dta() of a full year is
# 20-40GB in R and OOMs a 32GB machine. We therefore stream in chunks of
# cfg$build$chunk_rows: read chunk -> harmonize -> write part-<i>.parquet
# (atomic tmp+rename) -> free -> next. A ".complete" marker written after the
# last chunk is the skip/resume signal; a crashed year has no marker and is
# rebuilt cleanly. Peak RAM ~ one chunk, regardless of file size.
convert_one_year <- function(year, cfg) {
  t0   <- Sys.time()
  f    <- source_path(year, cfg)
  pdir <- file.path(cfg$paths$parquet_dir, paste0("data_year=", year))
  marker <- file.path(pdir, ".complete")

  if (file.exists(marker) && !isTRUE(cfg$build$overwrite))
    return(list(year = year, status = "skipped", rows = NA_integer_,
                seconds = 0, error = NA_character_))

  tryCatch({
    if (dir.exists(pdir)) unlink(pdir, recursive = TRUE)  # clear partials
    dir.create(pdir, recursive = TRUE, showWarnings = FALSE)
    chunk <- cfg$build$chunk_rows
    if (is.null(chunk) || !is.finite(chunk)) chunk <- Inf

    total <- 0L; i <- 0L
    repeat {
      dt <- read_source(f, n_max = chunk, skip = total,
                        col_select = cfg$build$col_select)
      n <- nrow(dt)
      if (n == 0L) { rm(dt); break }
      dt  <- harmonize_year(as.data.table(dt), year)
      out <- file.path(pdir, sprintf("part-%d.parquet", i))
      tmp <- paste0(out, ".tmp")
      .write_parquet(dt, tmp, cfg)
      file.rename(tmp, out)
      total <- total + n; i <- i + 1L
      log_info("  ", year, " chunk ", i, ": +", format(n, big.mark = ","),
               " rows (cum ", format(total, big.mark = ","), ")")
      rm(dt); gc()
      if (n < chunk) break          # short chunk = end of file
    }
    writeLines(as.character(total), marker)
    list(year = year, status = "ok", rows = total,
         seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
         error = NA_character_)
  }, error = function(e) {
    list(year = year, status = "error", rows = NA_integer_,
         seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
         error = conditionMessage(e))
  })
}

#' Build the harmonized 2019-2025 parquet panel. Sequential by design:
#' national-scale LAR years are too large for parallel reads on 32GB.
build_panel <- function(cfg, overwrite = NULL) {
  if (!is.null(overwrite)) cfg$build$overwrite <- overwrite
  init_build_log(cfg)
  dir.create(cfg$paths$parquet_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$paths$out_dir,     recursive = TRUE, showWarnings = FALSE)
  log_info("Starting HMDA Parquet build -> ", cfg$paths$parquet_dir)
  log_info("Compression: ", cfg$build$compression, " level ",
           cfg$build$compression_level, " | overwrite: ", cfg$build$overwrite)

  log_info("Source mode: ", cfg$source_mode,
           if (identical(cfg$source_mode, "stata"))
             "  (raw .dta read directly; SAS import stage eliminated)" else "")

  paths <- vapply(cfg$years, source_path, character(1), cfg = cfg)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    log_error("Missing input files: ", paste(basename(missing), collapse = ", "))
    stop("Aborting: missing input files.")
  }

  # Schema-presence drift report (header-only reads; fast)
  col_map <- rbindlist(lapply(seq_along(cfg$years), function(i) {
    hdr <- read_source(paths[i], n_max = 0)
    data.table(year = as.character(cfg$years[i]), col = tolower(names(hdr)))
  }))
  presence <- dcast(col_map[, .(year, col, present = TRUE)],
                    col ~ year, value.var = "present", fill = FALSE)
  fwrite(presence, file.path(cfg$paths$out_dir, "schema_presence.csv"))
  drift <- presence[rowSums(as.matrix(presence[, -1])) < length(cfg$sas_files), col]
  if (length(drift))
    log_info("Columns NOT in every year (see schema_presence.csv): ",
             paste(drift, collapse = ", "))

  start <- Sys.time()
  results <- lapply(cfg$years, convert_one_year, cfg = cfg)
  res <- rbindlist(lapply(results, as.data.table))

  for (i in seq_len(nrow(res))) {
    r <- res[i]
    if (r$status == "ok")
      log_info("OK   ", r$year, "  rows=", format(r$rows, big.mark = ","),
               "  (", round(r$seconds, 1), "s)")
    else if (r$status == "skipped")
      log_info("SKIP ", r$year, "  (already exists)")
    else
      log_error("FAIL ", r$year, " -- ", r$error)
  }
  total_min <- round(as.numeric(difftime(Sys.time(), start, units = "mins")), 2)
  log_info("Finished. ok=", sum(res$status == "ok"),
           " skipped=", sum(res$status == "skipped"),
           " errors=",  sum(res$status == "error"),
           " total=", total_min, " min")
  log_info("Log written to: ", .log_file)
  if (any(res$status == "error"))
    stop(sprintf("Build completed with %d error(s). See log: %s",
                 sum(res$status == "error"), .log_file))
  invisible(res)
}

#' Lazily open the harmonized parquet panel (arrow Dataset).
open_panel <- function(cfg) arrow::open_dataset(cfg$paths$parquet_dir)
