# ============================================================================
# 0c_fetch_reference.R  —  automated reference-data downloads
# ----------------------------------------------------------------------------
# Replaces the manual staging of three reference inputs:
#   1. pmms_weekly.csv   <- FRED series MORTGAGE30US (Freddie Mac PMMS 30yr)
#   2. msa_crosswalk.csv <- NBER Census CBSA-FIPS county crosswalk
#   3. jumbo_<year>.csv  <- FHFA conforming loan limit county flat files
# cu_assets_<year>.csv is NOT fetched here: it derives from internal OCE/5300
# data (0b_oce_to_assets_csv.sas) and has no public source.
#
# Idempotent: existing files are skipped unless overwrite = TRUE, so it is
# safe to call on every run. Each item downloads inside tryCatch and the
# function returns a status table; missing-but-required files stop the
# pipeline later at their usual hard gates with the manual URL in hand.
#
# CORPORATE NETWORK NOTE: downloads use download.file(method = "libcurl").
# If the NCUA proxy blocks libcurl, set options(download.file.method =
# "wininet") before calling (wininet inherits the Windows/IE proxy config),
# or stage the files manually from the URLs printed in the status table.
#
# SOURCE URLS (verified June 2026):
#   FRED  https://fred.stlouisfed.org/graph/fredgraph.csv?id=MORTGAGE30US
#   NBER  https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv
#   FHFA  https://www.fhfa.gov/document/d/cll/
#           fullcountyloanlimitlist<year>_hera-based_final_flat.csv (.xlsx)
#         with legacy-path fallbacks for pre-redesign years.
# ============================================================================

suppressPackageStartupMessages(library(data.table))

# ---- download helper ---------------------------------------------------------
.dl <- function(urls, dest, binary = TRUE) {
  old <- getOption("timeout"); options(timeout = max(600, old)); on.exit(options(timeout = old))
  for (u in urls) {
    ok <- tryCatch({
      utils::download.file(u, dest, mode = if (binary) "wb" else "w",
                           quiet = TRUE,
                           method = getOption("download.file.method", "libcurl"))
      file.exists(dest) && file.size(dest) > 1000   # tiny file = error page
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok)) return(u)
    unlink(dest)
  }
  stop("All candidate URLs failed:\n  ", paste(urls, collapse = "\n  "),
       call. = FALSE)
}

# ---- 1. PMMS weekly (FRED MORTGAGE30US) ---------------------------------------
fetch_pmms <- function(cfg, overwrite = FALSE) {
  dest <- file.path(cfg$paths$ref_dir, "pmms_weekly.csv")
  if (file.exists(dest) && !overwrite) return("skipped (exists)")
  tmp <- tempfile(fileext = ".csv")
  url <- .dl("https://fred.stlouisfed.org/graph/fredgraph.csv?id=MORTGAGE30US",
             tmp, binary = FALSE)
  x <- fread(tmp)
  # fredgraph.csv: col 1 = DATE / observation_date, col 2 = MORTGAGE30US;
  # missing weeks arrive as "." -> NA after coercion, dropped.
  setnames(x, c("date", "pmms"))
  x[, date := as.IDate(date)]
  x[, pmms := suppressWarnings(as.numeric(pmms))]
  x <- x[!is.na(date) & !is.na(pmms)]
  stopifnot("PMMS values implausible"   = x[, all(pmms > 1 & pmms < 20)],
            "PMMS coverage short"       = x[, max(year(date))] >= max(cfg$years),
            "PMMS starts too late"      = x[, min(year(date))] <= min(cfg$years))
  fwrite(x, dest)
  sprintf("ok (%d weeks, %s..%s) <- %s", nrow(x), min(x$date), max(x$date), url)
}

# ---- 2. MSA crosswalk (NBER cbsa2fipsxw) --------------------------------------
# NOTE (Connecticut): the current NBER file follows the latest Census
# delineations, which replace CT counties with planning regions from the
# 2023+ vintages. HMDA county codes for 2019-2024 use the LEGACY CT counties
# (09001-09015); those rows will not match and fall to msa = 0 / "CT_0".
# This affects CT only and only the geography FE granularity; if CT matters
# for a given analysis, supply a legacy-vintage crosswalk manually and the
# skip-if-exists guard will leave it untouched.
fetch_msa_xwalk <- function(cfg, overwrite = FALSE) {
  dest <- file.path(cfg$paths$ref_dir, "msa_crosswalk.csv")
  if (file.exists(dest) && !overwrite) return("skipped (exists)")
  tmp <- tempfile(fileext = ".csv")
  url <- .dl("https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv",
             tmp, binary = FALSE)
  x <- fread(tmp)
  setnames(x, tolower(gsub("[^A-Za-z0-9]", "", names(x))))
  need <- c("fipsstatecode", "fipscountycode", "cbsacode",
            "metropolitanmicropolitanstatis")
  miss <- setdiff(need, names(x))
  if (length(miss))
    stop("NBER crosswalk schema changed; missing: ", paste(miss, collapse = ", "),
         call. = FALSE)
  stopifnot("crosswalk implausibly small" = nrow(x) > 1500)
  fwrite(x, dest)
  sprintf("ok (%d county rows) <- %s", nrow(x), url)
}

# ---- 3. FHFA conforming loan limits per year ----------------------------------
.fhfa_urls <- function(year) {
  stem_l <- sprintf("fullcountyloanlimitlist%d_hera-based_final_flat", year)
  stem_u <- sprintf("FullCountyLoanLimitList%d_HERA-BASED_FINAL_FLAT", year)
  c(sprintf("https://www.fhfa.gov/document/d/cll/%s.csv",  stem_l),
    sprintf("https://www.fhfa.gov/document/d/cll/%s.xlsx", stem_l),
    sprintf("https://www.fhfa.gov/document/%s.xlsx",       stem_l),
    sprintf("https://www.fhfa.gov/DataTools/Downloads/Documents/Conforming-Loan-Limit/%s.xlsx", stem_u))
}

# header row is not always row 1 (a title line often precedes it);
# locate it by signature: a row mentioning the FIPS state-code header.
.find_header_line <- function(signatures) {
  norm <- tolower(gsub("[^A-Za-z0-9]", "", signatures))
  i <- which(grepl("fipsstatecode", norm, fixed = TRUE))[1]
  if (is.na(i))
    stop("Could not locate the FHFA header row (looked for 'FIPS State Code').",
         call. = FALSE)
  i
}

.read_fhfa <- function(path) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    hdr <- .find_header_line(readLines(path, n = 10L, warn = FALSE))
    fread(path, skip = hdr - 1L, header = TRUE)
  } else {
    rdr <- if (requireNamespace("openxlsx2", quietly = TRUE))
      function(p, skip) openxlsx2::read_xlsx(p, start_row = skip + 1L)
    else if (requireNamespace("readxl", quietly = TRUE))
      function(p, skip) readxl::read_excel(p, skip = skip)
    else stop("Reading FHFA .xlsx needs openxlsx2 or readxl.", call. = FALSE)
    probe <- as.data.frame(rdr(path, 0L))
    # header may already be the parsed names (no title row)...
    if (any(tolower(gsub("[^A-Za-z0-9]", "", names(probe))) == "fipsstatecode"))
      return(as.data.table(probe))
    # ...else find it inside the first rows by concatenated-row signature
    sig <- apply(head(probe, 10L), 1L, function(r)
      paste(as.character(r), collapse = ""))
    as.data.table(rdr(path, .find_header_line(sig)))
  }
}

#' Normalize FHFA columns to the exact names 02_derive.R expects.
normalize_fhfa <- function(x) {
  setDT(x)
  key <- tolower(gsub("[^A-Za-z0-9]", "", names(x)))
  map <- c(fipsstatecode  = "fips_state_code",
           fipscountycode = "fips_county_code",
           oneunitlimit   = "One_Unit_Limit",
           twounitlimit   = "Two_Unit_Limit",
           threeunitlimit = "Three_Unit_Limit",
           fourunitlimit  = "Four_Unit_Limit")
  hit <- key %in% names(map)
  setnames(x, names(x)[hit], unname(map[key[hit]]))
  miss <- setdiff(unname(map), names(x))
  if (length(miss))
    stop("FHFA file schema changed; missing after normalization: ",
         paste(miss, collapse = ", "), call. = FALSE)
  out <- x[, unname(map), with = FALSE]
  num <- function(v) suppressWarnings(as.numeric(gsub("[,$ ]", "", as.character(v))))
  for (cn in names(out)) out[, (cn) := num(get(cn))]
  out <- out[!is.na(fips_state_code) & !is.na(fips_county_code)]
  out
}

fetch_jumbo_year <- function(cfg, year, overwrite = FALSE) {
  dest <- file.path(cfg$paths$ref_dir, sprintf("jumbo_%d.csv", year))
  if (file.exists(dest) && !overwrite) return("skipped (exists)")
  urls <- .fhfa_urls(year)
  tmp  <- tempfile(fileext = ".bin")
  url  <- .dl(urls, tmp)
  # extension by winning URL (server content matches its path's extension)
  real <- paste0(tmp, if (grepl("\\.csv$", url)) ".csv" else ".xlsx")
  file.rename(tmp, real)
  x <- normalize_fhfa(.read_fhfa(real))
  stopifnot(
    "FHFA county count implausible" = nrow(x) > 3000,
    "One_Unit_Limit out of range"   =
      x[, all(One_Unit_Limit >= 3e5 & One_Unit_Limit <= 3e6)],
    "unit limits not monotone"      =
      x[, mean(Four_Unit_Limit >= One_Unit_Limit, na.rm = TRUE)] > 0.99)
  fwrite(x, dest)
  sprintf("ok (%d counties, 1-unit %s..%s) <- %s", nrow(x),
          format(min(x$One_Unit_Limit), big.mark = ","),
          format(max(x$One_Unit_Limit), big.mark = ","), url)
}

# ---- driver --------------------------------------------------------------------
fetch_reference <- function(cfg, years = cfg$years, overwrite = FALSE) {
  dir.create(cfg$paths$ref_dir, recursive = TRUE, showWarnings = FALSE)
  do1 <- function(item, fn) tryCatch(fn(),
    error = function(e) paste("FAILED:", conditionMessage(e)))
  status <- data.table(item = character(), status = character())
  status <- rbind(status, data.table(item = "pmms_weekly",
    status = do1("pmms", function() fetch_pmms(cfg, overwrite))))
  status <- rbind(status, data.table(item = "msa_crosswalk",
    status = do1("xwalk", function() fetch_msa_xwalk(cfg, overwrite))))
  for (y in years)
    status <- rbind(status, data.table(item = sprintf("jumbo_%d", y),
      status = do1("jumbo", function() fetch_jumbo_year(cfg, y, overwrite))))
  print(status)
  fails <- status[grepl("^FAILED", status)]
  if (nrow(fails))
    message("\nSome reference downloads failed (corporate proxy?). Either:\n",
            "  options(download.file.method = 'wininet')  and retry, or\n",
            "  stage the file(s) manually in ", cfg$paths$ref_dir, "\n",
            "Required-file gates in 02/04 will stop the pipeline if a\n",
            "needed year is still missing.")
  invisible(status)
}
