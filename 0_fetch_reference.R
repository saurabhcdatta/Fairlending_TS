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

# null-coalesce (base R >= 4.4 ships this; define for standalone sourcing)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# ---- download helper ---------------------------------------------------------
# Method cascade: libcurl first (direct), then wininet on Windows (inherits
# the Windows/IE PROXY configuration -- on managed networks where only .gov
# is reachable directly, this is usually the one that works), then the curl
# package if installed. Every failed attempt's reason is collected and
# reported, so a proxy block is distinguishable from a 404 or a timeout.
.dl_methods <- function() {
  m <- "libcurl"
  if (.Platform$OS.type == "windows") m <- c(m, "wininet")
  if (requireNamespace("curl", quietly = TRUE)) m <- c(m, "curl-pkg")
  unique(c(getOption("download.file.method", character()), m))
}

.dl <- function(urls, dest, binary = TRUE) {
  old <- getOption("timeout"); options(timeout = max(600, old))
  on.exit(options(timeout = old))
  log <- character()
  for (u in urls) for (meth in .dl_methods()) {
    res <- tryCatch({
      if (meth == "curl-pkg")
        curl::curl_download(u, dest, quiet = TRUE,
                            mode = if (binary) "wb" else "w")
      else
        suppressWarnings(utils::download.file(
          u, dest, mode = if (binary) "wb" else "w",
          quiet = TRUE, method = meth))
      if (file.exists(dest) && file.size(dest) > 1000) "OK"
      else "response too small (error page?)"
    }, error = function(e) conditionMessage(e))
    if (identical(res, "OK")) {
      if (meth != .dl_methods()[1])
        message("    [download] succeeded via method \"", meth, "\" -- ",
                "consider options(download.file.method = \"", 
                sub("curl-pkg", "libcurl", meth), "\") for this session")
      return(u)
    }
    unlink(dest)
    log <- c(log, sprintf("[%s] %s -> %s", meth, u, res))
  }
  stop("All download attempts failed:\n  ", paste(log, collapse = "\n  "),
       "\nIf the browser CAN open these URLs, the corporate proxy is the",
       " cause;\nif the browser CANNOT, the host is network-blocked --",
       " stage the file manually.", call. = FALSE)
}

# ---- 1. PMMS weekly: FRED primary, Freddie Mac archive fallback ---------------
pmms_validate_write <- function(x, cfg, dest, src_label) {
  x <- x[!is.na(date) & !is.na(pmms)]
  stopifnot("PMMS values implausible"   = x[, all(pmms > 1 & pmms < 20)],
            "PMMS coverage short"       = x[, max(year(date))] >= max(cfg$years),
            "PMMS starts too late"      = x[, min(year(date))] <= min(cfg$years))
  setorder(x, date)
  fwrite(x[, .(date, pmms)], dest)
  sprintf("ok (%d weeks, %s..%s) <- %s", nrow(x), min(x$date), max(x$date),
          src_label)
}

# FRED API (api.stlouisfed.org -- a DIFFERENT host from the often-blocked
# fred.stlouisfed.org; the user's macro-indicator script already uses it).
# Key resolution: cfg$fred_api_key, else FRED_API_KEY env var (.Renviron).
.fred_api_key <- function(cfg)
  cfg$fred_api_key %||% Sys.getenv("FRED_API_KEY", unset = NA)

.mask_key <- function(s, key)
  if (is.na(key) || !nzchar(key)) s else gsub(key, "***", s, fixed = TRUE)

pmms_from_fred_api <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("FRED API parsing needs the jsonlite package.", call. = FALSE)
  j <- jsonlite::fromJSON(path)
  if (is.null(j$observations))
    stop("FRED API response had no observations (bad key?).", call. = FALSE)
  x <- as.data.table(j$observations)[, .(date, value)]
  setnames(x, c("date", "pmms"))
  x[, date := as.IDate(date)]
  x[, pmms := suppressWarnings(as.numeric(pmms))]   # "." missings -> NA
  x[]
}

pmms_from_fred <- function(path) {
  x <- fread(path)                       # DATE/observation_date, MORTGAGE30US
  setnames(x, c("date", "pmms"))
  x[, date := as.IDate(date)]
  x[, pmms := suppressWarnings(as.numeric(pmms))]
  x[]
}

# Freddie Mac historicalweeklydata.xlsx: title rows precede a header row whose
# first cell is the week/date column; the 30yr FRM rate is the first column
# whose header mentions "30". Footnote rows fall out as NA dates.
pmms_from_freddie <- function(path) {
  rdr <- if (requireNamespace("readxl", quietly = TRUE))
    function(skip) readxl::read_excel(path, skip = skip, col_types = "text")
  else if (requireNamespace("openxlsx2", quietly = TRUE))
    function(skip) openxlsx2::read_xlsx(path, start_row = skip + 1L,
                                        col_types = "character")
  else stop("Reading the Freddie Mac PMMS .xlsx needs readxl or openxlsx2.",
            call. = FALSE)
  probe <- as.data.frame(rdr(0L))
  sig   <- apply(head(probe, 12L), 1L, function(r)
    paste(as.character(r), collapse = ""))
  norm  <- tolower(gsub("[^A-Za-z0-9]", "", c(paste(names(probe), collapse=""), sig)))
  hit   <- which(grepl("week|date", norm))[1]
  if (is.na(hit)) stop("Freddie PMMS layout not recognized.", call. = FALSE)
  d <- as.data.frame(if (hit == 1L) probe else rdr(hit - 1L))
  cn <- tolower(gsub("[^A-Za-z0-9]", "", names(d)))
  rate_col <- which(grepl("30", cn))[1]
  if (is.na(rate_col) || rate_col == 1L)
    stop("Freddie PMMS layout not recognized (no 30yr column).", call. = FALSE)
  out <- data.table(date_raw = as.character(d[[1]]),
                    pmms = suppressWarnings(as.numeric(d[[rate_col]])))
  out[, date := .parse_pmms_dates(date_raw)]
  out[, .(date, pmms)]
}

# dates arrive as ISO text, US text, or Excel serial numbers depending on
# the reader; parse all three, element-wise, NA on failure.
.parse_pmms_dates <- function(v) {
  v <- as.character(v)
  out <- rep(as.Date(NA), length(v))
  ser <- grepl("^[0-9]{4,6}$", v) & !is.na(v)
  out[ser] <- as.Date(suppressWarnings(as.numeric(v[ser])),
                      origin = "1899-12-30")
  for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")) {
    need <- is.na(out) & !is.na(v) & !ser
    if (!any(need)) break
    cand <- as.Date(v[need], format = fmt)
    # %Y greedily accepts 2-digit years as year 24 AD; reject implausible
    cand[!is.na(cand) & as.integer(format(cand, "%Y")) < 1950] <- NA
    out[need] <- cand
  }
  as.IDate(out)
}

fetch_pmms <- function(cfg, overwrite = FALSE) {
  dest <- file.path(cfg$paths$ref_dir, "pmms_weekly.csv")
  if (file.exists(dest) && !overwrite) return("skipped (exists)")
  key  <- .fred_api_key(cfg)
  errs <- character()

  # 1. FRED API (api.stlouisfed.org) -- first whenever a key is available
  if (!is.na(key) && nzchar(key)) {
    r <- tryCatch({
      tmp <- tempfile(fileext = ".json")
      u <- sprintf(paste0("https://api.stlouisfed.org/fred/series/observations",
                          "?series_id=MORTGAGE30US&file_type=json&api_key=%s"),
                   key)
      .dl(u, tmp, binary = FALSE)
      return(pmms_validate_write(pmms_from_fred_api(tmp), cfg, dest,
                                 "FRED API (api.stlouisfed.org)"))
    }, error = function(e) .mask_key(conditionMessage(e), key))
    errs <- c(errs, paste("FRED API:", r))
  } else {
    errs <- c(errs, paste("FRED API: no key (set cfg$fred_api_key or",
                          "FRED_API_KEY in .Renviron)"))
  }

  # 2. fredgraph.csv (fred.stlouisfed.org)
  r <- tryCatch({
    tmp <- tempfile(fileext = ".csv")
    url <- .dl("https://fred.stlouisfed.org/graph/fredgraph.csv?id=MORTGAGE30US",
               tmp, binary = FALSE)
    return(pmms_validate_write(pmms_from_fred(tmp), cfg, dest, url))
  }, error = function(e) conditionMessage(e))
  errs <- c(errs, paste("fredgraph:", r))

  # 3. Freddie Mac archive
  r <- tryCatch({
    tmp <- tempfile(fileext = ".xlsx")
    url <- .dl("https://www.freddiemac.com/pmms/docs/historicalweeklydata.xlsx",
               tmp)
    return(pmms_validate_write(pmms_from_freddie(tmp), cfg, dest, url))
  }, error = function(e) conditionMessage(e))
  errs <- c(errs, paste("Freddie:", r))

  stop(paste(errs, collapse = "\n"), call. = FALSE)
}

# ---- 2. MSA crosswalk (NBER cbsa2fipsxw) --------------------------------------
# NOTE (Connecticut): the current NBER file follows the latest Census
# delineations, which replace CT counties with planning regions from the
# 2023+ vintages. HMDA county codes for 2019-2024 use the LEGACY CT counties
# (09001-09015); those rows will not match and fall to msa = 0 / "CT_0".
# This affects CT only and only the geography FE granularity; if CT matters
# for a given analysis, supply a legacy-vintage crosswalk manually and the
# skip-if-exists guard will leave it untouched.
.xwalk_validate_write <- function(x, dest, src_label) {
  need <- c("fipsstatecode", "fipscountycode", "cbsacode",
            "metropolitanmicropolitanstatis")
  miss <- setdiff(need, names(x))
  if (length(miss))
    stop("crosswalk schema unexpected; missing: ", paste(miss, collapse = ", "),
         call. = FALSE)
  x <- x[!is.na(fipsstatecode) & !is.na(fipscountycode)]
  stopifnot("crosswalk implausibly small" = nrow(x) > 1000)
  fwrite(x, dest)
  sprintf("ok (%d county rows) <- %s", nrow(x), src_label)
}

xwalk_from_nber <- function(path) {
  x <- fread(path)
  setnames(x, tolower(gsub("[^A-Za-z0-9]", "", names(x))))
  x[]
}

# Census delineation file (list1): the .gov source the NBER file derives
# from. Title rows precede the header; footnote rows trail the data (dropped
# via NA fips). The March-2020 vintage (list1_2020.xls) carries LEGACY
# Connecticut counties, matching HMDA 2019-2024 -- preferable to the current
# NBER file for this panel.
xwalk_from_census <- function(path) {
  is_xls <- grepl("\\.xls$", path, ignore.case = TRUE)
  rdr <- if (requireNamespace("readxl", quietly = TRUE))
    function(skip) readxl::read_excel(path, skip = skip)
  else if (!is_xls && requireNamespace("openxlsx2", quietly = TRUE))
    function(skip) openxlsx2::read_xlsx(path, start_row = skip + 1L)
  else stop("Reading the Census delineation file needs readxl",
            if (is_xls) " (.xls requires readxl specifically)", ".",
            call. = FALSE)
  probe <- as.data.frame(rdr(0L))
  if (!any(tolower(gsub("[^A-Za-z0-9]", "", names(probe))) == "fipsstatecode")) {
    sig <- apply(head(probe, 10L), 1L, function(r)
      paste(as.character(r), collapse = ""))
    probe <- as.data.frame(rdr(.find_header_line(sig)))
  }
  x <- as.data.table(probe)
  setnames(x, tolower(gsub("[^A-Za-z0-9]", "", names(x))))
  # Census full name vs NBER Stata-truncated name expected by 02_derive
  long <- grep("^metropolitanmicropolitanstatis", names(x), value = TRUE)[1]
  if (!is.na(long) && long != "metropolitanmicropolitanstatis")
    setnames(x, long, "metropolitanmicropolitanstatis")
  for (cn in c("fipsstatecode", "fipscountycode", "cbsacode"))
    if (cn %in% names(x))
      x[, (cn) := suppressWarnings(as.numeric(get(cn)))]
  x[]
}

fetch_msa_xwalk <- function(cfg, overwrite = FALSE) {
  dest <- file.path(cfg$paths$ref_dir, "msa_crosswalk.csv")
  if (file.exists(dest) && !overwrite) return("skipped (exists)")
  errs <- character()
  # 1. NBER (canonical names, no Excel dependency)
  r <- tryCatch({
    tmp <- tempfile(fileext = ".csv")
    url <- .dl("https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv",
               tmp, binary = FALSE)
    return(.xwalk_validate_write(xwalk_from_nber(tmp), dest, url))
  }, error = function(e) conditionMessage(e))
  errs <- c(errs, paste("NBER:", r))
  # 2. Census .gov delineation files (2020 vintage first: legacy CT counties)
  for (u in c("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls",
              "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2023/delineation-files/list1_2023.xlsx")) {
    r <- tryCatch({
      tmp <- tempfile(fileext = paste0(".", tools::file_ext(u)))
      url <- .dl(u, tmp)
      return(.xwalk_validate_write(xwalk_from_census(tmp), dest, url))
    }, error = function(e) conditionMessage(e))
    errs <- c(errs, paste("Census:", r))
  }
  stop(paste(errs, collapse = "\n"), call. = FALSE)
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

# ---- manual staging: convert browser-downloaded files -------------------------
#' Guaranteed fallback when R cannot reach a host but the browser can:
#' download the raw files in Edge into one folder (e.g. Downloads), then
#'   stage_manual_reference(cfg, "C:/Users/<you>/Downloads")
#' Recognized raw files (any of):
#'   PMMS:   MORTGAGE30US*.csv / fredgraph*.csv / historicalweeklydata*.xlsx
#'   xwalk:  cbsa2fipsxw*.csv / list1*.xls / list1*.xlsx
#'   jumbo:  *ountyloanlimitlist<YEAR>*.csv / .xlsx  (year read from name)
#' Each is parsed, validated, and written to cfg$paths$ref_dir with the
#' exact schema the pipeline expects. Existing reference files are kept
#' unless overwrite = TRUE.
stage_manual_reference <- function(cfg, src_dir, overwrite = FALSE) {
  stopifnot(dir.exists(src_dir))
  dir.create(cfg$paths$ref_dir, recursive = TRUE, showWarnings = FALSE)
  fs <- list.files(src_dir, full.names = TRUE)
  st <- data.table(item = character(), status = character())
  add <- function(i, s) st <<- rbind(st, data.table(item = i, status = s))
  grab <- function(pat) { h <- fs[grepl(pat, basename(fs), ignore.case = TRUE)]
                          if (length(h)) h[which.max(file.mtime(h))] else NA }

  # PMMS
  dest <- file.path(cfg$paths$ref_dir, "pmms_weekly.csv")
  if (file.exists(dest) && !overwrite) add("pmms_weekly", "skipped (exists)")
  else {
    f_csv <- grab("^(MORTGAGE30US|fredgraph).*\\.csv$")
    f_xl  <- grab("^historicalweeklydata.*\\.xlsx$")
    add("pmms_weekly", tryCatch(
      if (!is.na(f_csv)) pmms_validate_write(pmms_from_fred(f_csv), cfg, dest, basename(f_csv))
      else if (!is.na(f_xl)) pmms_validate_write(pmms_from_freddie(f_xl), cfg, dest, basename(f_xl))
      else "no raw file found (MORTGAGE30US*.csv / historicalweeklydata*.xlsx)",
      error = function(e) paste("FAILED:", conditionMessage(e))))
  }

  # crosswalk
  dest <- file.path(cfg$paths$ref_dir, "msa_crosswalk.csv")
  if (file.exists(dest) && !overwrite) add("msa_crosswalk", "skipped (exists)")
  else {
    f_csv <- grab("^cbsa2fipsxw.*\\.csv$")
    f_xl  <- grab("^list1.*\\.(xls|xlsx)$")
    add("msa_crosswalk", tryCatch(
      if (!is.na(f_csv)) .xwalk_validate_write(xwalk_from_nber(f_csv), dest, basename(f_csv))
      else if (!is.na(f_xl)) .xwalk_validate_write(xwalk_from_census(f_xl), dest, basename(f_xl))
      else "no raw file found (cbsa2fipsxw*.csv / list1*.xls[x])",
      error = function(e) paste("FAILED:", conditionMessage(e))))
  }

  # jumbo, year parsed from filename
  jb <- fs[grepl("ountyloanlimitlist[0-9]{4}.*\\.(csv|xlsx)$", basename(fs),
                 ignore.case = TRUE)]
  for (f in jb) {
    y    <- as.integer(regmatches(basename(f),
              regexpr("[0-9]{4}", basename(f))))
    dest <- file.path(cfg$paths$ref_dir, sprintf("jumbo_%d.csv", y))
    if (file.exists(dest) && !overwrite) { add(sprintf("jumbo_%d", y), "skipped (exists)"); next }
    add(sprintf("jumbo_%d", y), tryCatch({
      x <- normalize_fhfa(.read_fhfa(f))
      stopifnot(nrow(x) > 3000,
                x[, all(One_Unit_Limit >= 3e5 & One_Unit_Limit <= 3e6)])
      fwrite(x, dest)
      sprintf("ok (%d counties) <- %s", nrow(x), basename(f))
    }, error = function(e) paste("FAILED:", conditionMessage(e))))
  }
  print(st)
  invisible(st)
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
