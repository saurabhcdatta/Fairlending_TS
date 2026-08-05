# =============================================================================
# 00_reference.R  --  build the three reference files in work_dir. Run ONCE.
# Fully standalone: no old pipeline, no other folders.
#
#   pmms_weekly.csv    date, pmms            weekly Freddie Mac 30yr rate
#   jumbo_2025.csv     fips, one_unit_limit  FHFA conforming limits by county
#   msa_crosswalk.csv  fips, cbsa, metro     county -> CBSA / metro status
#
# For each file, in order:
#   1. final file already in work_dir                     -> done
#   2. a manually staged RAW download is in work_dir      -> normalize it
#   3. try to download (several methods; proxies vary)    -> normalize it
#   4. stop and tell you EXACTLY what to download in the browser and what
#      filename to save it as in work_dir; then just rerun this script.
# =============================================================================

library(data.table)

source("settings.R")

# download with whatever method the network allows (corporate proxies differ).
# IMPORTANT: a BROWSER User-Agent is sent on every attempt -- data.nber.org
# rejects R's default UA with bot detection (browser downloads work, script
# downloads 403). Windows' built-in curl.exe is tried last for good measure.
.grab <- function(url, dest) {
  ua <- paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
               "AppleWebKit/537.36 (KHTML, like Gecko) ",
               "Chrome/126.0 Safari/537.36")
  old_ua <- getOption("HTTPUserAgent")
  on.exit(options(HTTPUserAgent = old_ua), add = TRUE)
  options(HTTPUserAgent = ua)
  methods <- c("default", if (.Platform$OS.type == "windows") "wininet",
               "libcurl", if (nzchar(Sys.which("curl"))) "curlexe")
  for (m in methods) {
    ok <- tryCatch({
      if (m == "curlexe")
        download.file(url, dest, mode = "wb", quiet = TRUE, method = "curl",
                      extra = paste0(
          "-L --fail --compressed -A \"", ua, "\" ",
          "-e \"https://www.nber.org/\" ",
          "-H \"Accept: text/html,application/xhtml+xml,application/xml;",
          "q=0.9,*/*;q=0.8\" ",
          "-H \"Accept-Language: en-US,en;q=0.9\""))
      else
        download.file(url, dest, mode = "wb", quiet = TRUE, method = m,
                      headers = c(`User-Agent` = ua))
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok && file.exists(dest) && file.size(dest) > 1000) return(TRUE)
    suppressWarnings(file.remove(dest))
  }
  FALSE
}
.pick <- function(nms, pattern) {              # first column matching pattern
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) stop("no column matching '", pattern, "' among: ",
                         paste(nms, collapse = ", "))
  hit[1]
}
.fips <- function(state, county)               # 2-digit + 3-digit -> "SSCCC"
  sprintf("%02d%03d", as.integer(state), as.integer(county))

# one engine for all three files: skip / staged / download / instruct.
# CSV ONLY throughout -- no xlsx, no openxlsx, no zip package needed.
# minimal parser for the Census API's JSON array-of-arrays (all values are
# quoted strings; names may contain commas, which stay inside the quotes)
.census_rows <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "")
  txt <- sub("^\\s*\\[", "", txt); txt <- sub("\\]\\s*$", "", txt)
  rows <- strsplit(txt, "\\]\\s*,\\s*\\[")[[1]]
  lapply(rows, function(r) {
    vals <- regmatches(r, gregexpr('"(\\\\.|[^"\\\\])*"', r))[[1]]
    gsub('^"|"$', "", vals)
  })
}

# CENSUS API fallback for the MSA crosswalk (api.census.gov: a machine API,
# keyless at this volume, no bot wall -- unlike data.nber.org). Two GETs:
#   1. summary level 313: every county inside each CBSA  -> fips, cbsa
#   2. summary level 310: every CBSA's NAME              -> "... Metro Area"
#      vs "... Micro Area" gives the metro flag
# Counties outside any CBSA are absent, exactly like the NBER file's classic
# format; 02_prepare.R assigns them non-metro by default.
.census_api_msa <- function() {
  base <- "https://api.census.gov/data/2023/acs/acs5?get=NAME"
  msa  <- "metropolitan%20statistical%20area/micropolitan%20statistical%20area"
  f1 <- tempfile(fileext = ".json"); f2 <- tempfile(fileext = ".json")
  ok1 <- .grab(paste0(base, "&for=county:*&in=", msa, ":*",
                      "&in=state%20(or%20part):*"), f1)
  ok2 <- .grab(paste0(base, "&for=", msa, ":*"), f2)
  if (!ok1 || !ok2) return(NULL)
  r1 <- .census_rows(f1); r2 <- .census_rows(f2)
  h1 <- tolower(r1[[1]]); h2 <- tolower(r2[[1]])
  i_msa <- grep("metropolitan", h1)[1]; i_st <- grep("state", h1)[1]
  i_co  <- which(h1 == "county")[1]
  d1 <- rbindlist(lapply(r1[-1], function(v)
          data.table(cbsa = v[i_msa],
                     fips = sprintf("%02d%03d", as.integer(v[i_st]),
                                    as.integer(v[i_co])))))
  j_msa <- grep("metropolitan", h2)[1]; j_nm <- which(h2 == "name")[1]
  d2 <- rbindlist(lapply(r2[-1], function(v)
          data.table(cbsa = v[j_msa],
                     metro = as.integer(grepl("Metro Area", v[j_nm])))))
  out <- merge(d1, d2, by = "cbsa")[, .(fips, cbsa, metro)]
  unique(out[!is.na(cbsa) & cbsa != ""], by = "fips")
}

# raw_names: staged filenames accepted, FIRST FOUND WINS -- includes the
# browser's original download name, so no renaming is ever needed.
# alt: optional function returning the FINAL normalized table (Census API).
.build <- function(final, raw_names, url, normalize, reader = fread,
                   alt = NULL) {
  if (file.exists(out(final))) {
    cat(final, "already present\n"); return(invisible())
  }
  staged <- raw_names[file.exists(out(raw_names))][1]
  src <- if (!is.na(staged)) {
    cat(final, "<- normalizing staged file", staged, "\n"); out(staged)
  } else {
    tmp <- tempfile(fileext = ".csv")
    if (.grab(url, tmp)) { cat(final, "<- downloaded\n"); tmp } else NA
  }
  if (is.na(src) && !is.null(alt)) {
    cat(final, "<- primary download blocked; trying the Census API...\n")
    a <- alt()
    if (!is.null(a) && nrow(a) > 100) {
      fwrite(a, out(final))
      cat(final, "<- built from api.census.gov (", nrow(a), "counties )\n")
      return(invisible())
    }
    cat("  Census API also unavailable.\n")
  }
  if (is.na(src))
    stop(final, ": download blocked. In your BROWSER open\n  ", url,
         "\nsave it into  ", work_dir, "  (its own name ",
         basename(url), " is fine, or ", raw_names[1], ")",
         "\nthen rerun 00_reference.R (it will normalize it).", call. = FALSE)
  fwrite(normalize(reader(src)), out(final))
}

# --- 1. PMMS weekly (FRED, no API key) -------------------------------------------
.build("pmms_weekly.csv", c("raw_pmms.csv", "fredgraph.csv", "MORTGAGE30US.csv"),
       "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MORTGAGE30US",
       function(p) {
         setnames(p, 1:2, c("date", "pmms"))   # by position; header names vary
         p <- p[!is.na(suppressWarnings(as.numeric(pmms)))]
         p[, .(date = as.Date(date), pmms = as.numeric(pmms))]
       })

# --- 2. FHFA conforming limits, 2025 ----------------------------------------------
.build("jumbo_2025.csv",
       c("raw_fhfa_2025.csv",
         "fullcountyloanlimitlist2025_hera-based_final_flat.csv"),
       paste0("https://www.fhfa.gov/document/d/cll/",
              "fullcountyloanlimitlist2025_hera-based_final_flat.csv"),
       function(j) {
         st <- .pick(names(j), "fips.?state"); co <- .pick(names(j), "fips.?county")
         lm <- .pick(names(j), "one.?unit")
         unique(data.table(fips = .fips(j[[st]], j[[co]]),
                           one_unit_limit = as.numeric(gsub("[$,]", "",
                                                            j[[lm]]))))
       })

# --- 3. MSA crosswalk ---------------------------------------------------------------
# Current NBER location (files moved into a /2023/ subfolder, May 2026 update):
#   https://data.nber.org/cbsa-csa-fips-county-crosswalk/2023/cbsa2fipsxw_2023.csv
# If staging by hand, EITHER save that csv as raw_msa.csv, OR open the Census
# list1_2023.xlsx in Excel and File > Save As > CSV named raw_msa.csv -- the
# reader below finds the header row automatically (the Census file has two
# title rows above it), so both work. No xlsx packages needed.
.read_msa_csv <- function(path) {
  hdr <- grep("fips.?state", readLines(path, n = 10, warn = FALSE),
              ignore.case = TRUE)[1]
  if (is.na(hdr))
    stop("No 'FIPS State' header in the first 10 lines of ", path,
         " -- is this the right file?", call. = FALSE)
  fread(path, skip = hdr - 1L)
}
.build("msa_crosswalk.csv",
       c("raw_msa.csv", "cbsa2fipsxw_2023.csv", "cbsa2fipsxw.csv",
         "list1_2023.csv"),
       paste0("https://data.nber.org/cbsa-csa-fips-county-crosswalk/",
              "2023/cbsa2fipsxw_2023.csv"),
       function(x) {
         st <- .pick(names(x), "fips.?state"); co <- .pick(names(x), "fips.?county")
         cb <- .pick(names(x), "cbsa.?code|^cbsa$")
         ms <- .pick(names(x), "micropolitan")   # matches NBER AND Census headers
         x <- x[!is.na(suppressWarnings(as.integer(x[[st]]))) &
                !is.na(suppressWarnings(as.integer(x[[co]])))]   # footnote rows
         d <- data.table(fips  = .fips(x[[st]], x[[co]]),
                         cbsa  = as.character(x[[cb]]),
                         metro = as.integer(grepl("^Metro", x[[ms]])))
         unique(d[!is.na(cbsa) & cbsa != ""], by = "fips")
       },
       reader = .read_msa_csv,
       alt = .census_api_msa)

# --- 4. show what landed --------------------------------------------------------------
for (rf in c("pmms_weekly.csv", "jumbo_2025.csv", "msa_crosswalk.csv")) {
  d <- fread(out(rf))
  cat(sprintf("%-18s %s rows | cols: %s\n", rf,
              format(nrow(d), big.mark = ","), paste(names(d), collapse = ", ")))
}
