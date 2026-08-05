# =============================================================================
# read_hmda_2025.R  (v2)  --  inspect the 2025 HMDA .dta WITHOUT tripping the
#                             strL memory failure.
#
# What happened on v1: the file contains strL (long-string) columns, and
# haven loads the ENTIRE embedded strL table at file open -- before reading
# any rows. On a 15.2 GB file that allocation fails, so even n_max = 10000
# dies. n_max cannot help; the crash happens before row 1.
#
# The fix, in two moves:
#   1. Read column NAMES and TYPES straight from the .dta binary header
#      (plain bytes a few KB into the file). No data parser, no strL table,
#      no memory risk. This also answers which columns ARE strL -- and what
#      lei / uli / credit score are actually called.
#   2. Read the sample EXCLUDING the strL columns, so the parser never
#      touches that table. haven first; readstata13 automatic fallback.
#
# Run line by line.
# =============================================================================

library(haven)

dta_path <- "//hqwinfs1/economist/Projects/HMDA/Agency Data/2025/HMDA_05_31_2026/all_agency_hmda_2025_05_31_2026_final.dta"
stopifnot(file.exists(dta_path))

# --- 1. names + types from the binary header (always works, seconds) -----------
source("dta_header.R")

info <- dta_header(dta_path)
cat(sprintf("File: %d columns x %s rows (format %d)\n", nrow(info),
            format(attr(info, "n_rows"), big.mark = ","),
            attr(info, "format")))
cat(sprintf("strL columns (the memory problem): %d\n\n", sum(info$is_strL)))
print(info[info$is_strL, ])                     # which columns are strL
write.csv(info, "hmda_2025_columns.csv", row.names = FALSE)
cat("\nFull column list saved -> hmda_2025_columns.csv\n")

# --- 2. the names we came for ----------------------------------------------------
cat("\nColumns matching lei / uli / score / credit / cu:\n")
print(info[grepl("lei|uli|universal|legal|score|credit|cu_", info$name,
                 ignore.case = TRUE), ])

# --- 3. sample rows, EXCLUDING strL columns ----------------------------------------
keep <- info$name[!info$is_strL]
hmda <- tryCatch(
  read_dta(dta_path, n_max = 10000, col_select = tidyselect::all_of(keep)),
  error = function(e) {
    cat("haven still failed (", conditionMessage(e),
        ") -- falling back to readstata13.\n")
    if (!requireNamespace("readstata13", quietly = TRUE)) {
      ok <- tryCatch({ install.packages("readstata13"); TRUE },
                     error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok || !requireNamespace("readstata13", quietly = TRUE))
        stop("Package readstata13 is required (haven cannot open this file) ",
             "and could not be installed automatically. Install it once from ",
             "a session where CRAN works: install.packages(\"readstata13\")",
             call. = FALSE)
    }
    readstata13::read.dta13(dta_path, select.rows = c(1, 10000),
                            select.cols = keep, convert.factors = FALSE)
  })
cat(sprintf("\nSample read OK: %d rows x %d columns (strL excluded)\n",
            nrow(hmda), ncol(hmda)))
str(hmda[, 1:min(15, ncol(hmda))])

if ("cu_number" %in% names(hmda)) {
  cat(sprintf("\ncu_number non-missing in sample: %d of %d (%.1f%%)\n",
              sum(!is.na(hmda$cu_number)), nrow(hmda),
              100 * mean(!is.na(hmda$cu_number))))
}
