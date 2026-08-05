# =============================================================================
# 01_extract.R  --  read the 2025 HMDA .dta, keep credit-union rows, save RDS.
#
# Builds directly on read_hmda_2025.R. Two practical realities and how this
# script handles them, in plain terms:
#   1. The file is ~15M rows and will NOT fit in RAM whole. So we read it in
#      chunks and keep only rows with a cu_number (~13%) as we go.
#   2. Reading a .dta with long-string columns OVER THE NETWORK is pathologically
#      slow (the reader seeks; the share hates seeks). So we copy the file to
#      the local disk first -- a plain sequential copy -- and read the copy.
#
# Output:  hmda_cu_2025.rds  (~2M rows, ALL columns, native types)
# =============================================================================

library(haven)
library(data.table)

source("settings.R")
source("dta_header.R")

chunk_rows <- 2e6          # rows per read; lower it if memory gets tight

# --- 1. local copy (skip if already copied today) ------------------------------
local_dta <- file.path(tempdir(), basename(raw_dta))
if (!file.exists(local_dta) || file.size(local_dta) != file.size(raw_dta)) {
  cat(sprintf("Copying %.1f GB to local disk (the slow part; sequential)...\n",
              file.size(raw_dta) / 1024^3))
  stopifnot(file.copy(raw_dta, local_dta, overwrite = TRUE))
}
cat("Reading from local copy:", local_dta, "\n")

# --- 2. columns and row count from the binary header ---------------------------
# strL (long-string) columns make haven load a giant lookup table AT OPEN --
# the "Unable to allocate memory" failure -- and it does so even when
# col_select excludes them (confirmed on the real 2025 file). So: list the
# strLs from the header, exclude them, and pick the engine ONCE up front.
info <- dta_header(local_dta)
N    <- attr(info, "n_rows")
keep <- info$name[!info$is_strL]
cat(sprintf("%s rows x %d columns; excluding %d strL columns:\n",
            format(N, big.mark = ","), nrow(info), sum(info$is_strL)))
print(info$name[info$is_strL])

use_haven <- tryCatch({
  invisible(read_dta(local_dta, n_max = 1,
                     col_select = tidyselect::all_of(keep))); TRUE
}, error = function(e) FALSE)
if (!use_haven) {
  cat("haven cannot open this file (strL table) -- using readstata13 throughout.\n")
  if (!requireNamespace("readstata13", quietly = TRUE))
    install.packages("readstata13")
}

read_chunk <- function(offset, n) {
  n <- min(n, N - offset)
  if (n <= 0) return(NULL)
  if (use_haven)
    read_dta(local_dta, skip = offset, n_max = n,
             col_select = tidyselect::all_of(keep))
  else
    readstata13::read.dta13(local_dta,
                            select.rows = c(offset + 1, offset + n),
                            select.cols = keep, convert.factors = FALSE)
}

# --- 3. chunked read, keeping CU rows only ---------------------------------------
# NOTE: each chunk re-parses the rows before it (dta reading is sequential),
# so expect later chunks to take progressively longer. All on local disk.
kept <- list()
offset <- 0
repeat {
  chunk <- read_chunk(offset, chunk_rows)
  if (is.null(chunk) || nrow(chunk) == 0) break
  kept[[length(kept) + 1]] <- chunk[!is.na(chunk$cu_number), ]
  offset <- offset + nrow(chunk)
  rm(chunk); gc(FALSE)                       # drop the 2M-row chunk promptly
  cat(sprintf("  read %s of %s rows (%.0f%%), kept %s CU rows\n",
              format(offset, big.mark = ","), format(N, big.mark = ","),
              100 * offset / N,
              format(sum(sapply(kept, nrow)), big.mark = ",")))
  if (offset >= N) break
}

hmda_cu <- rbindlist(kept, fill = TRUE)      # far lighter than base rbind
rm(kept); gc(FALSE)
hmda_cu <- zap_labels(zap_formats(hmda_cu))  # plain R types, no Stata baggage
cat(sprintf("\nDone: %s CU rows x %d columns\n",
            format(nrow(hmda_cu), big.mark = ","), ncol(hmda_cu)))

# --- 4. save ----------------------------------------------------------------------
saveRDS(hmda_cu, out("hmda_cu_2025.rds"), compress = FALSE)  # FALSE = big but fast
stopifnot(file.exists(out("hmda_cu_2025.rds")))              # verify, loudly
cat(sprintf("Saved -> %s (%.2f GB on disk)\n", out("hmda_cu_2025.rds"),
            file.size(out("hmda_cu_2025.rds")) / 1024^3))
