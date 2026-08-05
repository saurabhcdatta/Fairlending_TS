# =============================================================================
# 00_assets.R  --  credit-union assets: OCE pull + HMDA xwalk -> cu_assets_2025.csv
# Run ONCE. Why this matters:
#   * the screen is a TOP-200-BY-ASSETS screen -- exam resources go to the
#     largest institutions, so 04_screen.R restricts the tested universe to
#     the largest CUs, exactly as the NCUA methodology does;
#   * it is the only source of CU NAMES (and asset sizes) for readable output.
#
# Both inputs are Stata .dta files read directly in R. Column selection
# happens AT READ (haven col_select), so the multi-decade OCE file never
# loads whole -- 4 columns x ~1M rows is trivial. The join keeps xwalk
# fan-out (one join_number can map to several LEIs) and then the max-asset
# row per LEI. Output: lei, cu_number, name, assets_tot.
# =============================================================================

library(data.table)
library(haven)

source("settings.R")
source("dta_header.R")

year <- 2025

# resolve wanted columns CASE-INSENSITIVELY against what the .dta really has
# (the xwalk codes it LEI, not lei -- same uppercase trap as the raw HMDA file)
.resolve <- function(path, wanted, what) {
  have <- dta_header(path)$name
  m <- vapply(wanted, function(w) {
    hit <- have[tolower(have) == tolower(w)]
    if (length(hit)) hit[1] else NA_character_
  }, character(1))
  if (anyNA(m))
    stop(what, ": column(s) not found under any casing: ",
         paste(wanted[is.na(m)], collapse = ", "),
         "\nColumns actually present: ",
         paste(head(have, 40), collapse = ", "), call. = FALSE)
  m                                  # named: wanted -> true name in the file
}
.read_cols <- function(path, wanted, what) {
  m <- .resolve(path, wanted, what)
  d <- as.data.table(read_dta(path, col_select = unname(m)))
  setnames(d, unname(m), names(m))   # canonical lowercase names downstream
  d
}

.af <- out(sprintf("cu_assets_%d.csv", year))
.fresh <- FALSE
if (file.exists(.af)) {
  .chk <- fread(.af, nrows = 5000)
  if (!"cu_type" %in% names(.chk)) {
    cat("cu_assets file exists but is STALE (no cu_type column, predates",
        "the type split) -- REBUILDING it now.\n")
  } else if (all(.chk$cu_type %in% c(NA, 0L))) {
    cat("cu_assets file exists but cu_type is all NA/0 -- REBUILDING",
        "(the OCE pull will be re-read; check the warning if it repeats).\n")
  } else .fresh <- TRUE
}
if (.fresh) {
  cat(sprintf("cu_assets_%d.csv already present (cu_type populated)\n", year))
} else {
  for (f in c(oce_file, xwalk_file)) if (!file.exists(f))
    stop("Not found: ", f, "\nFix the path in settings.R (changes per release).")

  xw <- .read_cols(xwalk_file, c("join_number", "lei"), "HMDA xwalk")
  xw <- unique(xw[!is.na(lei) & trimws(lei) != ""])
  cat(sprintf("xwalk: %s join_number -> lei rows\n",
              format(nrow(xw), big.mark = ",")))

  oce_cols <- c("join_number", "cu_name", "assets_tot", "q_period_num")
  # cu_type: 1 = federally insured CU, 2 = state insured CU. Read it if the
  # file has it (any casing); warn loudly if absent.
  has_type <- any(tolower(dta_header(oce_file)$name) == "cu_type")
  if (has_type) oce_cols <- c(oce_cols, "cu_type")
  oce <- .read_cols(oce_file, oce_cols, "OCE combined")
  if (!has_type) {
    warning("cu_type not found in the OCE file -- filling NA. ",
            "Type-split screens will not separate.", immediate. = TRUE)
    oce[, cu_type := NA_integer_]
  }
  q <- oce[abs(q_period_num - (year + 0.4)) < 1e-6]        # year-end quarter
  if (!nrow(q)) stop("No OCE rows at quarter ", year, ".4 -- check the file.")

  j <- merge(q, xw, by = "join_number", allow.cartesian = TRUE)
  setorder(j, lei, -assets_tot)
  j <- j[, .SD[1L], by = lei]                              # max assets per lei
  res <- j[, .(lei, cu_number = join_number, name = cu_name, assets_tot,
               cu_type = as.integer(cu_type))]
  setorder(res, -assets_tot)
  fwrite(res, out(sprintf("cu_assets_%d.csv", year)))
  cat(sprintf("cu_assets_%d.csv: %s credit unions | largest: %s ($%.1fB)\n",
              year, format(nrow(res), big.mark = ","), res$name[1],
              res$assets_tot[1] / 1e9))
  cat("By cu_type (1 = federal insured, 2 = state insured):\n")
  print(res[, .(CUs = .N, total_assets_B = round(sum(assets_tot) / 1e9, 1)),
            by = cu_type][order(cu_type)])
}
