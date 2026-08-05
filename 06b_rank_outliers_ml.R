# =============================================================================
# 06b -- ML-STREAM RUNNER. Requires 03b to have produced
# residuals_ml_2025.rds. Screens with 04a (tagged ml) if needed, then ranks.
# =============================================================================
stream_tag <- "ml"
source("settings.R")
if (!file.exists(out("residuals_ml_2025.rds")))
  stop("residuals_ml_2025.rds not found -- run 03b_ml_residuals.R first.",
       call. = FALSE)
if (!file.exists(out("flags_ml_2025.csv"))) {
  cat("[06b] ml flags not found -- running 04a (ml) first\n")
  source("04a_scientific_screen.R")
}
source("06_rank_outliers.R")
