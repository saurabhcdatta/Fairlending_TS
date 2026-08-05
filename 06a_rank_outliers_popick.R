# =============================================================================
# 06a -- POPICK-STREAM RUNNER. Thin wrapper: one shared engine, per-stream
# configuration (copies would drift; the engine is the single point of
# truth). Screens with 04a if this stream's flags are missing, then ranks.
# =============================================================================
stream_tag <- "popick"
source("settings.R")
if (!file.exists(out("flags_popick_2025.csv"))) {
  cat("[06a] popick flags not found -- running 04a (popick) first\n")
  source("04a_scientific_screen.R")
}
source("06_rank_outliers.R")
