# =============================================================================
# 09_validation.R -- THE VALIDATION DOSSIER. Run after 04a (any stream).
#
# Produces the internal evidence that certifies the screen as an instrument:
#
#  1. PLACEBO TEST      race labels permuted WITHIN each CU, screen rerun
#                       B times. A valid screen flags ~nothing (<= the FDR
#                       rate). Demonstrates false-positive control on YOUR
#                       data, not just under textbook assumptions.
#  2. INJECTION POWER   violators of KNOWN size planted into the REAL
#                       residuals (gap x cell-size grid), screen rerun.
#                       Output = the screen's operating characteristics:
#                       "gaps >= X pp at cells >= N detected with Y%."
#  3. E-VALUE BOUNDS    for every flagged denial/withdrawal cell: how strong
#                       an OMITTED legitimate factor would need to be to
#                       explain the gap away. Converts the omitted-variable
#                       caveat into a quantified robustness exhibit.
#  4. PERSISTENCE       if flags files from other years are present in
#                       work_dir (flags_2023.csv, ...), institution overlap
#                       across years -- replication without field input.
#
# Everything restores the user's real outputs when done (file swaps are
# backed up and reinstated even on error).
# =============================================================================

library(data.table)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
B_placebo    <- 10                  # placebo permutation replicates
inject_gaps  <- c(0.02, 0.05, 0.10) # planted denial-gap sizes (pp/100)
inject_cells <- 5                   # cells cloned per gap size
seed         <- 20250101
# ------------------------------------------------------------------------------

set.seed(seed)
res_f   <- out("residuals_2025.rds")
flags_f <- out("flags_2025.csv")
if (!file.exists(res_f) || !file.exists(flags_f))
  stop("Run the chain through 04a first.", call. = FALSE)

# ---- backup everything 04a writes; restore on exit ------------------------------
.bak <- tempfile("val_bak_"); dir.create(.bak)
.protected <- c("residuals_2025.rds", "flags_2025.csv",
                list.files(out(""), pattern = "^flags_.*_2025\\.csv$"))
for (f in unique(.protected))
  if (file.exists(out(f))) file.copy(out(f), file.path(.bak, f))
# NOTE: on.exit() does not register at the top level of a source()d
# script, so restoration is explicit via tryCatch(finally=) below.
.restore <- function() {
  for (f in list.files(.bak)) file.copy(file.path(.bak, f), out(f),
                                        overwrite = TRUE)
  cat("(original residuals and flags restored)\n")
}

res0 <- readRDS(res_f)

.run_screen <- function(res_mod) {
  saveRDS(res_mod, res_f, compress = FALSE)
  ev <- new.env()
  op <- options(warn = -1)
  sink(tempfile())
  ok <- tryCatch({ sys.source("04a_scientific_screen.R", envir = ev); TRUE },
                 error = function(e) FALSE)
  sink(); options(op)
  if (!ok) stop("04a failed inside validation run.", call. = FALSE)
  fread(flags_f)
}

.permute_within_cu <- function(res) {
  lapply(res, function(d) {
    d <- copy(d)
    d[, group := sample(group), by = lei]
    d
  })
}

tryCatch({
# ---- 0. clean 04a baseline on real labels (self-consistent reference) -----------
cat("== 0. Baseline 04a run on real labels ==\n")
real_flags <- .run_screen(res0)
n_real <- real_flags[, sum(flag)]
cat(sprintf("  baseline: %d flagged cells\n", n_real))

# ---- 1. PLACEBO -------------------------------------------------------------------
cat(sprintf("== 1. PLACEBO: %d permutation replicates ==\n", B_placebo))
plac <- integer(B_placebo)
for (b in seq_len(B_placebo)) {
  fl <- .run_screen(.permute_within_cu(res0))
  plac[b] <- fl[, sum(flag)]
  cat(sprintf("  placebo %2d/%d: %d flags\n", b, B_placebo, plac[b]))
}
cat(sprintf("PLACEBO RESULT: mean %.1f flags (max %d) vs %d on real labels\n",
            mean(plac), max(plac), n_real))
cat(sprintf("  -> under permuted labels the screen flags %.1f%% of the real count\n",
            100 * mean(plac) / max(n_real, 1)))

# ---- 2. INJECTION POWER --------------------------------------------------------------
cat("\n== 2. INJECTION POWER (denial screen) ==\n")
den <- res0$denial
elig <- den[group != "white", .N, by = .(lei, group)][N >= 30]
grid <- CJ(gap = inject_gaps, rep = seq_len(inject_cells))
pw <- list()
for (g in inject_gaps) {
  tgt <- elig[sample(.N, min(inject_cells, .N))]
  res_i <- lapply(res0, copy)
  res_i$denial <- copy(den)
  for (k in seq_len(nrow(tgt)))
    res_i$denial[lei == tgt$lei[k] & group == tgt$group[k],
                 resid_denial := resid_denial + g]
  fl <- .run_screen(res_i)
  hit <- merge(tgt, fl[screen == "denial" & flag == 1, .(lei, group)],
               by = c("lei", "group"))
  pw[[as.character(g)]] <- data.table(gap_pp = g * 100,
                                      injected = nrow(tgt),
                                      detected = nrow(hit),
                                      cell_sizes = paste(sort(tgt$N),
                                                         collapse = ","))
  cat(sprintf("  gap %4.1fpp: %d/%d injected cells detected (n: %s)\n",
              g * 100, nrow(hit), nrow(tgt),
              paste(sort(tgt$N), collapse = ",")))
}
power_tab <- rbindlist(pw)
fwrite(power_tab, out("validation_power_2025.csv"))
}, finally = .restore())
cat("  NOTE: detection depends on peer-stratum richness -- the posterior
",
    " materiality gate shrinks single outliers hard when a group x type
",
    " stratum has few cells. Interpret power on the real data's strata.
")

# ---- 3. E-VALUE BOUNDS ---------------------------------------------------------------
cat("\n== 3. E-VALUES: how strong must an omitted factor be? ==\n")
ev <- real_flags[flag == 1 & screen %in% c("denial", "withdrawal") &
                 !is.na(mu_g) & !is.na(mu_w) & mu_w > 0]
fwrite(data.table(), out("validation_evalues_2025.csv"))  # always exists
if (nrow(ev)) {
  ev[, rr := pmax((mu_w + pmax(gap, 0)) / mu_w, 1 + 1e-9)]
  ev[, evalue := rr + sqrt(rr * (rr - 1))]
  ev <- ev[order(-evalue)]
  fwrite(ev[, .(name, cu_type, screen, group, n_g, gap = round(gap, 4),
                rr = round(rr, 2), evalue = round(evalue, 2))],
         out("validation_evalues_2025.csv"))
  cat("Top flagged cells by robustness to omitted variables:\n")
  print(ev[1:min(8, .N), .(name, screen, group, gap = round(gap, 3),
                           evalue = round(evalue, 2))])
  cat("  Reading: evalue = 2.5 means an unobserved legitimate factor would\n",
      " need risk ratios of 2.5 with BOTH the outcome AND group membership --\n",
      " beyond any observed underwriting variable -- to nullify the gap.\n")
} else cat("  (no flagged denial/withdrawal cells with base rates available)\n")

# ---- 4. PERSISTENCE ACROSS YEARS -------------------------------------------------------
cat("\n== 4. PERSISTENCE (needs flags files from other years) ==\n")
yrs <- list.files(out(""), pattern = "^flags_20[0-9]{2}\\.csv$")
if (length(yrs) > 1) {
  yl <- lapply(yrs, function(f) fread(out(f))[flag == 1, unique(lei)])
  names(yl) <- gsub("[^0-9]", "", yrs)
  base <- yl[[length(yl)]]
  for (i in seq_len(length(yl) - 1))
    cat(sprintf("  %s vs %s: %d of %d flagged CUs recur\n",
                names(yl)[length(yl)], names(yl)[i],
                length(intersect(base, yl[[i]])), length(base)))
} else cat("  Only one year present. Rerun the chain on prior-year raw",
           "files (settings.R paths) and place flags_<year>.csv here;\n",
           "  this section then reports institution recurrence -- the",
           "strongest validation available without exam outcomes.\n")

cat("\nDossier files: validation_power_2025.csv, validation_evalues_2025.csv\n")
cat("VALIDATION HARNESS COMPLETE.\n")
