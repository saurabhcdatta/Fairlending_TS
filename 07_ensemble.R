# =============================================================================
# 07_ensemble.R -- THE ROBUST LIST: convergence across the three streams.
#
#   popick    econometric residuals (03a) -> 04a -> 06   [tagged "popick"]
#   ml        cross-fitted GBM residuals (03b) -> 04a -> 06  [tagged "ml"]
#   steering  unsupervised placement screen (03c)         [its own flags/loans]
#
# Workflow: run 03a -> 04a(stream_tag="popick") -> 06(stream_tag="popick");
# then 03b (make_default=TRUE) -> rerun 04a/06 with stream_tag="ml";
# 03c any time after 02. THIS script ensembles whatever streams it finds.
#
# PRINCIPLES (documented for review):
#   * CONVERGENCE = PRIORITY, NOT UNANIMITY. Cells/loans flagged by >= 2
#     streams are the "robust" tier -- strongest field cases (finding
#     survives a change of methodology). Single-stream findings remain in
#     the queue: steering detects PLACEMENT patterns that level-screens
#     structurally cannot, so requiring unanimity would blind the ensemble.
#   * Streams share the same data and curated features; agreement means
#     robustness to modeling choices, not independent replication.
#
# Outputs: ensemble_flags_2025.csv     cell-level, with streams + n_streams
#          ensemble_loans_2025.csv     loan-level union, per-stream columns
#          ensemble_rankings_2025.csv  CUs ranked by ROBUST loans, per cu_type
# =============================================================================

library(data.table)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
robust_min_streams <- 2   # streams required for the "robust" tier
# ------------------------------------------------------------------------------

.read_if <- function(f, ...) if (file.exists(out(f))) fread(out(f), ...) else NULL

# ---- cell-level ensemble -------------------------------------------------------
cell_src <- list()
for (st in c("popick", "ml")) {
  fl <- .read_if(sprintf("flags_%s_2025.csv", st),
                 colClasses = list(character = "lei"))
  if (!is.null(fl))
    cell_src[[st]] <- fl[flag == 1, .(lei, group, screen, stream = st,
                                      gap, q, tier)]
}
sg <- .read_if("steering_flags_2025.csv", colClasses = list(character = "lei"))
if (!is.null(sg))
  cell_src[["steering"]] <- sg[flag == 1, .(lei, group, screen = "steering",
                                            stream = "steering", gap, q,
                                            tier = "flag")]
if (!length(cell_src))
  stop("No stream outputs found -- run 04a/06 (tagged) and/or 03c first.",
       call. = FALSE)
cells <- rbindlist(cell_src, use.names = TRUE)
cat("Streams found:", paste(names(cell_src), collapse = ", "), "\n")

# a cell converges when >=2 streams flag the SAME lei x group (any screen --
# a level finding and a placement finding about the same borrowers at the
# same institution corroborate each other)
cell_ens <- cells[, .(screens = paste(sort(unique(screen)), collapse = "+"),
                      streams = paste(sort(unique(stream)), collapse = "+"),
                      n_streams = uniqueN(stream),
                      best_q = min(q, na.rm = TRUE)),
                  by = .(lei, group)]
cell_ens[, robust := as.integer(n_streams >= robust_min_streams)]

assets <- fread(out("cu_assets_2025.csv"), colClasses = list(character = "lei"))
if (!"cu_type" %in% names(assets)) assets[, cu_type := 0L]
assets[is.na(cu_type), cu_type := 0L]
cell_ens <- merge(cell_ens, assets[, .(lei, name, cu_type, assets_tot)],
                  by = "lei", all.x = TRUE)
setorder(cell_ens, -robust, -n_streams, best_q)
fwrite(cell_ens, out("ensemble_flags_2025.csv"))
cat(sprintf("Cells: %d | robust (>=%d streams): %d\n",
            nrow(cell_ens), robust_min_streams, sum(cell_ens$robust)))
print(cell_ens[robust == 1][1:min(10, sum(robust)),
               .(name, cu_type, group, screens, streams, best_q = signif(best_q, 2))])

# ---- loan-level ensemble --------------------------------------------------------
loan_src <- list()
for (st in c("popick", "ml")) {
  ol <- .read_if(sprintf("outlier_loans_%s_2025.csv", st),
                 colClasses = list(character = c("lei", "uli")))
  if (!is.null(ol)) loan_src[[st]] <- ol[, .(uli, lei, cu_number, group,
                                             loan_cat, screen, stream = st)]
}
sl <- .read_if("steering_loans_2025.csv",
               colClasses = list(character = c("lei", "uli")))
if (!is.null(sl))
  loan_src[["steering"]] <- sl[, .(uli, lei, cu_number, group, loan_cat,
                                   screen = "steering", stream = "steering")]
loans <- rbindlist(loan_src, use.names = TRUE, fill = TRUE)
loan_ens <- loans[, .(screens = paste(sort(unique(screen)), collapse = "+"),
                      streams = paste(sort(unique(stream)), collapse = "+"),
                      n_streams = uniqueN(stream)),
                  by = .(uli, lei, cu_number, group, loan_cat)]
loan_ens[, robust := as.integer(n_streams >= robust_min_streams)]
loan_ens <- merge(loan_ens, assets[, .(lei, name, cu_type)], by = "lei",
                  all.x = TRUE)
setorder(loan_ens, -robust, -n_streams)
fwrite(loan_ens, out("ensemble_loans_2025.csv"))
cat(sprintf("Loans: %s unique ULIs | robust: %s\n",
            format(nrow(loan_ens), big.mark = ","),
            format(sum(loan_ens$robust), big.mark = ",")))

# ---- the robust ranking, within cu_type -----------------------------------------
rk <- loan_ens[, .(robust_loans = sum(robust), total_loans = .N),
               by = .(lei, name, cu_type)]
setorder(rk, cu_type, -robust_loans, -total_loans)
rk[, rank := seq_len(.N), by = cu_type]
setcolorder(rk, c("cu_type", "rank", "name", "robust_loans", "total_loans"))
fwrite(rk, out("ensemble_rankings_2025.csv"))
cat("\n==== ENSEMBLE: CUs RANKED BY ROBUST (MULTI-STREAM) LOANS ====\n")
for (ct in sort(unique(rk$cu_type))) {
  cat(sprintf("-- cu_type %d --\n", ct))
  print(rk[cu_type == ct][1:min(5, .N),
           .(rank, name, robust_loans, total_loans)])
}
# ---- promote the ensemble to a FIRST-CLASS STREAM for the 06 workbook ---------
# Rich loan rows (underwriting, reasons, rebuttal, comparator) come from the
# source streams -- Popick preferred as model of record -- plus the ensemble
# columns. flags_ensemble is the deduped union of stream flags. The next
# 06a/06b run then adds Ensemble Summary + per-screen tabs automatically.
rich <- list()
for (st in c("popick", "ml")) {
  f <- out(sprintf("outlier_loans_%s_2025.csv", st))
  if (file.exists(f)) {
    z <- fread(f, colClasses = list(character = c("lei", "uli")))
    if ("what_happened" %in% names(z)) rich[[st]] <- z[, src_pref := 
                                          fifelse(st == "popick", 1L, 2L)]
  }
}
if (length(rich)) {
  rl <- rbindlist(rich, use.names = TRUE, fill = TRUE)
  setorder(rl, src_pref)
  rl <- unique(rl, by = "uli")
  ens_loans <- merge(rl,
                     loan_ens[, .(uli, streams, n_streams, robust)],
                     by = "uli")
  ens_loans[, src_pref := NULL]
  ens_loans[, stream := "ensemble"]
  setorder(ens_loans, -robust, -n_streams, screen)
  fwrite(ens_loans, out("outlier_loans_ensemble_2025.csv"))
  ff <- list()
  for (st in c("popick", "ml")) {
    f <- out(sprintf("flags_%s_2025.csv", st))
    if (file.exists(f)) ff[[st]] <- fread(f,
                          colClasses = list(character = "lei"))[,
                          src_pref := fifelse(st == "popick", 1L, 2L)]
  }
  if (length(ff)) {
    fu <- rbindlist(ff, use.names = TRUE, fill = TRUE)
    setorder(fu, src_pref, q)
    fu <- unique(fu, by = c("lei", "group", "screen"))
    fu[, src_pref := NULL]
    fwrite(fu, out("flags_ensemble_2025.csv"))
  }
  cat(sprintf("Ensemble promoted to stream: %s rich loans (%s robust) ->\n  outlier_loans_ensemble_2025.csv + flags_ensemble_2025.csv\n  RERUN 06a OR 06b to add Ensemble tabs to the workbook.\n",
              format(nrow(ens_loans), big.mark = ","),
              format(sum(ens_loans$robust), big.mark = ",")))
}

cat("\nRobust tier = flagged by >=", robust_min_streams, "streams.",
    "Single-stream findings remain in ensemble_loans (robust = 0) --",
    "steering catches placement patterns level-screens cannot.\n")
