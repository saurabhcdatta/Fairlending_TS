# =============================================================================
# 06_rank_outliers.R  --  THE RANKING: credit unions ordered by the number of
#                         OUTLIER LOANS (by HMDA ULI), per loan category and
#                         per model, with every loan ID exported for the
#                         second-stage investigation.
#
# An OUTLIER LOAN is a minority-group loan, at a CU x group x screen cell the
# screen flagged, whose own residual is large in the adverse direction:
#   denial      denied although the race-blind model gave it a LOW denial
#               probability: resid_denial >= denial_loan_cut (1 - p_hat)
#   withdrawal  withdrawn despite low predicted withdrawal probability
#   pricing     rate residual >= pricing_loan_cut above the model prediction
#
# Outputs:
#   outlier_rankings_2025.csv   CU x screen x loan_cat counts + overall ranks
#   outlier_loans_2025.csv      EVERY outlier loan: ULI + CU + screen +
#                               category + group + residual + the loan's
#                               underwriting fields (stage-2 file)
# =============================================================================

library(data.table)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
stream_tag        <- "popick"        # tagged copies *_<stream>_2025.csv for 07
flag_source       <- c("v2", "sas")  # "v2" tiers, "sas" legacy, "steering"
# LOAN SELECTION -- EXCESS-CALIBRATED (the scientific rule):
#   Each flagged cell's posterior gap estimates the NUMBER of adverse
#   outcomes attributable to the disparity: excess = eb_gap x n_group
#   (e.g. 5.5pp x 5,089 black applicants ~ 280 excess denials). We export
#   exactly ceiling(excess) loans per cell -- ONE REVIEWED LOAN PER
#   STATISTICALLY ESTIMATED HARMED LOAN -- prioritized by model surprise
#   (largest residual first). The count is derived from the evidence, not a
#   threshold; it scales with institution size AND gap magnitude, and
#   shrinks where evidence is weak. Pricing: a loan qualifies only if its
#   individual excess >= the institution's own estimated systematic excess
#   (and the materiality floor) -- the loans that DRIVE the finding.
#   Cells without a posterior gap (legacy SAS / steering sources) fall back
#   to the top decile of adverse residuals.
pricing_loan_floor <- 0.10  # rate pp; same floor as the screen
fallback_share     <- 0.10  # top decile for cells lacking eb_gap
max_per_cell       <- 1000  # hard cap per cell (file-size guard)
# ------------------------------------------------------------------------------

res   <- readRDS(out("residuals_2025.rds"))
flags <- fread(out("flags_2025.csv"), colClasses = list(character = "lei"))
dat   <- readRDS(out("analysis_2025.rds"))

# cells to mine: v2 flags are CU x group x screen; SAS anomalies are CU x
# screen (all minority groups at that CU are then mined, as the SAS intended)
cells <- unique(rbind(
  if ("v2" %in% flag_source)
    flags[flag == 1, .(lei, group, screen, eb_gap, n_cell = n_g)],
  if ("sas" %in% flag_source) {
    sc <- flags[sas_anomaly == 1 & flag != 1,   # avoid double-counting v2
                .(group = c("black", "asian", "hispanic")),
                by = .(lei, screen)]
    # excess-calibrate SAS cells too: the flags table carries eb_gap / n_g
    # for EVERY tested cell -- a legacy flag with ~0 posterior gap therefore
    # exports ~0 loans (the calibration absorbs the legacy false flags)
    sc <- merge(sc, flags[, .(lei, group, screen, eb_gap, n_cell = n_g)],
                by = c("lei", "group", "screen"), all.x = TRUE)
    sc
  },
  if ("steering" %in% flag_source &&
      file.exists(out("steering_flags_2025.csv")))
    fread(out("steering_flags_2025.csv"),
          colClasses = list(character = "lei"))[flag == 1,
          .(lei, group, screen, eb_gap = NA_real_, n_cell = NA_integer_)]
), by = c("lei", "group", "screen"))
cat(sprintf("Mining %d flagged CU x group x screen cells (source: %s)\n",
            nrow(cells), paste(flag_source, collapse = " + ")))

.pick_loans <- function(rd, resid_col, screen_name) {
  r <- merge(rd, cells[screen == screen_name], by = c("lei", "group"))
  if (!nrow(r)) return(NULL)
  if (!"resid_oth" %in% names(r)) r[, resid_oth := NA_real_]
  setnames(r, resid_col, ".r")
  r <- r[.r > 0]                                  # adverse direction only
  setorder(r, lei, group, -.r)
  if (screen_name == "pricing") {
    # CUMULATIVE-MASS ATTRIBUTION: the cell's estimated total excess is
    # eb_gap x n (rate-pp mass). Export the SMALLEST set of top-residual
    # loans that jointly account for that excess (each also >= the 10bp
    # floor). A cell with ~zero posterior gap exports ~zero loans, so
    # legacy-track false flags contribute nothing at the loan level.
    r <- r[.r >= pricing_loan_floor]
    r[, idx := seq_len(.N), by = .(lei, group)]
    r[, prevcum := cumsum(.r) - .r, by = .(lei, group)]
    r[, take := fifelse(is.na(eb_gap) | is.na(n_cell),
                        as.numeric(idx <= ceiling(fallback_share * .N)),
                        as.numeric(pmax(eb_gap, 0) > 0 &
                                   prevcum < pmax(eb_gap, 0) * n_cell)),
      by = .(lei, group)]
    r <- r[take > 0 & idx <= max_per_cell]
    r[, c("idx", "prevcum", "take") := NULL]
    return(r[, .(uli, lei, cu_number, group, loan_cat, screen = screen_name,
                 resid = .r, resid_oth)])
  } else {
    # excess-calibrated count: one loan per estimated excess adverse outcome
    r[, take := pmin(
        fifelse(is.na(eb_gap), ceiling(fallback_share * .N),
                ceiling(pmax(eb_gap, 0) *
                        fifelse(is.na(n_cell), as.integer(.N), n_cell))),
        max_per_cell, .N), by = .(lei, group)]
  }
  r[, idx := seq_len(.N), by = .(lei, group)]
  r <- r[idx <= take]
  r[, .(uli, lei, cu_number, group, loan_cat, screen = screen_name,
        resid = .r, resid_oth)]
}
outliers <- rbind(
  .pick_loans(res$denial,     "resid_denial",    "denial"),
  .pick_loans(res$withdrawal, "resid_withdrawn", "withdrawal"),
  .pick_loans(res$pricing,    "resid_price",     "pricing")
)
if ("steering" %in% flag_source && file.exists(out("steering_loans_2025.csv"))) {
  st <- fread(out("steering_loans_2025.csv"),
              colClasses = list(character = c("lei", "uli")))
  if (nrow(st)) outliers <- rbind(outliers,
    st[, .(uli, lei, cu_number, group, loan_cat, screen = "steering",
           resid = NA_real_, resid_oth = NA_real_)])
}
cat(sprintf("Outlier loans identified: %s\n",
            format(nrow(outliers), big.mark = ",")))

# ---- RANKINGS: overall, PER SCREEN, and PER PRODUCT ----------------------------
# All product categories are 30-year fixed by sample design (Popick Types
# 1-10): the product dimension is purpose x program x jumbo. HELOCs and other
# open-end/ARM products are excluded from the models by the methodology.
assets <- fread(out("cu_assets_2025.csv"), colClasses = list(character = "lei"))
if (!"cu_type" %in% names(assets)) assets[, cu_type := NA_integer_]
assets[is.na(cu_type), cu_type := 0L]
if (all(assets$cu_type == 0L))
  warning("cu_type is UNKNOWN (0) for every CU -- cu_assets_2025.csv predates ",
          "the cu_type addition. DELETE it and rerun 00_assets.R, then rerun ",
          "the screen and this script, to get separate federal/state rankings.",
          call. = FALSE, immediate. = TRUE)
.nm <- function(x) merge(x, assets[, .(lei, cu_number, name, assets_tot,
                                       cu_type)], by = "lei")

# (a) overall: one row per CU, columns per screen and per product
by_cat <- dcast(outliers, lei ~ loan_cat, fun.aggregate = length,
                value.var = "uli")
by_scr <- dcast(outliers, lei ~ screen, fun.aggregate = length,
                value.var = "uli")
rank_tab <- merge(by_scr, by_cat, by = "lei", all = TRUE)
rank_tab[is.na(rank_tab)] <- 0
rank_tab[, total_outlier_loans := rowSums(.SD),
         .SDcols = intersect(c("denial", "withdrawal", "pricing"),
                             names(rank_tab))]
rank_tab <- .nm(rank_tab)
setorder(rank_tab, cu_type, -total_outlier_loans)
rank_tab[, rank := seq_len(.N), by = cu_type]      # rank WITHIN cu_type
setcolorder(rank_tab, c("cu_type", "rank", "name", "cu_number", "lei",
                        "total_outlier_loans"))
fwrite(rank_tab, out("outlier_rankings_2025.csv"))
fwrite(rank_tab, out(sprintf("outlier_rankings_%s_2025.csv", stream_tag)))

# (b) SEPARATE ranking per screen (denial / withdrawal / pricing), with the
#     product breakdown as columns; rank restarts within each screen
scr_tab <- dcast(outliers[, .N, by = .(screen, lei, loan_cat)],
                 screen + lei ~ loan_cat, value.var = "N", fill = 0)
scr_tab[, loans := rowSums(.SD), .SDcols = -(1:2)]
scr_tab <- .nm(scr_tab)
setorder(scr_tab, cu_type, screen, -loans)
scr_tab[, rank := seq_len(.N), by = .(cu_type, screen)]
setcolorder(scr_tab, c("cu_type", "screen", "rank", "name", "loans"))
fwrite(scr_tab, out("outlier_rank_by_screen_2025.csv"))

# (c) SEPARATE ranking per product, with the screen breakdown as columns;
#     rank restarts within each product
prd_tab <- dcast(outliers[, .N, by = .(loan_cat, lei, screen)],
                 loan_cat + lei ~ screen, value.var = "N", fill = 0)
prd_tab[, loans := rowSums(.SD), .SDcols = -(1:2)]
prd_tab <- .nm(prd_tab)
setorder(prd_tab, cu_type, loan_cat, -loans)
prd_tab[, rank := seq_len(.N), by = .(cu_type, loan_cat)]
setcolorder(prd_tab, c("cu_type", "loan_cat", "rank", "name", "loans"))
fwrite(prd_tab, out("outlier_rank_by_product_2025.csv"))

cat("\n==== CUs RANKED BY OUTLIER LOANS, WITHIN CU TYPE ====\n")
for (ct in sort(unique(rank_tab$cu_type)))
  {cat(sprintf("-- cu_type %d --\n", ct));
   print(rank_tab[cu_type == ct][1:min(5, .N),
                  .(rank, name, total_outlier_loans)])}
print(rank_tab[1:min(10, .N),
               .SD, .SDcols = intersect(
                 c("rank", "name", "total_outlier_loans", "denial",
                   "withdrawal", "pricing"), names(rank_tab))])
for (sc in unique(scr_tab$screen)) {
  cat(sprintf("\n==== %s: top CUs ====\n", toupper(sc)))
  print(scr_tab[screen == sc][1:min(5, .N),
                              .(rank, name, loans, assets_tot)])
}
for (pc in unique(prd_tab$loan_cat)) {
  cat(sprintf("\n==== product %s: top CUs ====\n", pc))
  print(prd_tab[loan_cat == pc][1:min(3, .N), .(rank, name, loans)])
}

# ---- charts: ONE PNG PER SCREEN, top CUs stacked by product --------------------
library(ggplot2)
for (sc in unique(outliers$screen)) {
  d <- outliers[screen == sc, .(loans = .N), by = .(lei, loan_cat)]
  d <- merge(d, assets[, .(lei, name, cu_type)], by = "lei")
  d[, name := sprintf("%s [T%d]", name, cu_type)]
  ord <- d[, .(tot = sum(loans)), by = name][order(-tot)][1:min(15, .N), name]
  p <- ggplot(d[name %in% ord],
              aes(x = factor(name, levels = rev(ord)), y = loans,
                  fill = loan_cat)) +
    geom_col() + coord_flip() +
    geom_text(aes(label = loans), position = position_stack(vjust = 0.5),
              size = 3, colour = "white") +
    labs(title = sprintf("%s: credit unions ranked by outlier loans, 2025",
                         toupper(sc)),
         subtitle = "stacked by product (all categories are 30-yr fixed by sample design)",
         x = NULL, y = "outlier loans (by ULI)", fill = "product") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "top")
  ggsave(out(sprintf("outlier_ranking_%s.png", sc)), p,
         width = 9, height = 6.5, dpi = 150)
  cat("Saved ->", out(sprintf("outlier_ranking_%s.png", sc)), "\n")
}

# ---- the stage-2 file: every outlier ULI with its underwriting profile --------
keep_fields <- intersect(
  c("uli", "credit_score", "dti", "ltv_combined", "income", "loan_amount",
    "interest_rate", "rate_spread", "disc_points", "lender_credits",
    "loan_costs", "aus", "broker", "early_bankrupt", "action_type",
    "action_date", "fips", "property_state",
    "denial_reason1", "denial_reason2", "denial_reason3"), names(dat))
loans <- merge(outliers, dat[, ..keep_fields], by = "uli", all.x = TRUE)

# WHY, per loan, in plain columns:
#   model_expected  what the race-blind model predicted for THIS loan
#                   (denial/withdrawal: probability; pricing: rate points
#                   ABOVE prediction is the resid itself)
#   cu_stated_reason the institution's OWN primary denial reason from HMDA,
#                   decoded -- lets the reviewer confront claim vs profile
#                   ("cited credit history; applicant score is 761")
loans[, stream := stream_tag]
loans[, model_expected := fifelse(screen %in% c("denial", "withdrawal"),
                                  round(1 - resid, 3), NA_real_)]
# pricing: what rate the model expected for THIS loan, and where the excess
# sits -- in the rate itself vs in fees (points - credits + costs, % of loan)
loans[screen == "pricing",
      model_expected_rate := round(interest_rate - resid, 3)]
loans[screen == "pricing",
      excess_other_costs_pct := round(resid_oth, 3)]
.dr <- c("1" = "debt-to-income ratio", "2" = "employment history",
         "3" = "credit history", "4" = "collateral",
         "5" = "insufficient cash", "6" = "unverifiable information",
         "7" = "application incomplete", "8" = "mortgage insurance denied",
         "9" = "other", "10" = "not applicable")
if ("denial_reason1" %in% names(loans))
  loans[, cu_stated_reason := unname(.dr[as.character(denial_reason1)])]
loans <- merge(loans, assets[, .(lei, name)], by = "lei", all.x = TRUE)
setorder(loans, screen, -resid)
setcolorder(loans, c("name", "lei", "screen", "loan_cat", "group", "uli",
                     "resid"))
fwrite(loans, out("outlier_loans_2025.csv"))
fwrite(loans, out(sprintf("outlier_loans_%s_2025.csv", stream_tag)))
cat(sprintf("\nStage-2 file: %s outlier loans with underwriting fields -> %s\n",
            format(nrow(loans), big.mark = ","), out("outlier_loans_2025.csv")))
cat("Per-screen breakdown of exported loan IDs:\n")
print(loans[, .(loans = .N, cus = uniqueN(lei)), by = .(screen, loan_cat)])
