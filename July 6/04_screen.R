# =============================================================================
# 04_screen.R  --  CU-level outlier screen with the v2 FILTER RULES.
#
# Every threshold sits in the FILTERS block below -- change them there.
# The rule set is the reviewed v2 from the rule lab (power-study validated:
# detection identical to the old rules, false flags driven to ~0):
#
#   TESTABLE   n_group >= 20 minority AND n_white >= 50 white apps at the CU
#   SIGNIFICANT one-sided Welch p (adverse direction), BH-corrected per screen
#   MATERIAL   the EB-SHRUNK gap must clear a practical floor
#              (denial 2pp | withdrawn 3pp | pricing 10bp) -- empirical-Bayes
#              shrinkage pulls noisy small-cell gaps toward zero, so a tiny-
#              but-"significant" gap cannot flag
#   TIERS      high  : q <= 0.01 AND eb_gap >= 2 x floor
#              flag  : q <= 0.05 AND eb_gap >= floor
#              watch : q <= 0.10 AND material, or significant but sub-material
#   FLAG = tier high or flag. Screens: denial, withdrawal, pricing.
#
# Universe: the TOP-N credit unions by assets (00_assets.R).
# Output: flags_2025.csv -- every tested cell with gap, eb_gap, q, tier, name.
# =============================================================================

library(data.table)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
min_group  <- 20      # minority applications needed at the CU to test
min_white  <- 50      # white applications needed to compare against
fdr_q      <- 0.05    # BH false-discovery rate for a flag
fdr_high   <- 0.01    # ...for the high tier
fdr_watch  <- 0.10    # ...for the watch tier
floors     <- c(denial = 0.02,      # 2pp  excess denial (probability units)
                withdrawal = 0.03,  # 3pp  excess withdrawal
                pricing = 0.10)     # 10bp excess rate (pp of rate spread)
high_mult  <- 2       # high tier needs eb_gap >= high_mult x floor
top_n      <- 200     # screen the largest N CUs by assets PER cu_type
                      # (1 = federal insured, 2 = state insured)

# --- LEGACY SAS TRACK (thresholds exactly as in the production SAS code) ------
sas_groups    <- c("black", "asian", "hispanic")  # groups in the SAS rules
sas_min_rec   <- 100    # min records at the CU (pricing: MINORITY originated)
sas_min_grps  <- 1      # minority groups that must show an adverse gap
sas_den_odds  <- 1.1    # denial: exp(gap) odds heuristic threshold
sas_wd_pct    <- 10     # withdrawal: worst percentile (1 = worst across CUs)
sas_int_dif   <- 0.10   # pricing: rate gap threshold (10bp, rate pp)
sas_ocost_dif <- 2.0    # pricing: other-cost composite gap (% of loan; the
                        # SAS 0.02 was in fraction-of-loan units)
sas_cats      <- c("conv_purchase", "conv_refi_nocashout", "conv_refi_cashout")
                        # the SAS models pool Types 1, 4, 6
# ------------------------------------------------------------------------------

res <- readRDS(out("residuals_2025.rds"))

af <- out("cu_assets_2025.csv")
if (!file.exists(af))
  stop("cu_assets_2025.csv not found -- run 00_assets.R first.", call. = FALSE)
assets  <- fread(af, colClasses = list(character = "lei"))
if (!"cu_type" %in% names(assets)) assets[, cu_type := NA_integer_]
assets[is.na(cu_type), cu_type := 0L]
top_uni <- assets[order(-assets_tot), head(.SD, top_n), by = cu_type]
top_lei <- top_uni$lei
cat("Screening universe per cu_type (1 = federal, 2 = state):\n")
print(top_uni[, .N, by = cu_type][order(cu_type)])

# Empirical-Bayes shrinkage across CUs within screen x group:
#   eb = gap * tau2 / (tau2 + se^2),
#   tau2 = max(0, var(gaps) - median(se^2))   (method of moments)
.eb_shrink <- function(gap, se) {
  ok <- is.finite(gap) & is.finite(se) & se > 0
  if (sum(ok) < 3L) return(gap)               # too few cells to estimate tau2
  tau2 <- max(0, var(gap[ok]) - median(se[ok]^2))
  fifelse(ok, gap * tau2 / (tau2 + se^2), NA_real_)
}

screen_one <- function(d, resid_col, screen_name) {
  d <- d[!is.na(get(resid_col)) & lei %in% top_lei]
  w <- d[group == "white", .(mu_w = mean(get(resid_col)),
                             v_w = var(get(resid_col)), n_w = .N), by = lei]
  g <- d[group != "white", .(mu_g = mean(get(resid_col)),
                             v_g = var(get(resid_col)), n_g = .N),
         by = .(lei, cu_number, group)]
  cells <- merge(g, w, by = "lei")[n_g >= min_group & n_w >= min_white]
  cells <- merge(cells, assets[, .(lei, cu_type)], by = "lei")
  cells[, `:=`(gap = mu_g - mu_w, se = sqrt(v_g / n_g + v_w / n_w))]
  cells[, p := pnorm(gap / se, lower.tail = FALSE)]        # one-sided adverse
  cells[, q := p.adjust(p, method = "BH"), by = cu_type]   # within type
  cells[, eb_gap := .eb_shrink(gap, se), by = .(group, cu_type)]
  fl <- floors[[screen_name]]
  cells[, material := !is.na(eb_gap) & eb_gap >= fl]
  cells[, tier := fcase(
      q <= fdr_high  & !is.na(eb_gap) & eb_gap >= high_mult * fl, "high",
      q <= fdr_q     & material,                                  "flag",
      q <= fdr_watch & material,                                  "watch",
      q <= fdr_q     & !material,                                 "watch",
      default = "none")]
  cells[, flag := as.integer(tier %in% c("high", "flag"))]
  cells[, screen := screen_name]
  cells[order(-flag, q)]
}

flags <- rbind(
  screen_one(res$denial,     "resid_denial",    "denial"),
  screen_one(res$withdrawal, "resid_withdrawn", "withdrawal"),
  screen_one(res$pricing,    "resid_price",     "pricing")
)
flags <- merge(flags, assets[, .(lei, name, assets_tot)], by = "lei",
               all.x = TRUE)
setorder(flags, -flag, q)

cat(sprintf("Tested %s cells | tiers: high %d, flag %d, watch %d, none %d\n",
            format(nrow(flags), big.mark = ","),
            sum(flags$tier == "high"), sum(flags$tier == "flag"),
            sum(flags$tier == "watch"), sum(flags$tier == "none")))
print(flags[flag == 1, .(tier, screen, name, group, n_g,
                         gap = round(gap, 4), eb_gap = round(eb_gap, 4),
                         q = signif(q, 2))])

# ======================= LEGACY SAS TRACK ======================================
# Faithful port of the production SAS anomaly rules, run on the pooled
# Type-1/4/6 residuals as the SAS did. SAS quirks preserved deliberately:
# missing group means -> 0; simple mean deltas (no significance test);
# denial uses exp(delta) as an odds heuristic; withdrawal uses the worst
# cross-CU percentile of positive deltas; pricing is the SIGN GAUNTLET on
# the rate gap and the other-cost composite (dp - lc + costs).
.sas_deltas <- function(d, resid_col, type_leis) {
  d <- d[lei %in% type_leis & loan_cat %in% sas_cats]
  M <- dcast(d[group %in% c("white", sas_groups),
               .(mu = mean(.SD[[1L]])), by = .(lei, group),
               .SDcols = resid_col],
             lei ~ group, value.var = "mu")
  for (g in c("white", sas_groups)) {
    if (!g %in% names(M)) M[, (g) := 0]
    M[is.na(get(g)), (g) := 0]                       # SAS: missing -> 0
  }
  for (g in sas_groups) M[, paste0("delta_", g) := get(g) - white]
  recs <- d[, .(records = .N), by = lei]
  merge(M, recs, by = "lei")
}

run_sas_type <- function(ct) {
type_leis <- top_uni[cu_type == ct, lei]
sas_denial <- .sas_deltas(res$denial, "resid_denial", type_leis)
for (g in sas_groups)
  sas_denial[, paste0("odds_", g) :=
               fifelse(get(paste0("delta_", g)) > 0,
                       exp(get(paste0("delta_", g))), NA_real_)]
sas_denial[, odds_minor := pmax(odds_black, odds_asian, odds_hispanic,
                                na.rm = TRUE)]
sas_denial[is.infinite(odds_minor), odds_minor := NA_real_]
sas_denial[, minor_grps := (delta_black > 0) + (delta_asian > 0) +
                           (delta_hispanic > 0)]
sas_denial[, anomaly := as.integer(records >= sas_min_rec &
             minor_grps >= sas_min_grps &
             !is.na(odds_minor) & odds_minor >= sas_den_odds)]
sas_denial[, screen := "denial"]

sas_wd <- .sas_deltas(res$withdrawal, "resid_withdrawn", type_leis)
for (g in sas_groups) {                              # cross-CU percentiles,
  dg <- sas_wd[[paste0("delta_", g)]]                # reversed: 1 = worst
  pct <- 100 - (frank(dg, ties.method = "min") - 1L) %/%
               max(1L, ceiling(nrow(sas_wd) / 100))
  sas_wd[, paste0("p_", g) := fifelse(dg > 0, pmin(pmax(pct, 1L), 100L),
                                      NA_integer_)]
}
sas_wd[, p_avg := pmin(p_black, p_asian, p_hispanic, na.rm = TRUE)]
sas_wd[is.infinite(p_avg), p_avg := NA_real_]
sas_wd[, minor_grps := (delta_black > 0) + (delta_asian > 0) +
                       (delta_hispanic > 0)]
sas_wd[, anomaly := as.integer(records >= sas_min_rec &
         minor_grps >= sas_min_grps & !is.na(p_avg) & p_avg <= sas_wd_pct)]
sas_wd[, screen := "withdrawal"]

# pricing: deltas on rate residual and on the other-cost composite
.pr <- res$pricing[lei %in% type_leis & loan_cat %in% sas_cats]
int <- .sas_deltas(.pr, "resid_price", type_leis)
oth <- .sas_deltas(.pr[!is.na(resid_oth)], "resid_oth", type_leis)
setnames(oth, paste0("delta_", sas_groups), paste0("doth_", sas_groups))
sas_pr <- merge(int, oth[, c("lei", paste0("doth_", sas_groups)), with = FALSE],
                by = "lei", all.x = TRUE)
minority_recs <- .pr[group %in% sas_groups, .(min_recs = .N), by = lei]
sas_pr <- merge(sas_pr, minority_recs, by = "lei", all.x = TRUE)
for (g in sas_groups) {
  di <- sas_pr[[paste0("delta_", g)]]; do <- sas_pr[[paste0("doth_", g)]]
  di[is.na(di)] <- 0; do[is.na(do)] <- 0
  sas_pr[, paste0("hit_int_", g) := as.integer(di > sas_int_dif & do >= 0)]
  sas_pr[, paste0("hit_oth_", g) := as.integer(do > sas_ocost_dif & di >= 0)]
}
sas_pr[, `:=`(col_t = hit_int_black + hit_int_asian + hit_int_hispanic,
              col_u = hit_oth_black + hit_oth_asian + hit_oth_hispanic)]
sas_pr[, anomaly := as.integer(!is.na(min_recs) & min_recs > sas_min_rec &
                               (col_t >= sas_min_grps | col_u >= sas_min_grps))]
sas_pr[, `:=`(screen = "pricing", records = min_recs)]

rbind(
  sas_denial[, .(lei, screen, records, minor_grps, sas_anomaly = anomaly)],
  sas_wd[,     .(lei, screen, records, minor_grps, sas_anomaly = anomaly)],
  sas_pr[,     .(lei, screen, records, minor_grps = col_t + col_u,
                 sas_anomaly = anomaly)])[, cu_type := ct][]
}
sas_flags <- rbindlist(lapply(sort(unique(top_uni$cu_type)), run_sas_type))
flags <- merge(flags, sas_flags[, .(lei, screen, sas_anomaly)],
               by = c("lei", "screen"), all.x = TRUE)
cat("Flags by cu_type (1 = federal, 2 = state):\n")
print(flags[, .(cells = .N, flagged = sum(flag),
                sas = sum(sas_anomaly, na.rm = TRUE)), by = cu_type])
sas_out <- merge(sas_flags, assets[, .(lei, name, assets_tot)], by = "lei",
                 all.x = TRUE)[order(-sas_anomaly, -assets_tot)]
fwrite(sas_out, out("legacy_sas_flags_2025.csv"))
cat(sprintf("Legacy SAS track: %d anomalies (denial %d, withdrawal %d, pricing %d)\n",
            sum(sas_out$sas_anomaly, na.rm = TRUE),
            sas_out[screen == "denial", sum(sas_anomaly, na.rm = TRUE)],
            sas_out[screen == "withdrawal", sum(sas_anomaly, na.rm = TRUE)],
            sas_out[screen == "pricing", sum(sas_anomaly, na.rm = TRUE)]))

fwrite(flags, out("flags_2025.csv"))
cat("Saved ->", out("flags_2025.csv"), "and legacy_sas_flags_2025.csv\n")
