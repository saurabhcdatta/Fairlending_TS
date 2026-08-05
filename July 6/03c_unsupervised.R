# =============================================================================
# 03c_unsupervised.R -- UNSUPERVISED strengtheners. Run after 02 (+00_assets).
#
# A. CU PEER CLUSTERS (k-means, base R). Clusters credit unions on business-
#    model features: size, product mix, borrower profile, metro share. Feeds
#    04a's empirical null so "outlier vs peers" means outlier vs ACTUAL
#    peers (set peer_group <- "cluster" there). Mirrors NCUA peer-group
#    practice. Features are curated and race-blind; geography stays coarse.
#
# B. STEERING SCREEN (pricing-regime clustering). k-means on the four price
#    components (standardized within loan category) discovers the pricing
#    regimes present in the data; the regime with the highest total cost is
#    the "high-cost" tier. Among PRIME-ELIGIBLE borrowers only (credit
#    profile good enough that placement cannot be creditworthiness), we test
#    per CU: do minority borrowers land in the high-cost regime more often
#    than white borrowers at the SAME CU? Levels are the pricing screen's
#    job; PLACEMENT is this screen's job. Triage evidence -- the residual
#    screens remain primary.
#
# Outputs: peer_clusters_2025.csv, steering_gaps_2025.csv,
#          steering_flags_2025.csv (schema-compatible with 06:
#          add "steering" to flag_source there to mine its loans)
# =============================================================================

library(data.table)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
peer_k       <- 6      # CU peer clusters
regime_k     <- 3      # pricing regimes per loan category
prime_cs     <- 680    # steering universe: credit score >= this
prime_ltv    <- 90     # ...and CLTV <= this
prime_dti    <- 43     # ...and DTI <= this
min_group    <- 20     # testability at the CU (same as the screens)
min_white    <- 50
fdr_q        <- 0.05
steer_floor  <- 0.05   # material = 5pp+ excess high-cost placement share
top_n        <- 200    # per cu_type, as elsewhere
seed         <- 20250101
# ------------------------------------------------------------------------------

set.seed(seed)
dat    <- readRDS(out("analysis_2025.rds"))
assets <- fread(out("cu_assets_2025.csv"), colClasses = list(character = "lei"))
if (!"cu_type" %in% names(assets)) assets[, cu_type := NA_integer_]
assets[is.na(cu_type), cu_type := 0L]
top_uni <- assets[order(-assets_tot), head(.SD, top_n), by = cu_type]

# ---- A. CU peer clusters -------------------------------------------------------
cu_feat <- dat[lei %in% top_uni$lei, .(
  n_loans        = .N,
  share_purchase = mean(loan_purpose == 1),
  share_fha_va   = mean(loan_type %in% 2:3),
  share_jumbo    = mean(jumbo == 1, na.rm = TRUE),
  mean_cs        = mean(credit_score, na.rm = TRUE),
  mean_log_inc   = mean(log(pmax(income, 1)), na.rm = TRUE),
  metro_share    = mean(metro == 1L)), by = lei]
cu_feat <- merge(cu_feat, assets[, .(lei, name, cu_type, assets_tot)],
                 by = "lei")
cu_feat[, log_assets := log(assets_tot)]
fcols <- c("log_assets", "n_loans", "share_purchase", "share_fha_va",
           "share_jumbo", "mean_cs", "mean_log_inc", "metro_share")
X <- scale(as.matrix(cu_feat[, ..fcols]))
X[!is.finite(X)] <- 0
km <- kmeans(X, centers = min(peer_k, nrow(X) - 1L), nstart = 25)
cu_feat[, peer_cluster := km$cluster]
fwrite(cu_feat[, c("lei", "name", "cu_type", "peer_cluster", "assets_tot",
                   fcols), with = FALSE], out("peer_clusters_2025.csv"))
cat("Peer clusters (k-means on business-model features):\n")
print(cu_feat[, .(CUs = .N, med_assets_B = round(median(assets_tot)/1e9, 2),
                  purchase = round(mean(share_purchase), 2),
                  metro = round(mean(metro_share), 2)),
              by = peer_cluster][order(peer_cluster)])

# ---- B. steering: pricing regimes + placement gaps ------------------------------
pr <- dat[originated == 1 & !is.na(group) &
          !is.na(ir_spread_pmms) & !is.na(disc_pts_pct) &
          !is.na(lend_cred_pct) & !is.na(loan_cost_pct) &
          !is.na(credit_score) & credit_score >= prime_cs &
          !is.na(ltv_combined) & ltv_combined <= prime_ltv &
          (!is.na(dti) & dti <= prime_dti) &
          lei %in% top_uni$lei]
cat(sprintf("\nSteering universe (prime-eligible originations): %s loans\n",
            format(nrow(pr), big.mark = ",")))

pr[, hi_cost := {
  Z <- scale(cbind(ir_spread_pmms, disc_pts_pct, -lend_cred_pct,
                   loan_cost_pct))
  Z[!is.finite(Z)] <- 0
  if (.N > 50 * regime_k) {
    cl <- kmeans(Z, centers = regime_k, nstart = 10,
                 iter.max = 100, algorithm = "Lloyd")
    tot <- rowSums(Z)
    hi <- which.max(tapply(tot, cl$cluster, mean))   # costliest regime
    as.integer(cl$cluster == hi)
  } else rep(NA_integer_, .N)
}, by = loan_cat]
pr <- pr[!is.na(hi_cost)]
cat(sprintf("High-cost regime share overall: %.1f%%\n",
            100 * mean(pr$hi_cost)))

w <- pr[group == "white", .(p_w = mean(hi_cost), n_w = .N), by = lei]
g <- pr[group != "white", .(p_g = mean(hi_cost), n_g = .N),
        by = .(lei, cu_number, group)]
cells <- merge(g, w, by = "lei")[n_g >= min_group & n_w >= min_white]
cells <- merge(cells, assets[, .(lei, name, cu_type)], by = "lei")
cells[, gap := p_g - p_w]
cells[, se := sqrt(p_g * (1 - p_g) / n_g + p_w * (1 - p_w) / n_w)]
cells[se == 0, se := NA_real_]
cells[, p := pnorm(gap / se, lower.tail = FALSE)]
cells[, q := p.adjust(p, method = "BH"), by = cu_type]
cells[, flag := as.integer(!is.na(q) & q <= fdr_q & gap >= steer_floor)]
cells[, `:=`(screen = "steering", tier = fifelse(flag == 1, "flag", "none"),
             sas_anomaly = 0L)]
setorder(cells, -flag, q)
fwrite(cells, out("steering_gaps_2025.csv"))
fwrite(cells[, .(lei, cu_number, group, screen, n_g, gap, se, p, q,
                 flag, tier, sas_anomaly, name, cu_type)],
       out("steering_flags_2025.csv"))
cat(sprintf("Steering cells tested: %s | flagged: %d\n",
            format(nrow(cells), big.mark = ","), sum(cells$flag)))
print(cells[flag == 1, .(name, cu_type, group, n_g,
                         excess_hi_cost_share = round(gap, 3),
                         q = signif(q, 2))])

# per-loan steering outliers: prime-eligible minority loans in the high-cost
# regime at flagged cells -- for 06 (add "steering" to flag_source there)
sl <- merge(pr[hi_cost == 1 & group != "white",
               .(uli, lei, cu_number, group, loan_cat)],
            cells[flag == 1, .(lei, group)], by = c("lei", "group"))
fwrite(sl, out("steering_loans_2025.csv"))
cat(sprintf("Prime-eligible minority loans in the high-cost regime at flagged CUs: %s -> steering_loans_2025.csv\n",
            format(nrow(sl), big.mark = ",")))
