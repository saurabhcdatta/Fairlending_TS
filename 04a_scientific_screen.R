# =============================================================================
# 04a_scientific_screen.R -- ALTERNATIVE to 04_screen.R: the best statistically
#                            defensible screen. NO legacy SAS rules.
#
# Run order:  03_models.R -> THIS FILE -> 05_report.R -> 06_rank_outliers.R
# (writes flags_2025.csv in the schema 05/06 consume; set flag_source <- "v2"
#  in 06 when using this path)
#
# THREE UPGRADES over the basic Welch+BH+shrinkage screen, and why:
#
# 1. EMPIRICAL NULL (Efron 2004) -- what are we testing against?
#    With race-blind models every CU inherits part of the MARKET-WIDE
#    residual gap, so testing "gap = 0" flags big CUs that merely mirror the
#    market. The empirical null estimates the null center/scale from the
#    peer distribution of z-scores (robust median/MAD -- most CUs are clean)
#    and flags CUs that are OUTLIERS RELATIVE TO PEERS. It also self-corrects
#    for systematic model miscalibration. Because absolute-vs-peer-relative
#    is a POLICY choice, it is a switch below:
#      null_type = "empirical"    peer-relative outliers (default)
#      null_type = "theoretical"  any disparity vs zero
#
# 2. PERMUTATION GUARD for small cells -- denial residuals are bounded and
#    bimodal; a normal approximation on 20 loans is fragile. Cells with
#    n_group < perm_below must ALSO pass an exact permutation test (group
#    labels shuffled within the CU, B draws) to flag. Distribution-free,
#    exact, and it only ever REMOVES fragile flags -- never adds.
#
# 3. POSTERIOR MATERIALITY -- instead of point-estimate >= floor, fit the
#    standard hierarchical (meta-analytic) model with Paule-Mandel tau2 and
#    report P(true gap >= floor | data): one coherent, graded answer to
#    "how likely is this CU's TRUE disparity to be materially large?".
#    Small noisy cells are discounted automatically; strong mid-size
#    evidence is not lost to a hard cutoff.
#
# Tiers (editable):  high  q <= 0.01 and P_material >= 0.90
#                    flag  q <= 0.05 and P_material >= 0.50
#                    watch q <= 0.10 and P_material >= 0.50,
#                          or q <= 0.05 but P_material < 0.50
# =============================================================================

library(data.table)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
min_group  <- 20          # minority applications needed at the CU to test
min_white  <- 50          # white applications needed to compare against
if (!exists("stream_tag"))               # wrappers may pre-set this
stream_tag <- "popick"    # which residual stream produced the input:
                          # "popick" (03a) | "ml" (03b). A tagged copy
                          # flags_<stream>_2025.csv is written for 07.
null_type  <- "empirical" # "empirical" (peer-relative) | "theoretical" (vs 0)
peer_group <- "cu_type"   # "cu_type" | "cluster" (business-model peers from
                          # 03c_unsupervised.R; falls back to cu_type if the
                          # peer_clusters file is absent)
perm_below <- 100         # permutation guard for cells with n_group < this
perm_B     <- 2000        # permutation draws
fdr_q      <- 0.05        # BH false-discovery rate for a flag
fdr_high   <- 0.01        # ...for the high tier
fdr_watch  <- 0.10        # ...for the watch tier
floors     <- c(denial = 0.02, withdrawal = 0.03, pricing = 0.10)
p_mat_high <- 0.90        # posterior P(gap >= floor) needed for high
p_mat_flag <- 0.50        # ...for flag / watch materiality
top_n      <- 200         # screen the largest N CUs by assets PER cu_type
                          # (1 = federal insured, 2 = state insured;
                          #  screening, peer nulls, shrinkage, BH and tiers
                          #  are all computed WITHIN type)
seed       <- 20250101
# ------------------------------------------------------------------------------

set.seed(seed)
res_file <- if (stream_tag == "ml" &&
                file.exists(out("residuals_ml_2025.rds"))) {
  out("residuals_ml_2025.rds")
} else if (stream_tag == "popick" &&
           file.exists(out("residuals_econ_2025.rds"))) {
  out("residuals_econ_2025.rds")
} else out("residuals_2025.rds")
cat(sprintf("[stream %s] screening residuals: %s\n", stream_tag,
            basename(res_file)))
res <- readRDS(res_file)

af <- out("cu_assets_2025.csv")
if (!file.exists(af))
  stop("cu_assets_2025.csv not found -- run 00_assets.R first.", call. = FALSE)
assets  <- fread(af, colClasses = list(character = "lei"))
if (!"cu_type" %in% names(assets)) assets[, cu_type := NA_integer_]
assets[is.na(cu_type), cu_type := 0L]        # unknown type screened together
top_uni <- assets[order(-assets_tot), head(.SD, top_n), by = cu_type]
assets[, peer := cu_type]
if (peer_group == "cluster" && file.exists(out("peer_clusters_2025.csv"))) {
  pc <- fread(out("peer_clusters_2025.csv"),
              colClasses = list(character = "lei"))
  assets[pc, on = "lei", peer := i.peer_cluster]
  cat("[04a] peers = business-model clusters (03c)\n")
} else if (peer_group == "cluster")
  cat("[04a] peer_clusters_2025.csv not found -- peers fall back to cu_type\n")
top_lei <- top_uni$lei
cat(sprintf("Scientific screen (04a) | null: %s | universe per cu_type:\n",
            null_type))
print(top_uni[, .N, by = cu_type][order(cu_type)])

# Paule-Mandel tau2 (meta-analytic between-CU heterogeneity), per group
.pm_tau2 <- function(g, se) {
  ok <- is.finite(g) & is.finite(se) & se > 0
  g <- g[ok]; se <- se[ok]; k <- length(g)
  if (k < 3L) return(0)
  tau2 <- max(0, var(g) - median(se^2))          # start at MoM
  for (i in 1:25) {
    w  <- 1 / (se^2 + tau2)
    mu <- sum(w * g) / sum(w)
    Q  <- sum(w * (g - mu)^2)
    if (!is.finite(Q)) break
    step <- (Q - (k - 1)) / sum(w^2 * (g - mu)^2)
    tau2 <- max(0, tau2 + step)
    if (abs(step) < 1e-10) break
  }
  tau2
}

# exact permutation p (one-sided): shuffle group/white labels within the CU
.perm_p <- function(rg, rw, B) {
  obs <- mean(rg) - mean(rw)
  pool <- c(rg, rw); ng <- length(rg)
  hits <- 0L
  for (b in seq_len(B)) {
    idx <- sample.int(length(pool), ng)
    if (mean(pool[idx]) - mean(pool[-idx]) >= obs) hits <- hits + 1L
  }
  (1 + hits) / (B + 1)
}

screen_one <- function(d, resid_col, screen_name) {
  d <- d[!is.na(get(resid_col)) & lei %in% top_lei]
  setnames(d, resid_col, ".r")
  w <- d[group == "white", .(mu_w = mean(.r), v_w = var(.r), n_w = .N),
         by = lei]
  g <- d[group != "white", .(mu_g = mean(.r), v_g = var(.r), n_g = .N),
         by = .(lei, cu_number, group)]
  cells <- merge(g, w, by = "lei")[n_g >= min_group & n_w >= min_white]
  cells <- merge(cells, assets[, .(lei, cu_type, peer)], by = "lei")
  cells[, `:=`(gap = mu_g - mu_w, se = sqrt(v_g / n_g + v_w / n_w))]
  cells[, z := gap / se]

  # --- 1. empirical (peer-relative) or theoretical null: peers = SAME TYPE ----
  if (null_type == "empirical") {
    cells[, `:=`(z0 = median(z), s0 = pmax(mad(z), 1e-6)),
          by = .(group, peer)]
  } else cells[, `:=`(z0 = 0, s0 = 1)]
  cells[, p := pnorm((z - z0) / s0, lower.tail = FALSE)]   # one-sided adverse
  cells[, q := p.adjust(p, method = "BH"), by = cu_type]   # BH within type

  # --- 2. permutation guard for small cells -----------------------------------
  cells[, p_perm := NA_real_]
  small <- cells[q <= fdr_watch & n_g < perm_below]
  if (nrow(small)) {
    for (i in seq_len(nrow(small))) {
      ce <- small[i]
      rg <- d[lei == ce$lei & group == ce$group, .r]
      rw <- d[lei == ce$lei & group == "white",  .r]
      small[i, p_perm := .perm_p(rg, rw, perm_B)]
    }
    cells[small, on = .(lei, group), p_perm := i.p_perm]
  }
  cells[, perm_ok := is.na(p_perm) | p_perm <= 0.05]

  # --- 3. posterior materiality: P(true gap >= floor | data) ------------------
  fl <- floors[[screen_name]]
  cells[, tau2 := .pm_tau2(gap, se), by = .(group, peer)]
  cells[, `:=`(B_i = se^2 / (se^2 + tau2))]
  cells[, mu_grp := {w <- 1/(se^2 + tau2); sum(w * gap)/sum(w)},
        by = .(group, peer)]
  cells[, post_m := B_i * mu_grp + (1 - B_i) * gap]
  cells[, post_v := se^2 * tau2 / (se^2 + tau2)]
  cells[, p_material := fifelse(post_v > 0,
          pnorm((fl - post_m) / sqrt(post_v), lower.tail = FALSE),
          as.numeric(post_m >= fl))]

  cells[, tier := fcase(
      q <= fdr_high  & p_material >= p_mat_high & perm_ok, "high",
      q <= fdr_q     & p_material >= p_mat_flag & perm_ok, "flag",
      q <= fdr_watch & p_material >= p_mat_flag,           "watch",
      q <= fdr_q     & p_material <  p_mat_flag,           "watch",
      default = "none")]
  cells[, flag := as.integer(tier %in% c("high", "flag"))]
  cells[, `:=`(screen = screen_name, eb_gap = post_m)]
  cells[order(-flag, q)]
}

flags <- rbind(
  screen_one(res$denial,     "resid_denial",    "denial"),
  screen_one(res$withdrawal, "resid_withdrawn", "withdrawal"),
  screen_one(res$pricing,    "resid_price",     "pricing")
)
flags[, sas_anomaly := 0L]                 # schema compatibility with 04/06
flags <- merge(flags, assets[, .(lei, name, assets_tot)], by = "lei",
               all.x = TRUE)
setorder(flags, -flag, q)

cat("Tier counts BY CU TYPE (1 = federal, 2 = state):\n")
print(dcast(flags[, .N, by = .(cu_type, tier)], cu_type ~ tier,
            value.var = "N", fill = 0))
print(flags[flag == 1, .(cu_type, tier, screen, name, group, n_g,
                         gap = round(gap, 4), post_gap = round(eb_gap, 4),
                         P_mat = round(p_material, 2), q = signif(q, 2),
                         p_perm = signif(p_perm, 2))])

flags[, stream := stream_tag]
fwrite(flags, out("flags_2025.csv"))
fwrite(flags, out(sprintf("flags_%s_2025.csv", stream_tag)))
cat("Saved ->", out("flags_2025.csv"), "+ tagged copy (stream:",
    stream_tag, ") for 07\n")
cat("NOTE: run 06_rank_outliers.R with  flag_source <- \"v2\"\n")
