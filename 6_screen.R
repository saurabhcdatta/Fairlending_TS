# =============================================================================
# 6_screen.R  (DEV chain)  --  THE ENHANCED SCREEN (our filters), Popick-only.
# Same statistics as production 04a_scientific_screen.R, with the stream
# machinery removed and the run unwrapped into RUN-BY-BLOCK sections:
#
#   BLOCK 0  setup + FILTERS      all thresholds in one place
#   BLOCK 1  screening universe   assets, top-N per cu_type
#   BLOCK 2  the engine           pm_tau2 / perm_p / screen_one (verbose)
#   BLOCK 3  DENIAL screen        cells_denial in the environment
#   BLOCK 4  WITHDRAWAL screen    cells_withdrawal
#   BLOCK 5  PRICING screen       cells_pricing
#   BLOCK 6  combine + diagnose   flags, tier table, plots, checks
#
# THE THREE ENHANCEMENTS (unchanged; rationale as in production):
#  1. EMPIRICAL NULL (Efron): flag CUs that are outliers RELATIVE TO PEERS
#     of the same charter type (robust median/MAD of peer z-scores), not
#     merely mirrors of the market-wide gap. Switch: null_type.
#  2. PERMUTATION GUARD: cells with n_group < perm_below must also pass an
#     exact within-CU permutation test -- only ever REMOVES fragile flags.
#  3. POSTERIOR MATERIALITY: P(true gap >= floor | data) from the
#     hierarchical model (Paule-Mandel tau2) replaces a hard floor cutoff.
# =============================================================================

library(data.table)
library(ggplot2)

cat("== 6_screen.R DEV VERSION 2026-08-05 (enhanced filters, Popick-only) ==\n")
if (!exists("out")) source("0_settings.R")   # paths: run/edit 0_settings.R first

# tiny dev-check helper: green line if OK, loud line + R warning if not
.chk <- function(ok, msg) {
  cat(sprintf("%s %s\n", if (isTRUE(ok)) "  [check OK]  " else "  !! CHECK FAILED:", msg))
  if (!isTRUE(ok)) warning("CHECK FAILED: ", msg, call. = FALSE, immediate. = TRUE)
  invisible(isTRUE(ok))
}

# ------------------------------- FILTERS (edit here) --------------------------
min_group  <- 20          # minority applications needed at the CU to test
min_white  <- 50          # white applications needed to compare against
null_type  <- "empirical" # "empirical" (peer-relative) | "theoretical" (vs 0)
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
res <- readRDS(out("residuals_2025.rds"))   # 5_models.R output (Popick)

af <- out("cu_assets_2025.csv")
if (!file.exists(af))
  stop("cu_assets_2025.csv not found -- run 00_assets.R first.", call. = FALSE)
assets  <- fread(af, colClasses = list(character = "lei"))
if (!"cu_type" %in% names(assets)) assets[, cu_type := NA_integer_]
assets[is.na(cu_type), cu_type := 0L]        # unknown type screened together
if (!is.finite(top_n)) top_n <- .Machine$integer.max  # top_n <- Inf =
                                                       # screen EVERY CU
top_uni <- assets[order(-assets_tot), head(.SD, top_n), by = cu_type]
assets[, peer := cu_type]         # peers = same charter type (cluster peers
                                  # return with the unsupervised track later)
top_lei <- top_uni$lei
cat(sprintf("Enhanced screen | null: %s | universe per cu_type:\n", null_type))
print(top_uni[, .N, by = cu_type][order(cu_type)])
cat("Smallest CU inside each top-N universe (screen coverage floor, $M):\n")
print(top_uni[, .(N = .N, smallest_M = round(min(assets_tot) / 1e6, 1)),
              by = cu_type][order(cu_type)])

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
    cat(sprintf("  [%s] empirical null by group x peer (z0 = center, s0 = scale):\n",
                screen_name))
    print(cells[, .(cells = .N, z0 = round(z0[1], 2), s0 = round(s0[1], 2)),
                by = .(group, peer)][order(group, peer)])
    if (cells[, any(s0 <= 1e-6)])
      cat("    !! a group x peer stratum has MAD ~ 0 (near-identical z-scores)",
          "-- its p-values are unstable; inspect that stratum\n")
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
  cat(sprintf("  [%s] permutation guard: %d small cells tested, %d flags removed by the guard\n",
              screen_name, nrow(small),
              cells[q <= fdr_q & !perm_ok, .N]))

  # --- 3. posterior materiality: P(true gap >= floor | data) ------------------
  fl <- floors[[screen_name]]
  cells[, tau2 := .pm_tau2(gap, se), by = .(group, peer)]
  cat(sprintf("  [%s] between-CU heterogeneity (Paule-Mandel tau, gap units):\n",
              screen_name))
  print(cells[, .(cells = .N, tau = round(sqrt(tau2[1]), 4)),
              by = .(group, peer)][order(group, peer)])
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

# ==================== BLOCK 3: DENIAL SCREEN ==================================
cat("== BLOCK 3: denial screen ==\n")
cells_denial <- screen_one(res$denial, "resid_denial", "denial")
cat(sprintf("  denial: %d cells tested | flagged: %d\n",
            nrow(cells_denial), sum(cells_denial$flag)))

# ==================== BLOCK 4: WITHDRAWAL SCREEN ==============================
cat("== BLOCK 4: withdrawal screen ==\n")
cells_withdrawal <- screen_one(res$withdrawal, "resid_withdrawn", "withdrawal")
cat(sprintf("  withdrawal: %d cells tested | flagged: %d\n",
            nrow(cells_withdrawal), sum(cells_withdrawal$flag)))

# ==================== BLOCK 5: PRICING SCREEN =================================
cat("== BLOCK 5: pricing screen ==\n")
cells_pricing <- screen_one(res$pricing, "resid_price", "pricing")
cat(sprintf("  pricing: %d cells tested | flagged: %d\n",
            nrow(cells_pricing), sum(cells_pricing$flag)))

# ============== BLOCK 6: COMBINE, DIAGNOSE, SAVE ==============================
flags <- rbind(cells_denial, cells_withdrawal, cells_pricing)
flags[, sas_anomaly := 0L]                 # schema compatibility downstream
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

fwrite(flags, out("flags_2025.csv"))
cat("Saved ->", out("flags_2025.csv"), "\n")

# diagnostic picture: where the flagged cells sit in each screen's gap
# distribution (prints to the RStudio Plots pane AND saves a PNG)
p_gaps <- ggplot(flags, aes(x = gap, fill = factor(flag))) +
  geom_histogram(bins = 60, alpha = 0.75, position = "identity") +
  facet_wrap(~ screen, scales = "free") +
  scale_fill_manual(values = c(`0` = "grey70", `1` = "#C0392B"),
                    labels = c("not flagged", "flagged"), name = NULL) +
  labs(title = "Residual gaps across all tested CU x group cells, 2025",
       subtitle = "flags should sit in the right tail -- a flag in the bulk means the filters are off",
       x = "residual gap vs white applicants at the same CU", y = "cells") +
  theme_minimal(base_size = 11)
print(p_gaps)
ggsave(out("screen_gap_density.png"), p_gaps, width = 9, height = 5, dpi = 150)

# going-off checks on the screen itself
.chk(flags[, all(p >= 0 & p <= 1 & q >= 0 & q <= 1)],
     "all p and q inside [0, 1]")
.chk(flags[, all(is.finite(se) & se > 0)], "all cell SEs finite and positive")
.chk(flags[, mean(flag)] <= 0.10,
     sprintf("flag share %.1f%% of tested cells <= 10%% (screens are selective by design)",
             100 * flags[, mean(flag)]))
.chk(flags[flag == 1, all(gap > 0)],
     "every flagged cell has a positive (adverse) raw gap")
.chk(!anyDuplicated(flags[, .(lei, group, screen)]),
     "one row per CU x group x screen")
cat("Left in the environment: flags, cells_denial/withdrawal/pricing,",
    "assets, top_uni, res\n")
