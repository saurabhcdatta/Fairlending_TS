# =============================================================================
# 5_models.R  (DEV chain)  --  RACE-BLIND Popick models + residual gaps,
#                              unwrapped into RUN-BY-BLOCK sections.
#
# Same estimator and specification as production 03a_popick_models.R; the
# pipeline is no longer wrapped in run_* functions, so you can select a
# block, run it, and inspect everything it leaves in the environment:
#
#   BLOCK 0  setup             dat, controls, helpers
#   BLOCK 1  DENIAL logits     mods_denial[[cat]], den_gaps, den_resid
#   BLOCK 2  WITHDRAWAL logits mods_wdr[[cat]],   wdr_gaps, wdr_resid
#   BLOCK 3  PRICING (4 comps) mods_price[[cat|comp]], pri_gaps, pri_resid
#   BLOCK 4  tables + saves    gap CSVs, dollar diffs, residuals_2025.rds
#   BLOCK 5  regression PDF    regression_output_2025.pdf
#
# DESIGN (unchanged): race/ethnicity is NOT a regressor in ANY model; the
# fair-lending evidence is the mean residual gap vs white (Welch test).
# SEs clustered by LEI; state x MSA + year-month FE.
# =============================================================================

# ========================= BLOCK 0: SETUP =====================================
library(data.table)
library(fixest)
library(ggplot2)

cat("== 5_models.R DEV VERSION 2026-08-05 (block-by-block Popick models) ==\n")
if (!exists("out")) source("0_settings.R")   # paths: run/edit 0_settings.R first

# tiny dev-check helper: green line if OK, loud line + R warning if not
.chk <- function(ok, msg) {
  cat(sprintf("%s %s\n", if (isTRUE(ok)) "  [check OK]  " else "  !! CHECK FAILED:", msg))
  if (!isTRUE(ok)) warning("CHECK FAILED: ", msg, call. = FALSE, immediate. = TRUE)
  invisible(isTRUE(ok))
}

dat <- readRDS(out("analysis_2025.rds"))

FE     <- "state_msa + year_month"
SIG    <- 0.01
MIN_N  <- 1000L
GROUPS <- c("black", "hispanic", "asian", "aian", "nhpi")

ctrl_denial  <- c("cs_bin", "dti_bin", "ltv_bin", "early_bankrupt", "aus",
                  "broker", "other_lien", "offers_other_types",
                  "lender_orig_bin")
ctrl_wdr     <- c("early_bankrupt", "aus", "broker", "other_lien",
                  "offers_other_types", "lender_orig_bin")   # no-credit spec
ctrl_pricing <- c("cs_bin", "ltv_bin", "income_bin", "early_bankrupt",
                  "broker", "other_lien", "offers_other_types",
                  "lender_orig_bin")

# drop factor controls with < 2 levels in this estimation sample
.active <- function(d, ctrls) {
  keep <- vapply(ctrls, function(v) !is.factor(d[[v]]) ||
                   length(unique(droplevels(d[[v]]))) >= 2L, logical(1))
  if (any(!keep)) cat("    (dropped single-level controls:",
                      paste(ctrls[!keep], collapse = ", "), ")\n")
  ctrls[keep]
}
.stars <- function(p) fifelse(p < 0.001, "***",
                fifelse(p < 0.01, "**", fifelse(p < 0.05, "*", "")))
.tidy_full <- function(model) {
  co <- as.data.table(summary(model)$coeftable, keep.rownames = "term")
  setnames(co, 2:5, c("estimate", "se", "stat", "p"))
  co[, stars := .stars(p)][]
}

# residual gaps vs white: Welch test on mean residuals. fixest drops rows at
# estimation (NA covariates, degenerate FE cells); predict() returns NA for
# those, and they MUST be excluded or one NA poisons every gap.
residual_gaps <- function(d) {
  d <- d[!is.na(resid)]
  w <- d[group == "white", .(mu_w = mean(resid), v_w = var(resid), n_w = .N)]
  if (!nrow(w) || w$n_w < 2L)      # no usable white benchmark in this sample
    return(data.table(group = character(), mu = numeric(), v = numeric(),
                      n = integer(), gap = numeric(), se = numeric(),
                      n_white = integer(), z = numeric(), p = numeric(),
                      lo = numeric(), hi = numeric(), sig = logical()))
  g <- d[group %in% GROUPS,
         .(mu = mean(resid), v = var(resid), n = .N), by = group]
  g[, `:=`(gap = mu - w$mu_w, se = sqrt(v / n + w$v_w / w$n_w), n_white = w$n_w)]
  g[, `:=`(z = gap / se)]
  g[, `:=`(p = 2 * pnorm(-abs(z)), lo = gap - 1.96 * se, hi = gap + 1.96 * se)]
  g[, sig := p < SIG]
  g[order(group)]
}
.print_gaps <- function(gt, unit, scale = 1) {
  cat(sprintf("    residual gap vs white (%s):\n", unit))
  cat(sprintf("    %-10s %10s %9s %8s %9s %s\n",
              "group", "gap", "se", "p", "n", "sig"))
  for (i in seq_len(nrow(gt))) with(gt[i],
    cat(sprintf("    %-10s %10.4f %9.4f %8.4f %9s %s\n",
                group, gap * scale, se * scale, p,
                format(n, big.mark = ","), .stars(p))))
}
# per-model diagnostic line + going-off checks, shared by every block
.model_diag <- function(m, d, y, label, kind) {
  pred <- if (kind == "logit") predict(m, newdata = d, type = "response")
          else predict(m, newdata = d)
  na_share <- mean(is.na(pred))
  fit_stat <- unname(if (kind == "logit") 1 - m$deviance / m$null.deviance
                     else suppressWarnings(r2(m, "r2")))
  if (!length(fit_stat) || !is.finite(fit_stat)) fit_stat <- NA_real_
  cat(sprintf("  [%s] n=%s | %s=%.3f | mean(pred)=%.4f vs mean(y)=%.4f | pred NA %.1f%%\n",
              label, format(nobs(m), big.mark = ","),
              if (kind == "logit") "pseudo-R2" else "R2",
              fit_stat, mean(pred, na.rm = TRUE), mean(y, na.rm = TRUE),
              100 * na_share))
  if (isTRUE(na_share > 0.10))
    cat("    !! >10% NA predictions: many rows dropped at estimation --",
        "check covariate NAs / degenerate FE cells for this category\n")
  if (kind == "logit" && is.finite(fit_stat) &&
      (fit_stat < 0.01 || fit_stat > 0.7))
    cat("    !! pseudo-R2 outside the usual 0.01-0.70 band -- inspect the",
        "specification or the sample for this category\n")
  pred
}

coef_tables <- list()          # every model's full coefficient table (for PDF)

# ================= BLOCK 1: DENIAL LOGITS (with-credit spec) ==================
# Leaves in env: mods_denial (named list of feglm fits), den_gaps, den_resid.
# Rerun this block alone after any spec change; inspect e.g.
#   summary(mods_denial[["conv_purchase"]]);  den_gaps[sig == TRUE]
cat("== BLOCK 1: denial (race-blind, with-credit) ==\n")
mods_denial <- list(); .gap_l <- list(); .res_l <- list()
for (lc in levels(dat$loan_cat)) {
  d <- droplevels(dat[loan_cat == lc & in_denial_universe == TRUE &
                      !is.na(group) &
                      !is.na(cs_bin) & !is.na(dti_bin) & !is.na(ltv_bin)])
  if (nrow(d) < MIN_N) {
    cat(sprintf("  [denied:%s] skipped (n=%d < %d)\n", lc, nrow(d), MIN_N)); next
  }
  rhs <- paste(.active(d, ctrl_denial), collapse = " + ")
  m <- feglm(as.formula(sprintf("denied ~ %s | %s", rhs, FE)),
             d, family = binomial("logit"), cluster = ~ lei)
  pred <- .model_diag(m, d, d$denied, paste0("denied:", lc), "logit")
  fc <- .tidy_full(m); fc[, `:=`(model = sprintf("denied | %s", lc), n = nobs(m))]
  coef_tables[[length(coef_tables) + 1L]] <- fc
  d[, resid := denied - pred]
  gt <- residual_gaps(d)
  .print_gaps(gt, "percentage points", scale = 100)
  gt[, `:=`(loan_cat = lc, outcome = "denied")]
  mods_denial[[lc]] <- m
  .gap_l[[lc]] <- gt
  .res_l[[lc]] <- d[!is.na(resid), .(uli, lei, cu_number, group, loan_cat, resid)]
}
den_gaps  <- rbindlist(.gap_l, fill = TRUE)
den_resid <- rbindlist(.res_l, fill = TRUE)
.chk(length(mods_denial) >= 1, "denial: at least one category estimated")
.chk(den_gaps[n >= 200, all(!is.na(gap) & !is.na(p))],
     "denial gaps compute for all n>=200 cells (no NA poisoning)")
.chk(den_resid[, abs(mean(resid))] < 0.02,
     "denial residuals centered near zero overall")
.chk(den_gaps[, all(abs(gap) < 0.30)],
     "no denial gap beyond 30pp (larger usually means a data problem)")
cat(sprintf("Denial: %d models | %s residual rows | residual quantiles:\n",
            length(mods_denial), format(nrow(den_resid), big.mark = ",")))
print(round(quantile(den_resid$resid, c(.01, .25, .5, .75, .99)), 3))

# ================ BLOCK 2: WITHDRAWAL LOGITS (no-credit spec) =================
# Leaves in env: mods_wdr, wdr_gaps, wdr_resid.
cat("== BLOCK 2: withdrawal (race-blind, no-credit) ==\n")
mods_wdr <- list(); .gap_l <- list(); .res_l <- list()
for (lc in levels(dat$loan_cat)) {
  d <- droplevels(dat[loan_cat == lc & in_withdrawal_universe == TRUE &
                      !is.na(group)])
  if (nrow(d) < MIN_N) {
    cat(sprintf("  [withdrawn:%s] skipped (n=%d < %d)\n", lc, nrow(d), MIN_N)); next
  }
  rhs <- paste(.active(d, ctrl_wdr), collapse = " + ")
  m <- feglm(as.formula(sprintf("withdrawn ~ %s | %s", rhs, FE)),
             d, family = binomial("logit"), cluster = ~ lei)
  pred <- .model_diag(m, d, d$withdrawn, paste0("withdrawn:", lc), "logit")
  fc <- .tidy_full(m); fc[, `:=`(model = sprintf("withdrawn | %s", lc), n = nobs(m))]
  coef_tables[[length(coef_tables) + 1L]] <- fc
  d[, resid := withdrawn - pred]
  gt <- residual_gaps(d)
  .print_gaps(gt, "percentage points", scale = 100)
  gt[, `:=`(loan_cat = lc, outcome = "withdrawn")]
  mods_wdr[[lc]] <- m
  .gap_l[[lc]] <- gt
  .res_l[[lc]] <- d[!is.na(resid), .(uli, lei, cu_number, group, loan_cat, resid)]
}
wdr_gaps  <- rbindlist(.gap_l, fill = TRUE)
wdr_resid <- rbindlist(.res_l, fill = TRUE)
.chk(length(mods_wdr) >= 1, "withdrawal: at least one category estimated")
.chk(wdr_gaps[n >= 200, all(!is.na(gap) & !is.na(p))],
     "withdrawal gaps compute for all n>=200 cells")

# ============== BLOCK 3: PRICING -- FOUR RACE-BLIND COMPONENTS ================
# Leaves in env: mods_price (list keyed "cat | component"), pri_gaps,
# pri_resid (wide: rate residual + other-cost composite per loan).
# NOTE the lender_credits sign: credits REDUCE borrower cost, so a POSITIVE
# gap on that component is FAVORABLE to the group.
cat("== BLOCK 3: pricing (race-blind, four components) ==\n")
rate_dv <- if (dat[, any(!is.na(ir_spread_pmms))]) {
  "ir_spread_pmms"
} else "interest_rate"
comps <- c(interest_rate = rate_dv, disc_points = "disc_pts_pct",
           lender_credits = "lend_cred_pct", loan_costs = "loan_cost_pct")
mods_price <- list(); .gap_l <- list(); .res_l <- list()
for (lc in levels(dat$loan_cat)) for (nm in names(comps)) {
  dv <- comps[[nm]]
  d  <- droplevels(dat[loan_cat == lc & originated == 1 & !is.na(get(dv)) &
                       !is.na(group) & !is.na(cs_bin) & !is.na(ltv_bin)])
  if (nrow(d) < MIN_N) next
  rhs <- paste(.active(d, ctrl_pricing), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | %s", dv, rhs, FE)), d,
             cluster = ~ lei)
  pred <- .model_diag(m, d, d[[dv]], paste0("price:", nm, ":", lc), "ols")
  fc <- .tidy_full(m)
  fc[, `:=`(model = sprintf("pricing %s | %s", nm, lc), n = nobs(m))]
  coef_tables[[length(coef_tables) + 1L]] <- fc
  d[, resid := get(dv) - pred]
  gt <- residual_gaps(d)
  .print_gaps(gt, if (nm == "interest_rate") "rate, percentage points"
                  else "% of loan amount")
  gt[, `:=`(loan_cat = lc, component = nm)]
  mods_price[[paste(lc, nm, sep = " | ")]] <- m
  .gap_l[[paste(lc, nm)]] <- gt
  .res_l[[paste(lc, nm)]] <-
    d[!is.na(resid),
      .(uli, lei, cu_number, group, loan_cat, component = nm, resid)]
}
pri_gaps <- rbindlist(.gap_l, fill = TRUE)
# wide per-loan residual table: rate residual + SAS other-cost composite
# (r_dp - r_lc + r_lcst, all in % of loan amount)
.R <- rbindlist(.res_l, fill = TRUE)
pri_resid <- dcast(.R, uli + lei + cu_number + group + loan_cat ~ component,
                   value.var = "resid")
for (v in names(comps)) if (!v %in% names(pri_resid)) pri_resid[, (v) := NA_real_]
pri_resid[, resid := interest_rate]
pri_resid[, resid_oth := disc_points - lender_credits + loan_costs]
pri_resid <- pri_resid[!is.na(resid),
                       .(uli, lei, cu_number, group, loan_cat, resid, resid_oth)]
.chk(length(mods_price) >= 1, "pricing: at least one component model estimated")
.chk(pri_gaps[component == "interest_rate" & n >= 200, all(!is.na(gap))],
     "rate gaps compute for all n>=200 cells")
.chk(pri_resid[, abs(mean(resid))] < 0.05,
     "rate residuals centered near zero overall")
rm(.gap_l, .res_l, .R)

# ============ BLOCK 4: GAP TABLES, DOLLAR DIFFS, RESIDUAL SAVE ================
# dollar aggregation of pricing gaps ($200k / 30yr, Popick fn. 16)
pricing_dollar_diff <- function(gaps, loan = 200000, term = 360,
                                base_rate = 0.065, disc_annual = 0.03) {
  w <- dcast(gaps, loan_cat + group ~ component, value.var = "gap")
  for (v in c("interest_rate", "disc_points", "loan_costs", "lender_credits"))
    if (!v %in% names(w)) w[, (v) := NA_real_]
  pv <- function(d_pp) {
    if (is.na(d_pp)) return(NA_real_)
    pmt <- function(r) loan * r / (1 - (1 + r)^(-term))
    ex <- pmt((base_rate + d_pp / 100) / 12) - pmt(base_rate / 12)
    dr <- disc_annual / 12
    ex * (1 - (1 + dr)^(-term)) / dr
  }
  g0 <- function(x) fifelse(is.na(x), 0, x)
  w[, dollar_interest := vapply(interest_rate, pv, numeric(1))]
  w[, dollar_total := g0(dollar_interest) + g0(disc_points) / 100 * loan +
      g0(loan_costs) / 100 * loan - g0(lender_credits) / 100 * loan]
  w[order(loan_cat, group)]
}
fwrite(rbindlist(coef_tables), out("model_coefficients_2025.csv"))
fwrite(den_gaps, out("denial_gaps_2025.csv"))
fwrite(wdr_gaps, out("withdrawal_gaps_2025.csv"))
fwrite(pri_gaps, out("pricing_gaps_2025.csv"))
fwrite(pricing_dollar_diff(pri_gaps), out("pricing_dollar_diff_2025.csv"))

setnames(copy(den_resid), "resid", "resid_denial") -> .dn
setnames(copy(wdr_resid), "resid", "resid_withdrawn") -> .wd
setnames(copy(pri_resid), "resid", "resid_price") -> .pr
saveRDS(list(denial = .dn, withdrawal = .wd, pricing = .pr),
        out("residuals_2025.rds"), compress = FALSE)
rm(.dn, .wd, .pr)
cat("Saved -> coefficients + gap tables (5 CSVs) + residuals_2025.rds\n")
cat("\nSignificant residual gaps at 1% (denial):\n")
print(den_gaps[sig == TRUE, .(loan_cat, group, gap_pp = round(100 * gap, 2),
                              p = signif(p, 2))])
cat("Significant residual gaps at 1% (rate):\n")
print(pri_gaps[component == "interest_rate" & sig == TRUE,
               .(loan_cat, group, gap_bp = round(100 * gap, 1),
                 p = signif(p, 2))])

# =================== BLOCK 5: REGRESSION OUTPUT PDF ===========================
.page_text <- function(title, subtitle, body, size = 3.0) {
  ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void() +
    labs(title = title, subtitle = subtitle) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(size = 10, colour = "grey30"),
          plot.margin = margin(20, 24, 20, 24)) +
    annotate("text", x = 0, y = 1, label = body, hjust = 0, vjust = 1,
             family = "mono", size = size)
}
.page_gap_forest <- function(gt, title, subtitle, x_lab, scale = 1) {
  d <- copy(gt)[, `:=`(x = gap * scale, xlo = lo * scale, xhi = hi * scale)]
  ggplot(d, aes(x = x, y = group, colour = sig)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
    geom_errorbarh(aes(xmin = xlo, xmax = xhi), height = 0.2) +
    geom_point(size = 2) + facet_wrap(~ loan_cat) +
    scale_colour_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "grey55"),
                        labels = c(`TRUE` = "sig at 1%", `FALSE` = "not sig"),
                        name = NULL) +
    labs(title = title, subtitle = subtitle, x = x_lab, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
}
pdf(out("regression_output_2025.pdf"), width = 11, height = 8.5)
print(.page_text("NCUA HMDA Fair-Lending Models -- 2025 (race-blind)",
  format(Sys.time(), "Generated %Y-%m-%d %H:%M"), paste0(
  "Models are RACE-BLIND: race/ethnicity is NOT a regressor anywhere.\n",
  "Each model predicts the outcome from underwriting factors only:\n",
  "  CS/DTI/LTV bins (+ income bins for pricing), AUS, broker channel,\n",
  "  additional lien, offers-other-loan-types, lender-size bin\n",
  "  | state x MSA-status FE + year-month FE, SEs clustered by LEI.\n\n",
  "The fair-lending evidence is in the RESIDUALS: for each group, the\n",
  "mean residual gap vs white applicants, with a Welch test. A significant\n",
  "positive gap = the group fares systematically worse than the race-blind\n",
  "model predicts, relative to comparable white borrowers.\n\n",
  "  Stars: *** p<0.001   ** p<0.01   * p<0.05\n",
  "  Models estimated: ", length(coef_tables), "\n"), size = 3.4))
print(.page_text("How to read the residual gaps", "", paste0(
  "DENIAL / WITHDRAWAL (shown in percentage points)\n",
  "  gap > 0 -> the group is denied / withdraws MORE than the race-blind\n",
  "  model predicts, relative to comparable white applicants.\n\n",
  "PRICING -- rate spread (pp), discount points %, total loan costs %\n",
  "  gap > 0 -> the group PAYS MORE than comparable white borrowers.\n\n",
  "PRICING -- lender credits %  (NOTE THE FLIP)\n",
  "  credits REDUCE borrower cost, so gap > 0 = the group RECEIVES MORE\n",
  "  credits (favorable); the ADVERSE sign for this component is NEGATIVE.\n\n",
  "Gaps are conditional associations, not causal effects. Full control\n",
  "coefficients follow, one model per page."), size = 3.6))
if (nrow(den_gaps)) print(.page_gap_forest(den_gaps,
  "Denial: mean residual gap vs white, by group and loan category",
  "positive = denied more than the race-blind model predicts | bars = 95% CI",
  "gap (percentage points)", scale = 100))
if (nrow(wdr_gaps)) print(.page_gap_forest(wdr_gaps,
  "Withdrawal: mean residual gap vs white, by group and loan category",
  "no-credit specification | positive = withdraws more | bars = 95% CI",
  "gap (percentage points)", scale = 100))
for (cmp in unique(pri_gaps$component)) {
  sub <- if (cmp == "lender_credits")
    "gap > 0 = MORE credits RECEIVED (favorable); adverse sign is NEGATIVE"
  else "gap > 0 = the group pays more than comparable white borrowers | 95% CI"
  xl <- if (cmp == "interest_rate") "gap (rate, percentage points)"
        else "gap (% of loan amount)"
  print(.page_gap_forest(pri_gaps[component == cmp],
    sprintf("Pricing: %s -- mean residual gap vs white", cmp), sub, xl))
}
for (fc in coef_tables) {
  rows <- fc[, sprintf("  %-36s %10.4f %9.4f %8.4f %-3s",
                       substr(term, 1, 36), estimate, se, p, stars)]
  body <- paste(c(sprintf("  %-36s %10s %9s %8s %s",
                          "term", "estimate", "std.err", "p", "sig"),
                  paste0("  ", strrep("-", 72)), rows), collapse = "\n")
  print(.page_text(paste0(fc$model[1], "  (race-blind)"),
    sprintf("n = %s | FE: state x MSA + year-month (absorbed) | SE clustered by LEI",
            format(fc$n[1], big.mark = ",")), body, size = 2.6))
}
dev.off()
cat("Regression PDF ->", out("regression_output_2025.pdf"), "\n")
cat("Left in the environment: mods_denial, mods_wdr, mods_price,",
    "den_gaps/wdr_gaps/pri_gaps, den_resid/wdr_resid/pri_resid, coef_tables\n")
