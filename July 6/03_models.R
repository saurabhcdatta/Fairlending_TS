# =============================================================================
# 03_models.R  --  RACE-BLIND fair-lending models + residual-gap analysis.
#                  Estimated separately by loan category (Popick Types 1-10).
#
# DESIGN. Race/ethnicity is NOT a regressor in ANY model. Each model predicts
# the outcome from legitimate underwriting factors only:
#     CS/DTI/LTV bins (+ income bins for pricing), AUS, broker channel,
#     additional lien, offers-other-loan-types, lender-size bin
#     | state x MSA-status FE + year-month FE,  SEs clustered by LEI.
# The fair-lending question is answered AFTER estimation, from the residuals:
# for each minority group, is the mean residual different from WHITE
# applicants' mean residual? A significant positive gap means the group
# experiences systematically worse outcomes than the race-blind model
# predicts, relative to comparable white borrowers.
#
#   DENIAL      logit over decisioned applications (with-credit spec)
#   WITHDRAWAL  logit over the application universe (no-credit spec)
#   PRICING     four components: PMMS rate spread, discount points %,
#               lender credits %, total loan costs % (originated loans)
#
# Outputs:
#   model_coefficients_2025.csv   every control coefficient, every model
#   denial_gaps_2025.csv          mean residual gap vs white, by group x cat
#   withdrawal_gaps_2025.csv      (same, withdrawal)
#   pricing_gaps_2025.csv         (same, per pricing component)
#   pricing_dollar_diff_2025.csv  gaps aggregated to $ on a $200k/30yr loan
#   regression_output_2025.pdf    coefficients + residual-gap forest plots
#   residuals_2025.rds            row-level residuals for 04_screen.R
# =============================================================================

library(data.table)
library(fixest)
library(ggplot2)

source("settings.R")

dat <- readRDS(out("analysis_2025.rds"))

FE    <- "state_msa + year_month"
SIG   <- 0.01
MIN_N <- 1000L
GROUPS <- c("black", "hispanic", "asian", "aian", "nhpi")

.logit_controls <- function(require_credit) {
  base <- c("early_bankrupt", "aus", "broker", "other_lien",
            "offers_other_types", "lender_orig_bin")
  if (require_credit) c("cs_bin", "dti_bin", "ltv_bin", base) else base
}
.pricing_controls <- function()
  c("cs_bin", "ltv_bin", "income_bin", "early_bankrupt", "broker",
    "other_lien", "offers_other_types", "lender_orig_bin")

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

# ---- residual gaps vs white: Welch test on mean residuals --------------------
# gap = mean(resid | group) - mean(resid | white). Positive = worse outcome
# than the race-blind model predicts, relative to comparable white borrowers.
residual_gaps <- function(d) {
  # fixest drops observations during estimation (NA covariates; FE levels
  # with only-0/only-1 outcomes or singletons); predict() returns NA for
  # those rows. They must be EXCLUDED here -- one NA otherwise poisons the
  # whole group mean and every gap prints NA.
  d <- d[!is.na(resid)]
  w <- d[group == "white", .(mu_w = mean(resid), v_w = var(resid), n_w = .N)]
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

# ---- logits (denial / withdrawal), race-blind ---------------------------------
run_logit_models <- function(dat, outcome, universe_col, require_credit) {
  gaps <- list(); resid_rows <- list()
  for (lc in levels(dat$loan_cat)) {
    d <- droplevels(dat[loan_cat == lc & get(universe_col) == TRUE &
                        !is.na(group)])
    if (require_credit) d <- d[!is.na(cs_bin) & !is.na(dti_bin) & !is.na(ltv_bin)]
    if (nrow(d) < MIN_N) {
      cat(sprintf("  [%s:%s] skipped (n=%d)\n", outcome, lc, nrow(d))); next
    }
    rhs <- paste(.active(d, .logit_controls(require_credit)), collapse = " + ")
    m <- feglm(as.formula(sprintf("%s ~ %s | %s", outcome, rhs, FE)),
               d, family = binomial("logit"), cluster = ~ lei)
    cat(sprintf("  [%s:%s] n=%s | pseudo-R2=%.3f  (race-blind)\n", outcome, lc,
                format(nobs(m), big.mark = ","),
                1 - m$deviance / m$null.deviance))
    fc <- .tidy_full(m)
    fc[, `:=`(model = sprintf("%s | %s", outcome, lc), n = nobs(m))]
    .FULL[[length(.FULL) + 1L]] <<- fc
    d[, resid := get(outcome) - predict(m, newdata = d, type = "response")]
    gt <- residual_gaps(d)
    .print_gaps(gt, "percentage points", scale = 100)
    gt[, `:=`(loan_cat = lc, outcome = outcome)]
    gaps[[lc]] <- gt
    resid_rows[[lc]] <- d[!is.na(resid),
                          .(uli, lei, cu_number, group, loan_cat, resid)]
  }
  list(gaps = rbindlist(gaps, fill = TRUE),
       resid = rbindlist(resid_rows, fill = TRUE))
}

# ---- pricing: four race-blind component models ---------------------------------
run_pricing_models <- function(dat) {
  rate_dv <- if (dat[, any(!is.na(ir_spread_pmms))]) "ir_spread_pmms"
             else "interest_rate"
  comps <- c(interest_rate = rate_dv, disc_points = "disc_pts_pct",
             lender_credits = "lend_cred_pct", loan_costs = "loan_cost_pct")
  gaps <- list(); resid_rows <- list()
  for (lc in levels(dat$loan_cat)) for (nm in names(comps)) {
    dv <- comps[[nm]]
    d  <- droplevels(dat[loan_cat == lc & originated == 1 & !is.na(get(dv)) &
                         !is.na(group) & !is.na(cs_bin) & !is.na(ltv_bin)])
    if (nrow(d) < MIN_N) next
    rhs <- paste(.active(d, .pricing_controls()), collapse = " + ")
    m <- feols(as.formula(sprintf("%s ~ %s | %s", dv, rhs, FE)), d,
               cluster = ~ lei)
    cat(sprintf("  [price:%s:%s] n=%s | R2=%.3f  (race-blind)\n", nm, lc,
                format(nobs(m), big.mark = ","), r2(m, "r2")))
    fc <- .tidy_full(m)
    fc[, `:=`(model = sprintf("pricing %s | %s", nm, lc), n = nobs(m))]
    .FULL[[length(.FULL) + 1L]] <<- fc
    d[, resid := get(dv) - predict(m, newdata = d)]
    gt <- residual_gaps(d)
    .print_gaps(gt, if (nm == "interest_rate") "rate, percentage points"
                    else "% of loan amount")
    gt[, `:=`(loan_cat = lc, component = nm)]
    gaps[[paste(lc, nm)]] <- gt
    resid_rows[[paste(lc, nm)]] <-
      d[!is.na(resid),
        .(uli, lei, cu_number, group, loan_cat, component = nm, resid)]
  }
  # wide per-loan residual table: rate residual + SAS other-cost composite
  # oth = disc_points - lender_credits + loan_costs residuals (per the
  # production SAS: r_dp - r_lc + r_lcst), all in % of loan amount
  R <- rbindlist(resid_rows, fill = TRUE)
  W <- dcast(R, uli + lei + cu_number + group + loan_cat ~ component,
             value.var = "resid")
  for (v in c("interest_rate", "disc_points", "lender_credits", "loan_costs"))
    if (!v %in% names(W)) W[, (v) := NA_real_]
  W[, resid := interest_rate]
  W[, resid_oth := disc_points - lender_credits + loan_costs]
  list(gaps = rbindlist(gaps, fill = TRUE),
       resid = W[!is.na(resid),
                 .(uli, lei, cu_number, group, loan_cat, resid, resid_oth)])
}

# ---- dollar aggregation of the pricing gaps ($200k / 30yr, Popick fn. 16) -----
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

# ---- four-component pricing system: SUR, deliberately NOT 2SLS ----------------
# The production SAS estimates this system with proc syslin 2SLS (all four
# price components endogenous, income/LTV/CS bins as instruments). We
# DELIBERATELY use SUR instead, per Popick's published method, because:
#   1. IDENTIFICATION. 2SLS needs excluded instruments per equation --
#      exclusion restrictions like "income shifts points but not the rate
#      directly" that are hard to defend for jointly-negotiated prices.
#      Arguable exclusions = arguable identification; the structural
#      cross-price coefficients would not be credible.
#   2. SMALL CATEGORIES. With weak/questionable instruments, 2SLS is badly
#      behaved in finite samples (bias toward OLS, explosive variance).
#      Several loan categories are thin (FHA refi, jumbo cells); reliable
#      2SLS estimates there are not attainable.
#   3. THE OBJECT. For fair lending we need the CONDITIONAL FIT and its
#      residuals, not structural price elasticities. SUR is always
#      estimable, gains efficiency from cross-equation error correlation,
#      is stable in small samples, and degrades gracefully to
#      equation-by-equation OLS below min_n_sur (the SAS-style fallback).
# SUR does not "fix" the endogeneity of the cross-price terms either -- it
# simply does not claim to; those coefficients are read as associations.
sur_pricing <- function(d, n_sub = 50000L, min_n_sur = 5000L, seed = 1L) {
  comps <- c(ir = "ir_spread_pmms", dp = "disc_pts_pct",
             lc = "lend_cred_pct",  tc = "loan_cost_pct")
  need <- c(unname(comps), .pricing_controls(), "state_msa", "year_month")
  d <- droplevels(d[complete.cases(d[, ..need])])
  if (nrow(d) > n_sub) { set.seed(seed); d <- droplevels(d[sample(.N, n_sub)]) }
  ctrl <- paste(c(.active(d, .pricing_controls()), "state_msa", "year_month"),
                collapse = " + ")
  eqs <- lapply(names(comps), function(k) {
    others <- setdiff(unname(comps), comps[[k]])
    as.formula(paste(comps[[k]], "~", paste(others, collapse = " + "),
                     "+", ctrl))
  })
  names(eqs) <- names(comps)
  k_par <- 3 + sum(vapply(.active(d, .pricing_controls()), function(v)
      if (is.factor(d[[v]])) nlevels(d[[v]]) else 1L, integer(1))) +
    nlevels(factor(d$state_msa)) + nlevels(factor(d$year_month))
  ols_fallback <- function(reason) {
    cat("  sur_pricing:", reason,
        "-> falling back to equation-by-equation OLS\n")
    structure(lapply(eqs, function(f) lm(f, data = d)),
              method = "OLS_fallback", n = nrow(d), reason = reason)
  }
  if (!requireNamespace("systemfit", quietly = TRUE))
    return(ols_fallback("systemfit not installed"))
  if (nrow(d) < max(min_n_sur, 5L * k_par))
    return(ols_fallback(sprintf(
      "insufficient observations for the system (n=%s vs ~%d params/eq)",
      format(nrow(d), big.mark = ","), k_par)))
  fit <- tryCatch(systemfit::systemfit(eqs, method = "SUR",
                                       data = as.data.frame(d)),
                  error = identity)
  if (inherits(fit, "error"))
    return(ols_fallback(paste("SUR failed:", conditionMessage(fit))))
  structure(fit, method = "SUR", n = nrow(d))
}

# ============================ regression output PDF ============================
.page_text <- function(title, subtitle, body, size = 3.0) {
  ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void() +
    labs(title = title, subtitle = subtitle) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(size = 10, colour = "grey30"),
          plot.margin = margin(20, 24, 20, 24)) +
    annotate("text", x = 0, y = 1, label = body, hjust = 0, vjust = 1,
             family = "mono", size = size)
}

# forest of MEAN RESIDUAL GAPS by group (vs white), faceted by loan category
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

save_models_pdf <- function(den_g, wdr_g, pri_g, full, path) {
  pdf(path, width = 11, height = 8.5)
  on.exit(dev.off(), add = TRUE)
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
    "  Models estimated: ", length(full), "\n"), size = 3.4))
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
  if (nrow(den_g)) print(.page_gap_forest(den_g,
    "Denial: mean residual gap vs white, by group and loan category",
    "positive = denied more than the race-blind model predicts | bars = 95% CI",
    "gap (percentage points)", scale = 100))
  if (nrow(wdr_g)) print(.page_gap_forest(wdr_g,
    "Withdrawal: mean residual gap vs white, by group and loan category",
    "no-credit specification | positive = withdraws more | bars = 95% CI",
    "gap (percentage points)", scale = 100))
  for (cmp in unique(pri_g$component)) {
    sub <- if (cmp == "lender_credits")
      "gap > 0 = MORE credits RECEIVED (favorable); adverse sign is NEGATIVE"
    else "gap > 0 = the group pays more than comparable white borrowers | 95% CI"
    xl <- if (cmp == "interest_rate") "gap (rate, percentage points)"
          else "gap (% of loan amount)"
    print(.page_gap_forest(pri_g[component == cmp],
      sprintf("Pricing: %s -- mean residual gap vs white", cmp), sub, xl))
  }
  for (fc in full) {
    rows <- fc[, sprintf("  %-36s %10.4f %9.4f %8.4f %-3s",
                         substr(term, 1, 36), estimate, se, p, stars)]
    body <- paste(c(sprintf("  %-36s %10s %9s %8s %s",
                            "term", "estimate", "std.err", "p", "sig"),
                    paste0("  ", strrep("-", 72)), rows), collapse = "\n")
    print(.page_text(paste0(fc$model[1], "  (race-blind)"),
      sprintf("n = %s | FE: state x MSA + year-month (absorbed) | SE clustered by LEI",
              format(fc$n[1], big.mark = ",")), body, size = 2.6))
  }
  invisible(path)
}

# ============================== run everything =================================
.FULL <- list()
cat("== denial (race-blind, with_credit) ==\n")
den <- run_logit_models(dat, "denied", "in_denial_universe", TRUE)
cat("== withdrawal (race-blind, no_credit) ==\n")
wdr <- run_logit_models(dat, "withdrawn", "in_withdrawal_universe", FALSE)
cat("== pricing (race-blind, four components) ==\n")
pri <- run_pricing_models(dat)

fwrite(rbindlist(.FULL), out("model_coefficients_2025.csv"))
fwrite(den$gaps, out("denial_gaps_2025.csv"))
fwrite(wdr$gaps, out("withdrawal_gaps_2025.csv"))
fwrite(pri$gaps, out("pricing_gaps_2025.csv"))
fwrite(pricing_dollar_diff(pri$gaps), out("pricing_dollar_diff_2025.csv"))

save_models_pdf(den$gaps, wdr$gaps, pri$gaps, .FULL,
                out("regression_output_2025.pdf"))
cat("Regression PDF ->", out("regression_output_2025.pdf"),
    sprintf("(%d model pages + guide + gap forests)\n", length(.FULL)))

setnames(den$resid, "resid", "resid_denial")
setnames(wdr$resid, "resid", "resid_withdrawn")
setnames(pri$resid, "resid", "resid_price")
saveRDS(list(denial = den$resid, withdrawal = wdr$resid, pricing = pri$resid),
        out("residuals_2025.rds"), compress = FALSE)
cat("Saved -> coefficients + gap tables (5 csv), PDF, residuals_2025.rds\n")
cat("\nSignificant residual gaps (denial, at 1%):\n")
print(den$gaps[sig == TRUE, .(loan_cat, group, gap_pp = round(100 * gap, 2),
                              p = signif(p, 2))])
