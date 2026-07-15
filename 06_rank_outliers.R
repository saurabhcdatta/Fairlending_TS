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

cat("== 06_rank_outliers.R VERSION 2026-07-15a",
    "(multi-tab workbook + Summary + rebuttal w/ model context) ==\n")

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
    "denial_reason1", "denial_reason2", "denial_reason3",
    "cs_bin", "dti_bin", "ltv_bin", "income_bin"), names(dat))
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
# ---- WHY-ENGINE: which factors made this outcome an outlier -------------------
# For the Popick stream the logit is transparent: each factor's coefficient
# IS its contribution. For every outlier loan we look up the coefficients of
# the loan's own characteristics in that screen's model and report:
#   outlier_reason        the top factors that pointed toward the GOOD
#                         outcome (approval / retention / cheaper rate) --
#                         i.e., what made the actual outcome surprising
#   risk_factors_present  any characteristics that pointed the other way,
#                         so the reviewer sees both sides
# (ML-stream equivalent: shap_outliers_2025.csv from 03b.)
cf_file <- out("model_coefficients_2025.csv")
if (file.exists(cf_file)) {
  cf <- fread(cf_file)
  # fixest names plain-factor terms as <var><level>, e.g. "cs_bin[740, Inf)";
  # numeric dummies (aus, broker, early_bankrupt) are just the bare name --
  # their coefficient applies at level "1".
  expl_vars <- c("cs_bin", "dti_bin", "ltv_bin", "income_bin",
                 "aus", "broker", "early_bankrupt")
  cf[, var := ""]
  for (v in expl_vars[order(-nchar(expl_vars))])
    cf[var == "" & startsWith(term, v),
       `:=`(var = v, level = substring(term, nchar(v) + 1L))]
  cf <- cf[var != ""]
  cf[level == "", level := "1"]
  # model key per screen: denial/withdrawn logits; pricing = rate model
  loans[, mkey := fifelse(screen == "denial",
                    sprintf("denied | %s", loan_cat),
                  fifelse(screen == "withdrawal",
                    sprintf("withdrawn | %s", loan_cat),
                  fifelse(screen == "pricing",
                    sprintf("pricing interest_rate | %s", loan_cat),
                    NA_character_)))]
  have <- intersect(expl_vars, names(loans))
  lv <- melt(loans[, c("uli", "mkey", have), with = FALSE],
             id.vars = c("uli", "mkey"), variable.name = "var",
             value.name = "level", variable.factor = FALSE)
  lv[, level := as.character(level)]
  lv <- merge(lv, cf[, .(model, var, level, estimate)],
              by.x = c("mkey", "var", "level"),
              by.y = c("model", "var", "level"))
  lv[, lbl := sprintf("%s=%s (%+.2f)", var, level, estimate)]
  # ---- plain-language rendering for non-technical readers ----------------
  .rng <- function(x) {           # "(80,90]" -> "between 80 and 90", etc.
    x <- trimws(x)
    first <- substr(x, 1, 1); last <- substr(x, nchar(x), nchar(x))
    if (!(first %in% c("(", "[")) || !(last %in% c(")", "]"))) return(x)
    parts <- trimws(strsplit(substr(x, 2, nchar(x) - 1), ",",
                             fixed = TRUE)[[1]])
    if (length(parts) != 2) return(x)
    lo <- parts[1]; hi <- parts[2]
    if (lo %in% c("-Inf", "-inf")) paste(hi, "or below")
    else if (hi %in% c("Inf", "inf")) paste(lo, "or above")
    else paste("between", lo, "and", hi)
  }
  .noun <- c(cs_bin = "credit score %s",
             dti_bin = "debt-to-income ratio %s%%",
             ltv_bin = "loan-to-value %s%%",
             income_bin = "income in the %s band",
             aus = "application went through automated underwriting",
             broker = "application came through a broker",
             early_bankrupt = "recent credit-risk markers on file")
  lv[, plain := vapply(seq_len(.N), function(i) {
        v <- var[i]
        if (v %in% c("aus", "broker", "early_bankrupt")) .noun[[v]]
        else sprintf(.noun[[v]], .rng(level[i]))
      }, character(1))]
  lv[, weight := fifelse(abs(estimate) >= 0.8, "major factor",
                 fifelse(abs(estimate) >= 0.3, "moderate factor",
                                               "supporting factor"))]
  lv[, plain_w := sprintf("%s (%s)", plain, weight)]
  good <- lv[estimate < 0][order(estimate),
             .(outlier_reason = paste(head(lbl, 4), collapse = "; "),
               reason_plain   = paste(sprintf("%d) %s",
                                              seq_len(min(.N, 4)),
                                              head(plain_w, 4)),
                                      collapse = "; ")),
             by = uli]
  bad  <- lv[estimate > 0.05][order(-estimate),
             .(risk_factors_present = paste(head(lbl, 2), collapse = "; "),
               risk_plain = paste(head(plain_w, 2), collapse = "; ")),
             by = uli]
  loans <- merge(loans, good, by = "uli", all.x = TRUE)
  loans <- merge(loans, bad,  by = "uli", all.x = TRUE)
  for (cc in c("outlier_reason", "reason_plain", "risk_factors_present",
               "risk_plain"))
    if (cc %in% names(loans)) loans[is.na(get(cc)), (cc) := ""]
  # (kept compact: factors are ranked 1) 2) 3) 4); the ReadMe sheet
  #  explains that these are the reasons the model expected a good outcome)
  loans[, mkey := NULL]
  cat("Per-loan reasons attached from model coefficients",
      "(top approval-pointing factors + any risk factors).
")
} else cat("(model_coefficients_2025.csv not found -- rerun 03a for",
           "per-loan reasons)
")
# ---- MATCHED COMPARATOR: the classic examination exhibit ----------------------
# For every outlier, the closest-profile WHITE loan at the SAME CU and SAME
# product with the GOOD outcome: approved (denial), completed (withdrawal),
# or priced at/below model expectation (pricing). Match is algorithmic
# (nearest neighbor on standardized score/DTI/CLTV/amount/income), distance
# reported, and weaker_profile_comparator = 1 marks the strongest exhibits:
# the comparator's profile is no better on ANY matched dimension.
set.seed(20250101)
dat[, `:=`(.la = log(pmax(loan_amount, 1)), .li = log(pmax(income, 1)))]
mfeat <- c("credit_score", "dti", "ltv_combined", ".la", ".li")
pools <- list(
  denial     = dat[in_denial_universe == TRUE & group == "white" &
                   denied == 0],
  withdrawal = dat[in_withdrawal_universe == TRUE & group == "white" &
                   withdrawn == 0],
  pricing    = {
    pw <- res$pricing[!is.na(resid_price) & resid_price <= 0 &
                      group == "white", .(uli)]
    dat[uli %in% pw$uli]
  })
comp_rows <- list()
for (sc in intersect(names(pools), unique(loans$screen))) {
  ol <- loans[screen == sc]
  po <- pools[[sc]]
  if (!nrow(ol) || !nrow(po)) next
  for (key in unique(ol[, paste(lei, loan_cat)])) {
    oo <- ol[paste(lei, loan_cat) == key]
    pp <- po[paste(lei, loan_cat) == key]
    if (!nrow(pp)) next
    if (nrow(pp) > 5000) pp <- pp[sample(.N, 5000)]
    mu <- pp[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mfeat]
    sg <- pp[, lapply(.SD, function(z) pmax(sd(z, na.rm = TRUE), 1e-6)),
             .SDcols = mfeat]
    Z  <- as.matrix(pp[, ..mfeat]); O <- as.matrix(oo[, .(credit_score, dti,
                    ltv_combined, .la = log(pmax(loan_amount, 1)),
                    .li = log(pmax(income, 1)))])
    for (j in seq_along(mfeat)) {
      Z[, j] <- (Z[, j] - mu[[j]]) / sg[[j]]
      O[, j] <- (O[, j] - mu[[j]]) / sg[[j]]
    }
    Z[is.na(Z)] <- 0; O[is.na(O)] <- 0
    for (i in seq_len(nrow(oo))) {
      dist2 <- colSums((t(Z) - O[i, ])^2)
      b <- which.min(dist2)
      weaker <- as.integer(
        (is.na(pp$credit_score[b]) | is.na(oo$credit_score[i]) |
           pp$credit_score[b] <= oo$credit_score[i]) &
        (is.na(pp$dti[b]) | is.na(oo$dti[i]) | pp$dti[b] >= oo$dti[i]) &
        (is.na(pp$ltv_combined[b]) | is.na(oo$ltv_combined[i]) |
           pp$ltv_combined[b] >= oo$ltv_combined[i]))
      comp_rows[[length(comp_rows) + 1L]] <- data.table(
        uli = oo$uli[i], comp_uli = pp$uli[b],
        comp_credit_score = pp$credit_score[b], comp_dti = pp$dti[b],
        comp_ltv = pp$ltv_combined[b], comp_loan_amount = pp$loan_amount[b],
        comp_interest_rate = pp$interest_rate[b],
        comp_outcome = c(denial = "approved", withdrawal = "completed",
                         pricing = "priced at/below expectation")[[sc]],
        match_distance = round(sqrt(dist2[b]), 3),
        weaker_profile_comparator = weaker)
    }
  }
}
# benchmarks for the REBUTTAL column: this CU's typical APPROVED white
# borrower, per product (median score / DTI / CLTV)
wbench <- pools$denial[, .(med_cs = median(credit_score, na.rm = TRUE),
                           med_dti = median(dti, na.rm = TRUE),
                           med_ltv = median(ltv_combined, na.rm = TRUE)),
                       by = .(lei, loan_cat)]
if (length(comp_rows)) {
  loans <- merge(loans, rbindlist(comp_rows), by = "uli", all.x = TRUE)
  cat(sprintf("Matched white comparators attached: %s of %s outliers (%s with equal-or-weaker profiles)\n",
      format(sum(!is.na(loans$comp_uli)), big.mark = ","),
      format(nrow(loans), big.mark = ","),
      format(sum(loans$weaker_profile_comparator %in% 1L), big.mark = ",")))
}
loans <- merge(loans, assets[, .(lei, name)], by = "lei", all.x = TRUE)
# ---- REBUTTAL: test the institution's stated reason against its own data ------
# For each denied outlier with a stated HMDA reason, compare the applicant's
# relevant metric to (a) this CU's median APPROVED white borrower and (b)
# the matched approved comparator. When the applicant is STRONGER than
# both, the stated reason is contradicted by the institution's own lending
# record. Reasons HMDA cannot observe (cash, employment, verification,
# completeness) are labeled not testable -- honesty preserves credibility.
loans <- merge(loans, wbench, by = c("lei", "loan_cat"), all.x = TRUE)
.reb <- function(reason, cs, dti, ltv, ccs, cdti, cltv, mcs, mdti, mltv,
                 me, factor1, weaker) {
  ctx <- sprintf("Model context: %.1f%% predicted denial odds%s%s",
                 100 * me,
                 if (nzchar(factor1)) paste0("; top strength: ", factor1)
                 else "",
                 if (!is.na(weaker) && weaker == 1L)
                   "; white comparator APPROVED with equal-or-weaker profile"
                 else if (!is.na(ccs))
                   "; closest white comparator approved" else "")
  if (is.na(reason) || reason == 10)
    return(paste("NO SPECIFIC REASON REPORTED -- Reg B requires specific",
                 "adverse-action reasons.", ctx))
  if (reason == 9)
    return(paste("Stated reason 'other' (unspecific).", ctx))
  if (reason == 3) {          # credit history: higher score = stronger
    if (is.na(cs) || is.na(mcs)) return("stated reason: credit history (score unavailable)")
    tag <- if (!is.na(ccs) && cs >= mcs && cs >= ccs) "CONTRADICTED" else
           if (cs >= mcs) "QUESTIONABLE" else "consistent with data"
    sprintf("%s -- cited credit history, but applicant score %d vs CU's median APPROVED white score %d%s",
            tag, as.integer(cs), as.integer(mcs),
            if (!is.na(ccs)) sprintf(" and approved comparator %d",
                                     as.integer(ccs)) else "")
  } else if (reason == 1) {   # DTI: lower = stronger
    if (is.na(dti) || is.na(mdti)) return("stated reason: DTI (value unavailable)")
    tag <- if (!is.na(cdti) && dti <= mdti && dti <= cdti) "CONTRADICTED" else
           if (dti <= mdti) "QUESTIONABLE" else "consistent with data"
    sprintf("%s -- cited debt-to-income, but applicant DTI %.1f vs CU's median APPROVED white DTI %.1f%s",
            tag, dti, mdti,
            if (!is.na(cdti)) sprintf(" and approved comparator %.1f", cdti)
            else "")
  } else if (reason == 4) {   # collateral: lower CLTV = stronger
    if (is.na(ltv) || is.na(mltv)) return("stated reason: collateral (CLTV unavailable)")
    tag <- if (!is.na(cltv) && ltv <= mltv && ltv <= cltv) "CONTRADICTED" else
           if (ltv <= mltv) "QUESTIONABLE" else "consistent with data"
    sprintf("%s -- cited collateral, but applicant CLTV %.0f vs CU's median APPROVED white CLTV %.0f%s",
            tag, ltv, mltv,
            if (!is.na(cltv)) sprintf(" and approved comparator %.0f", cltv)
            else "")
  } else paste0("Stated reason (",
                c("1" = "DTI", "2" = "employment history",
                  "3" = "credit history", "4" = "collateral",
                  "5" = "insufficient cash", "6" = "unverifiable info",
                  "7" = "application incomplete",
                  "8" = "mortgage insurance denied")[as.character(reason)],
                ") not directly testable in HMDA. ", ctx)
}
if ("denial_reason1" %in% names(loans)) {
  loans[screen == "denial",
        rebuttal_evidence := mapply(.reb, denial_reason1, credit_score, dti,
                                    ltv_combined, comp_credit_score,
                                    comp_dti, comp_ltv, med_cs, med_dti,
                                    med_ltv, model_expected,
                                    fifelse(nzchar(reason_plain),
                                      sub(";.*$", "",
                                          sub("^1\\) ", "", reason_plain)),
                                      ""),
                                    weaker_profile_comparator)]
  loans[is.na(rebuttal_evidence), rebuttal_evidence := ""]
  cat(sprintf("Rebuttal evidence: %d stated reasons CONTRADICTED by the CU's own approvals\n",
              loans[grepl("^CONTRADICTED", rebuttal_evidence), .N]))
}
loans[, c("med_cs", "med_dti", "med_ltv") := NULL]
# ---- examiner-first columns ----------------------------------------------------
loans[, what_happened := fifelse(screen == "denial",
    sprintf("DENIED despite %.1f%% predicted denial risk",
            100 * (1 - resid)),
  fifelse(screen == "withdrawal",
    sprintf("WITHDREW despite %.1f%% predicted withdrawal risk",
            100 * (1 - resid)),
  fifelse(screen == "pricing",
    sprintf("Paid %.3f%% vs %.3f%% expected (+%.0f bp)",
            interest_rate, model_expected_rate, 100 * resid),
    "Placed in high-cost product tier despite prime-eligible profile")))]
loans[, surprise_level := fifelse(screen %in% c("denial", "withdrawal"),
    fcase(1 - resid <= 0.05, "EXTREME (<=5% expected)",
          1 - resid <= 0.15, "HIGH (<=15% expected)",
          default = "MODERATE"),
    fcase(resid >= 0.50, "EXTREME (50+ bp excess)",
          resid >= 0.25, "HIGH (25+ bp excess)",
          default = "MODERATE"))]
loans[screen == "pricing",
      excess_rate_dollars_yr := round(resid / 100 * loan_amount)]
loans[screen == "pricing" & !is.na(excess_other_costs_pct),
      excess_fees_dollars := round(excess_other_costs_pct / 100 * loan_amount)]
loans[, review_priority := frank(-resid, ties.method = "first"),
      by = .(lei, screen)]
setorder(loans, screen, -resid)
# (2) comparator variables NEXT TO their outlier counterparts
.pairs <- c("name", "screen", "loan_cat", "group", "uli", "review_priority",
            "what_happened", "surprise_level",
            "credit_score", "comp_credit_score", "dti", "comp_dti",
            "ltv_combined", "comp_ltv", "loan_amount", "comp_loan_amount",
            "interest_rate", "comp_interest_rate", "model_expected",
            "model_expected_rate", "excess_rate_dollars_yr",
            "excess_fees_dollars", "comp_outcome", "match_distance",
            "weaker_profile_comparator", "reason_plain", "risk_plain",
            "cu_stated_reason", "rebuttal_evidence")
setcolorder(loans, intersect(.pairs, names(loans)))
setcolorder(loans, c("name", "lei", "screen", "loan_cat", "group", "uli",
                     "resid"))
fwrite(loans, out("outlier_loans_2025.csv"))
fwrite(loans, out(sprintf("outlier_loans_%s_2025.csv", stream_tag)))
# ---- separate worksheets per screen: ALWAYS all three core screens ------------
# (an empty sheet with headers means "no outliers on this screen" -- a
#  missing sheet would be ambiguous)
sheet_screens <- union(c("denial", "withdrawal", "pricing"),
                       unique(loans$screen))
for (sc in sheet_screens)
  fwrite(loans[screen == sc],
         out(sprintf("outlier_loans_%s_sheet_2025.csv", sc)))
have_ox2      <- requireNamespace("openxlsx2", quietly = TRUE)
have_openxlsx <- requireNamespace("openxlsx",  quietly = TRUE)
have_writexl  <- requireNamespace("writexl",   quietly = TRUE)
if (have_ox2 || have_openxlsx || have_writexl) {
  readme <- data.table(
    column = c("name", "screen", "group", "uli", "resid", "model_expected",
               "model_expected_rate", "excess_other_costs_pct",
               "reason_plain", "risk_plain", "cu_stated_reason",
               "credit_score", "dti", "ltv_combined", "outlier_reason",
               "risk_factors_present", "stream"),
    what_it_means = c(
      "Credit union name",
      "Which review: denial, withdrawal, pricing, or steering",
      "Borrower group compared against white borrowers at the same CU",
      "The loan identifier (HMDA universal loan ID)",
      "How surprising the outcome was (bigger = more unexpected)",
      "The chance the model gave this adverse outcome (e.g. 0.05 = 5%); small numbers mean the outcome was very unexpected",
      "Pricing rows: the interest rate the model expected for this loan",
      "Pricing rows: extra fees beyond expectation (points - credits + costs, % of loan)",
      "PLAIN-LANGUAGE: why the model expected a good outcome for this borrower",
      "PLAIN-LANGUAGE: anything in the profile that pointed the other way",
      "The institution's own reported denial reason from its HMDA filing",
      "Borrower credit score used in underwriting",
      "Debt-to-income ratio (%)",
      "Combined loan-to-value (%)",
      "Technical version of reason_plain (model log-odds contributions)",
      "Technical version of risk_plain",
      "Which analysis stream produced this row (popick / ml)"))
  readme <- rbind(readme, data.table(
    column = c("pair", "row_type", "comp_uli", "match_distance",
               "weaker_profile_comparator"),
    what_it_means = c(
      "Links each outlier to the comparator row directly beneath it",
      "OUTLIER = the flagged loan; '-> comparator' = the matched white loan at the same CU/product with the good outcome",
      "Loan ID of the matched white comparator",
      "How similar the two profiles are (0 = identical; computed on score, DTI, CLTV, amount, income)",
      "1 = the comparator's profile was equal or WEAKER on every matched dimension -- the strongest exhibits")))
  readme <- rbind(readme, data.table(
    column = c("what_happened", "surprise_level", "review_priority",
               "excess_rate_dollars_yr", "excess_fees_dollars"),
    what_it_means = c(
      "One-sentence summary: the outcome vs what the model predicted",
      "How unexpected: EXTREME / HIGH / MODERATE",
      "1 = the most unexpected loan at that credit union for that screen -- start here",
      "Pricing: extra interest dollars PER YEAR implied by the rate excess on this loan",
      "Pricing: extra upfront fees in dollars (points - credits + costs beyond expectation)")))
  readme <- rbind(readme, data.table(
    column = "rebuttal_evidence",
    what_it_means = paste(
      "Tests the CU's stated denial reason against its OWN approvals:",
      "CONTRADICTED / QUESTIONABLE / consistent. Reasons HMDA cannot test",
      "get MODEL CONTEXT (predicted denial odds, top strength, comparator",
      "outcome). A denial with NO applicable reason is itself flagged --",
      "Reg B requires specific adverse-action reasons")))
  readme <- rbind(readme, data.table(
    column = "reason_plain",
    what_it_means = "Factors ranked 1) strongest to 4): why the model expected the GOOD outcome for this borrower"))
  # gather ALL streams whose tagged loan files exist (this run + prior runs)
  all_streams <- list()
  for (st in c("popick", "ml")) {
    f <- out(sprintf("outlier_loans_%s_2025.csv", st))
    if (st == stream_tag) all_streams[[st]] <- loans
    else if (file.exists(f))
      all_streams[[st]] <- fread(f, colClasses = list(character =
                                                      c("lei", "uli")))
  }
  .interleave <- function(x) {
    if (!nrow(x) || !"comp_uli" %in% names(x)) {
      if (nrow(x)) x[, `:=`(pair = .I, row_type = "OUTLIER")]
      return(x)
    }
    x[, pair := .I]
    x[, row_type := "OUTLIER"]
    cmp <- x[!is.na(comp_uli),
             .(pair, row_type = "-> comparator (white, good outcome)",
               name, lei, screen, loan_cat, group = "white", uli = comp_uli,
               credit_score = comp_credit_score, dti = comp_dti,
               ltv_combined = comp_ltv, loan_amount = comp_loan_amount,
               interest_rate = comp_interest_rate,
               reason_plain = fifelse(weaker_profile_comparator == 1L,
                 "COMPARATOR: same CU, same product, equal-or-WEAKER profile -- good outcome",
                 "COMPARATOR: same CU, same product, closest profile -- good outcome"),
               match_distance)]
    spacer <- x[, .(pair, row_type = "zz_spacer")]    # blank line per pair
    out <- rbind(x, cmp, spacer, fill = TRUE)
    setorder(out, pair, row_type)   # OUTLIER < "-> comparator" < zz_spacer
    for (cc in setdiff(names(out), "pair"))
      out[row_type == "zz_spacer", (cc) := NA]
    # comparator values live on their own row; comp_ columns are redundant
    dropc <- grep("^comp_", names(out), value = TRUE)
    if (length(dropc)) out[, (dropc) := NULL]
    setcolorder(out, c("pair", "row_type"))
    out
  }
  # ---- SUMMARY TAB: the examiner's landing page --------------------------------
  smy <- loans[, .(
      total_outliers   = .N,
      denials          = sum(screen == "denial"),
      withdrawals      = sum(screen == "withdrawal"),
      pricing          = sum(screen == "pricing"),
      steering         = sum(screen == "steering"),
      extreme_cases    = sum(grepl("^EXTREME", surprise_level)),
      contradicted_reasons = if ("rebuttal_evidence" %in% names(loans))
        sum(grepl("^CONTRADICTED", rebuttal_evidence)) else 0L,
      weaker_profile_pairs = sum(weaker_profile_comparator %in% 1L),
      excess_dollars_yr = sum(excess_rate_dollars_yr, na.rm = TRUE) +
                          sum(excess_fees_dollars, na.rm = TRUE),
      groups_affected  = paste(sort(unique(group)), collapse = ", ")),
    by = .(lei, name)]
  fl_cu <- flags[, .(minority_records = sum(n_g, na.rm = TRUE),
                     high_tier_cells = sum(tier == "high", na.rm = TRUE),
                     strongest_q = suppressWarnings(min(q, na.rm = TRUE))),
                 by = lei]
  smy <- merge(smy, fl_cu, by = "lei", all.x = TRUE)
  smy <- merge(smy, assets[, .(lei, cu_type, assets_tot)], by = "lei",
               all.x = TRUE)
  smy[, outliers_per_1000 := round(1000 * total_outliers /
                                   pmax(minority_records, 1), 1)]
  if (file.exists(out("ensemble_rankings_2025.csv"))) {
    er <- fread(out("ensemble_rankings_2025.csv"),
                colClasses = list(character = "lei"))
    smy <- merge(smy, er[, .(lei, robust_loans)], by = "lei", all.x = TRUE)
  }
  smy[, charter := c("0" = "Unknown", "1" = "Federal",
                     "2" = "State")[as.character(cu_type)]]
  smy[, assets_B := round(assets_tot / 1e9, 2)]
  setorder(smy, -total_outliers)
  smy[, rank_overall := .I]
  smy[, rank_in_charter := frank(-total_outliers, ties.method = "first"),
      by = cu_type]
  keepc <- intersect(c("rank_overall", "rank_in_charter", "name", "charter",
                       "assets_B", "total_outliers", "denials",
                       "withdrawals", "pricing", "steering",
                       if ("robust_loans" %in% names(smy)) "robust_loans",
                       "outliers_per_1000", "extreme_cases",
                       "contradicted_reasons", "weaker_profile_pairs",
                       "excess_dollars_yr", "high_tier_cells",
                       "strongest_q", "groups_affected"), names(smy))
  smy <- smy[, ..keepc]
  fwrite(smy, out("outlier_summary_by_cu_2025.csv"))
  cat("Summary by CU ->", out("outlier_summary_by_cu_2025.csv"), "\n")
  readme <- rbind(readme, data.table(
    column = c("SUMMARY: outliers_per_1000", "SUMMARY: extreme_cases",
               "SUMMARY: contradicted_reasons", "SUMMARY: excess_dollars_yr",
               "SUMMARY: strongest_q"),
    what_it_means = c(
      "Outlier intensity: loans for review per 1,000 minority records tested -- compares institutions of different sizes fairly",
      "Loans where the model gave the adverse outcome <=5% chance (or 50+ bp pricing excess)",
      "Denials where the CU's stated reason is contradicted by its OWN approved white borrowers",
      "Pricing rows: total excess interest per year + excess fees, in dollars",
      "The single most significant statistical result at this CU (smaller = stronger evidence)")))
  sheets <- list(ReadMe = readme, Summary = smy)
  for (st in names(all_streams))
    for (sc in sheet_screens) {
      nmx <- tools::toTitleCase(paste(st, sc))
      sheets[[substr(nmx, 1, 31)]] <-
        .interleave(copy(all_streams[[st]][screen == sc]))
    }
  wrote_styled <- FALSE
  if (have_ox2) {
    wrote_styled <- tryCatch({
      wb <- openxlsx2::wb_workbook()
      for (nm in names(sheets)) {
        wb <- openxlsx2::wb_add_worksheet(wb, nm)
        wb <- openxlsx2::wb_add_data(wb, sheet = nm, x = sheets[[nm]])
        if ("row_type" %in% names(sheets[[nm]])) {
          rt <- sheets[[nm]]$row_type
          cc <- which(!is.na(rt) & startsWith(rt, "->")) + 1L
          nc <- ncol(sheets[[nm]])
          for (r in cc)
            wb <- openxlsx2::wb_add_fill(wb, sheet = nm,
                    dims = openxlsx2::wb_dims(rows = r, cols = seq_len(nc)),
                    color = openxlsx2::wb_color(hex = "FFDCE9F7"))
        }
      }
      openxlsx2::wb_save(wb, out("outlier_loans_2025.xlsx"),
                         overwrite = TRUE)
      TRUE
    }, error = function(e) {
      cat("!! openxlsx2 save FAILED:", conditionMessage(e),
          "\n   (trying openxlsx / writexl next)\n"); FALSE })
    if (wrote_styled)
      cat("STYLED workbook via openxlsx2 (comparators shaded) ->",
          out("outlier_loans_2025.xlsx"), "\n")
  }
  if (!wrote_styled && have_openxlsx) {
    wb <- openxlsx::createWorkbook()
    st_out <- openxlsx::createStyle(textDecoration = "bold")
    st_cmp <- openxlsx::createStyle(fgFill = "#DCE9F7")
    for (nm in names(sheets)) {
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeData(wb, nm, sheets[[nm]])
      if ("row_type" %in% names(sheets[[nm]])) {
        rt <- sheets[[nm]]$row_type
        oc <- which(!is.na(rt) & rt == "OUTLIER") + 1L
        cc <- which(!is.na(rt) & startsWith(rt, "->")) + 1L
        nc <- ncol(sheets[[nm]])
        if (length(oc)) openxlsx::addStyle(wb, nm, st_out, rows = oc,
              cols = seq_len(nc), gridExpand = TRUE, stack = TRUE)
        if (length(cc)) openxlsx::addStyle(wb, nm, st_cmp, rows = cc,
              cols = seq_len(nc), gridExpand = TRUE, stack = TRUE)
      }
    }
    wrote_styled <- tryCatch({
      openxlsx::saveWorkbook(wb, out("outlier_loans_2025.xlsx"),
                             overwrite = TRUE); TRUE },
      error = function(e) {
        cat("!! openxlsx save FAILED:", conditionMessage(e),
            "\n   (falling back to writexl if available)\n"); FALSE })
    if (wrote_styled)
      cat("STYLED workbook (outliers bold, comparators shaded) ->",
          out("outlier_loans_2025.xlsx"), "\n")
  }
  if (!wrote_styled && have_writexl) {
    writexl::write_xlsx(sheets, out("outlier_loans_2025.xlsx"))
    cat("Workbook (plain; install.packages('openxlsx') for shading) ->",
        out("outlier_loans_2025.xlsx"), "\n")
  }
  if (!wrote_styled && !have_writexl)
    cat("!! WORKBOOK NOT WRITTEN: openxlsx failed and writexl is not",
        "installed. Run install.packages('writexl') and rerun.\n")
  if (wrote_styled || have_writexl) {
    cat("Sheets written:", paste(names(sheets), collapse = ", "), "\n")
    cat("==> OPEN THIS FILE IN EXCEL:", out("outlier_loans_2025.xlsx"),
        "\n    (the .csv files are flat exports; the WORKBOOK with",
        "Summary/ReadMe/per-screen tabs is the .xlsx)\n")
  }
} else cat("(NO EXCEL WRITER INSTALLED -- only flat CSVs were produced.\n",
           "  Run:  install.packages('openxlsx')   (styled workbook)\n",
           "  or:   install.packages('writexl')    (plain workbook)\n",
           "  then rerun this script for the multi-tab .xlsx.)\n")
cat(sprintf("\nStage-2 file: %s outlier loans with underwriting fields -> %s\n",
            format(nrow(loans), big.mark = ","), out("outlier_loans_2025.csv")))
cat("Per-screen breakdown of exported loan IDs:\n")
print(loans[, .(loans = .N, cus = uniqueN(lei)), by = .(screen, loan_cat)])
