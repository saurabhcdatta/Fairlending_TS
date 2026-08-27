# =============================================================================
# 9_smoke_check.R  (DEV chain)  --  prove the dev chain works end to end.
#
# Builds a fake 2025 .dta (60k rows, 30 credit unions), PLANTS one CU that
# denies black applicants far too often (CU07, federal) and one that
# overprices hispanic borrowers (CU12, state), runs 2 -> 7 unchanged, then
# the validation harness (8) with trimmed knobs, and checks:
#   * the planted CUs land in the HIGH tier on the right screen/group
#   * no more than a couple of clean CUs are flagged by chance
#   * excess calibration, evidence columns, workbook contract, restoration
# Run from this folder:  Rscript 9_smoke_check.R
# (fake paths are set directly in the environment and restored afterwards --
# your 0_settings.R and any values from it in your session are untouched)
# =============================================================================

library(haven); library(data.table)
set.seed(42)

# --- fake raw file -------------------------------------------------------------
n <- 60000
cu  <- sample(1:30, n, replace = TRUE,                    # 30 CUs,
              prob = seq(3, 0.3, length.out = 30))        # UNEQUAL sizes
grp <- sample(c("white", "black", "hispanic", "asian"), n, replace = TRUE,
              prob = c(.6, .15, .15, .10))
score <- round(rnorm(n, 705, 60)); dti <- pmin(pmax(rnorm(n, 36, 9), 5), 60)
loan_amt <- round(exp(rnorm(n, 12.2, .5)))
# TWO PRICING MENUS (real lenders have product tiers): 15% baseline land in
# the high-cost menu (+60bp rate, +2% of loan in costs). CU12 ROUTES its
# hispanic borrowers there at +25pp -- the steering plant. This also adds to
# CU12's rate-level gap, which the pricing screen tests separately.
menu <- rbinom(n, 1, 0.15 + 0.25 * (cu == 12 & grp == "hispanic"))
raw <- data.frame(
  cu_number    = ifelse(runif(n) < 0.9, cu, NA),          # 10% non-CU rows
  lei          = sprintf("LEI%02d", cu),
  uli          = sprintf("U%06d", 1:n),
  loan_amt     = loan_amt,
  income       = round(exp(rnorm(n, 4.4, .6))),
  credit_score_app = score,
  credit_score_co  = NA_real_,
  app_race1    = fcase(grp == "black", 3, grp == "asian", 2, default = 5),
  app_race2    = NA_real_,
  app_ethnic1  = fifelse(grp == "hispanic", 1, 2),
  app_ethnic2  = NA_real_,
  gender_app   = sample(1:2, n, TRUE),
  debt_to_inc  = dti,
  property_county_fip = sprintf("%05d", sample(1:12, n, TRUE)),
  property_state = sample(c("CA", "TX", "NY", "VA"), n, TRUE),
  denial1      = ifelse(runif(n) < .7, sample(c(1, 3, 4, 5), n, TRUE),
                        ifelse(runif(n) < .5, 10, NA)),  # incl. code 10
  loan_purpose = sample(c(1, 31, 32), n, TRUE),
  loan_type    = 1, occupancy_type = 1,
  ln_term      = 360, intro_rate_period = NA_real_,       # 30yr fixed
  lien         = 1, tot_units = 1, construction_method = 1,
  property_value = round(loan_amt / 0.8),
  ltv_combined = pmin(round(100 * loan_amt / (loan_amt / 0.8), 1) +
                        sample(0:15, n, TRUE), 99),
  auto_undrwrting_sys1  = sample(c(1, 2, 6), n, TRUE),
  application_submission = sample(1:2, n, TRUE),
  discount_points = round(runif(n, 0, 0.02) * loan_amt),
  lender_credits  = round(runif(n, 0, 0.005) * loan_amt),
  tot_ln_costs    = round(runif(n, 0.01, 0.04) * loan_amt +
                          0.02 * loan_amt * menu),
  action_date  = as.Date("2025-01-01") + sample(0:360, n, TRUE),
  stringsAsFactors = FALSE)

# outcomes driven ONLY by legitimate factors...
p_deny <- plogis(-1.2 - 0.02 * (score - 700) + 0.05 * (dti - 36))
# ...except at CU 7: +18pp denial for black applicants (the plant)
p_deny <- p_deny + ifelse(cu == 7 & grp == "black", 0.18, 0)
denied <- rbinom(n, 1, pmin(p_deny, 0.95))
raw$action_type <- ifelse(denied == 1, 3, sample(c(1, 1, 1, 4), n, TRUE))
spread <- 0.4 - 0.004 * (score - 700) + 0.01 * (dti - 36) + rnorm(n, 0, 0.3)
spread <- spread + ifelse(cu == 12 & grp == "hispanic", 0.35, 0)  # plant 2
raw$rate_spread <- ifelse(raw$action_type == 1, round(spread + 0.6 * menu, 3), NA)
# sprinkle covariate NAs so fixest DROPS observations during estimation --
# reproduces the real-data condition that once turned every gap NA
raw$debt_to_inc[sample(n, floor(0.02 * n))]   <- NA
raw$ltv_combined[sample(n, floor(0.02 * n))]  <- NA
raw$income[sample(n, floor(0.02 * n))]        <- NA   # -> income_bin NA ->
# fixest itself removes these rows at fit time (pricing controls), the exact
# real-data mechanism that produced NA-poisoned gaps
raw$interest_rate <- ifelse(raw$action_type == 1,
                            round(6.5 + spread + 0.6 * menu, 3), NA)

dir.create("/tmp/fake_share", showWarnings = FALSE)
# plant ULI duplicates: 2 exact copies + 1 conflicting refile (action flips)
raw <- rbind(raw, raw[c(101, 202), ])                 # exact duplicates
refile <- raw[303, ]; refile$action_type <- ifelse(refile$action_type == 1, 3, 1)
raw <- rbind(raw, refile)                             # conflicting duplicate
write_dta(raw, "/tmp/fake_share/all_agency_hmda_2025_test.dta")

# --- point the chain at the fake data --------------------------------------------
# Scripts 1-8 auto-source 0_settings.R ONLY when `out` is absent, so defining
# the fake paths here (globally) is all the redirection needed. Snapshot any
# real values first and restore them in finally, so an interactive session
# keeps its configuration even if the smoke check dies halfway.
.set_names <- c("raw_dta", "oce_file", "xwalk_file", "work_dir", "out")
.old_set <- mget(.set_names, envir = globalenv(), ifnotfound = list(NULL))
tryCatch({
# deliberately mixed casing: the real xwalk codes it LEI (uppercase), and the
# OCE pull's casing is not guaranteed either -- 00_assets.R must resolve both
oce_df <- data.frame(join_number = rep(1:30, 2),
                     CU_Name = sprintf("TEST CU %02d", rep(1:30, 2)),
                     Assets_Tot = rep(seq(3e9, 1e8, length.out = 30), 2),
                     CU_Type = rep(ifelse(1:30 %in% 28:29, 3L,      # 2 non-fed-insured
                                   ifelse(1:30 %% 3 == 0, 2L, 1L)), 2),
                     q_period_num = rep(c(2025.4, 2024.4), each = 30))
# CU12 (pricing plant) -> cu_type 2 (12 %% 3 == 0); CU07 (denial) -> type 1
haven::write_dta(oce_df, "/tmp/fake_share/oce.dta")
haven::write_dta(data.frame(Join_Number = 1:30, LEI = sprintf("LEI%02d", 1:30)),
                 "/tmp/fake_share/xwalk.dta")
raw_dta    <<- "/tmp/fake_share/all_agency_hmda_2025_test.dta"
oce_file   <<- "/tmp/fake_share/oce.dta"
xwalk_file <<- "/tmp/fake_share/xwalk.dta"
work_dir   <<- file.path(tempdir(), "hmda_work")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
out        <<- function(name) file.path(work_dir, name)

# fabricate the three reference files the chain expects (1_reference.R
# builds the real ones on the workstation; here we make small consistent fakes)
fwrite(data.table(date = seq(as.Date("2024-12-01"), as.Date("2025-12-31"), by = 7),
                  pmms = 6.5), out("pmms_weekly.csv"))
fwrite(data.table(fips = sprintf("%05d", 1:12),
                  one_unit_limit = c(rep(806500, 10), 300000, 300000)),
       out("jumbo_2025.csv"))
fwrite(data.table(fips = sprintf("%05d", 1:12),
                  cbsa = c(rep("C100", 6), rep("C200", 3), rep("", 3)),
                  metro = c(rep(1L, 6), rep(1L, 3), rep(0L, 3))),
       out("msa_crosswalk.csv"))

fwrite(data.table(lei = "LEISTALE", cu_number = 1,
                  name = "STALE ROW", assets_tot = 1),
       out("cu_assets_2025.csv"))   # stale pre-cu_type file: must be rebuilt
for (s in c("2_assets.R", "3_extract.R", "4_prepare.R", "5_models.R",
            "6_screen.R", "7_rank.R")) {
  cat("\n========", s, "========\n"); source(s, local = new.env())
}

# --- verdict -----------------------------------------------------------------------
flags <- fread(out("flags_2025.csv"))
hit_denial  <- flags[screen == "denial"  & lei == "LEI07" & group == "black",   flag] == 1
hit_pricing <- flags[screen == "pricing" & lei == "LEI12" & group == "hispanic", flag] == 1
stopifnot(flags[screen == "denial"  & lei == "LEI07" & group == "black", tier] == "high",
          flags[screen == "pricing" & lei == "LEI12" & group == "hispanic", tier] == "high")
cat("planted violators land in the HIGH tier: OK\n")
false_pos   <- flags[flag == 1 & !(lei == "LEI07" & group == "black") &
                                 !(lei == "LEI12" & group == "hispanic"), .N]
cat(sprintf("\nPlanted denial CU flagged:  %s\nPlanted pricing CU flagged: %s\nFalse positives: %d\n",
            hit_denial, hit_pricing, false_pos))
stopifnot(hit_denial, hit_pricing, false_pos <= 2)
dg <- fread(out("denial_gaps_2025.csv"))
pg <- fread(out("pricing_gaps_2025.csv"))
stopifnot(dg[n >= 200, all(!is.na(gap) & !is.na(p))],
          pg[component == "interest_rate" & n >= 200, all(!is.na(gap))])
cat("residual gaps compute despite dropped observations: OK\n")
stopifnot(file.exists(out("regression_output_2025.pdf")),
          file.size(out("regression_output_2025.pdf")) > 20000)

rk <- fread(out("outlier_rankings_2025.csv"))
stopifnot("cu_type" %in% names(rk),
          rk[lei == "LEI07", cu_type] == 1, rk[lei == "LEI12", cu_type] == 2,
          rk[lei == "LEI07", rank] == 1,    # rank 1 WITHIN federal CUs
          rk[lei == "LEI12", rank] == 1)    # rank 1 WITHIN state CUs
cat("type-split verified: plants rank #1 within their own cu_type\n")

ol <- fread(out("outlier_loans_2025.csv"))
stopifnot(rk[rank == 1, all(lei %in% c("LEI07", "LEI12"))],
          ol[lei == "LEI07" & screen == "denial", .N] >= 20,
          # excess calibration: exported count ~ ceiling(eb_gap x n_g)
          {fl <- fread(out("flags_2025.csv"));
           ex <- fl[lei == "LEI07" & screen == "denial" & group == "black",
                    ceiling(eb_gap * n_g)];
           abs(ol[lei == "LEI07" & screen == "denial" & group == "black", .N] -
               ex) <= 1},
          all(c("uli", "credit_score", "dti", "resid", "model_expected",
                "cu_stated_reason") %in% names(ol)),
          ol[screen == "denial", all(model_expected < 0.5)],
          "outlier_reason" %in% names(ol),
          ol[screen == "denial",
             mean(!is.na(outlier_reason) & nzchar(outlier_reason))] > 0.4,
          ol[screen == "denial",
             mean(!is.na(reason_plain) & nzchar(reason_plain))] > 0.4,
          ol[nzchar(reason_plain), all(grepl("^1\\)", reason_plain))],
          all(c("what_happened", "surprise_level", "review_priority")
              %in% names(ol)),
          which(names(ol) == "comp_credit_score") ==
            which(names(ol) == "credit_score") + 1L,
          ol[screen == "denial", all(grepl("^DENIED despite", what_happened))],
          "rebuttal_evidence" %in% names(ol),
          file.exists(out("outlier_summary_by_cu_2025.csv")),
          file.exists(out("outlier_summary_denial_2025.csv")),
          file.exists(out("outlier_summary_pricing_2025.csv")),
          ol[screen == "pricing", all(grepl("^PRICE CASE", rebuttal_evidence))],
          ol[screen == "withdrawal",
             all(grepl("^WITHDRAWAL CASE", rebuttal_evidence))],
          {sm <- fread(out("outlier_summary_by_cu_2025.csv"));
           all(c("outliers_per_1000","contradicted_reasons",
                 "rank_in_charter") %in% names(sm)) &&
           sm[rank_overall == 1, grepl("TEST CU", name)]},
          ol[grepl("^CONTRADICTED|^QUESTIONABLE", rebuttal_evidence), .N] >= 1,
          ol[screen == "denial" & denial_reason1 %in% 10,
             all(grepl("NO SPECIFIC REASON REPORTED", rebuttal_evidence))],
          ol[screen == "denial" & is.na(denial_reason1),
             all(grepl("NO SPECIFIC REASON REPORTED", rebuttal_evidence))],
          ol[screen == "denial", all(nzchar(rebuttal_evidence))],
          "comp_uli" %in% names(ol),
          ol[screen == "denial", mean(!is.na(comp_uli))] > 0.8,
          ol[!is.na(comp_uli), all(weaker_profile_comparator %in% 0:1)],
          ol[screen == "pricing",
             all(!is.na(model_expected_rate) & interest_rate > model_expected_rate)],
          all(grepl("^U", ol$uli)))
cat("outlier ranking + evidence columns verified\n")
# Phase-1 contract: workbook XOR flat sheet CSVs (fallback-only)
{wbx <- file.exists(out("outlier_loans_2025.xlsx"))
 shx <- vapply(c("denial", "withdrawal", "pricing"), function(sc)
   file.exists(out(sprintf("outlier_loans_%s_sheet_2025.csv", sc))),
   logical(1))
 stopifnot(if (wbx) !any(shx) else all(shx))}
cat("workbook contract (xlsx XOR fallback sheet CSVs) verified\n")

# reference joins actually landed:
a <- readRDS(out("analysis_2025.rds"))
stopifnot(all(!is.na(a$pmms)),
          abs(mean(a$rate_over_pmms[a$priced == 1], na.rm = TRUE) -
              mean(a$rate_spread[a$priced == 1], na.rm = TRUE)) < 0.05,
          mean(a$jumbo[a$fips %in% sprintf("%05d", 11:12)]) >
            mean(a$jumbo[!a$fips %in% sprintf("%05d", 11:12)]))
cat("reference joins verified: pmms / jumbo / msa\n")
# cu_type-3 CUs (LEI28/LEI29) must be gone from the ANALYSIS and the screen
stopifnot(!anyDuplicated(a[, .(lei, uli)]),  # dedupe key = lender x loan id
          !any(a$lei %in% c("LEI28", "LEI29")),
          !any(flags$lei %in% c("LEI28", "LEI29")))
cat("non-federally insured (type 3) dropped from analysis + screen: OK\n")
stopifnot(all(!is.na(flags$name)),
          flags[lei == "LEI07" & screen == "denial" & group == "black",
                name] == "TEST CU 07")
cat("assets universe + CU names verified\n")

# --- validation harness (trimmed knobs) --------------------------------------------
orig_nflag <- fread(out("flags_2025.csv"))[, sum(flag)]
Sys.setenv(VAL_B_PLACEBO = 2, VAL_INJECT_GAPS = "0.10", VAL_INJECT_CELLS = 3)
source("8_validation.R")
Sys.unsetenv(c("VAL_B_PLACEBO", "VAL_INJECT_GAPS", "VAL_INJECT_CELLS"))
pw <- fread(out("validation_power_2025.csv"))
stopifnot(mean(plac) <= max(2, 0.3 * n_real),        # placebo ~ quiet
          pw[gap_pp == 10, detected >= 1],
          file.exists(out("validation_evalues_2025.csv")))
fl_after <- fread(out("flags_2025.csv"))
stopifnot(identical(fl_after[, sum(flag)], orig_nflag))  # originals restored
cat("validation harness: placebo quiet, injection detected, originals restored\n")

cat("\nDEV SMOKE CHECK PASSED\n")
}, finally = {
  for (nm in .set_names)                 # put the session back how we found it
    if (is.null(.old_set[[nm]])) {
      if (exists(nm, envir = globalenv(), inherits = FALSE))
        rm(list = nm, envir = globalenv())
    } else assign(nm, .old_set[[nm]], envir = globalenv())
})
