# =============================================================================
# 02_prepare.R  --  turn the raw CU extract into a small, clean analysis table.
#
# One decision to make here, once: the RENAME MAP. The raw file's column names
# are the release's names, not necessarily the tidy ones we want. Run
# names(readRDS(out("hmda_cu_2025.rds"))) (or step 01's printout), then fill in
# the RIGHT-hand side of `rename` below with what the file actually uses.
# The left-hand side is what every later script relies on -- never changes.
#
# Output:  analysis_2025.rds  (one row per application, only the columns used)
# =============================================================================

library(data.table)

source("settings.R")

src <- out("hmda_cu_2025.rds")
if (!file.exists(src))
  stop("Not found: ", src,
       "\nRun 01_extract.R first, and confirm settings.R points work_dir at",
       " the real project folder (an interrupted smoke_test used to leave it",
       " pointing at a TEMPORARY folder that vanishes when R restarts --",
       " check the '[settings] outputs go to:' line printed above).",
       call. = FALSE)
raw <- as.data.table(readRDS(src))
setnames(raw, tolower(names(raw)))

# --- 1. rename map: our_name = candidate raw names, FIRST ONE PRESENT WINS -----
# Confirmed against the real 2025 file (2026-07-06): gender_app (not app_sex),
# property_county_fip (not county_code); the rest matched. Extra candidates
# stay listed so a future release rename cannot break this silently.
rename <- list(
  lei           = c("lei", "legal_entity_identifier"),
  uli           = c("uli", "universal_loan_identifier"),
  cu_number     = c("cu_number"),
  action_type   = c("action_type"),
  loan_amount   = c("loan_amt", "loan_amount"),
  income        = c("income"),
  credit_score  = c("credit_score_app", "applicant_credit_score"),
  race1         = c("app_race1"),
  ethnicity1    = c("app_ethnic1"),
  sex           = c("gender_app", "app_sex", "sex"),
  dti           = c("debt_to_inc", "dti"),
  county        = c("property_county_fip", "property_county_fips",
                    "county_code", "county"),
  loan_purpose  = c("loan_purpose"),
  loan_type     = c("loan_type"),
  occupancy     = c("occupancy_type", "occupancy"),
  action_date   = c("action_date"),
  rate_spread   = c("rate_spread"),
  interest_rate = c("interest_rate"),
  # --- Popick controls / taxonomy / pricing components ---
  race2         = c("app_race2"),
  ethnicity2    = c("app_ethnic2"),
  credit_score_co = c("credit_score_co"),
  ltv_combined  = c("ltv_combined"),
  ln_term       = c("ln_term", "loan_term"),
  property_value = c("property_value"),
  intro_rate_period = c("intro_rate_period"),
  aus1          = c("auto_undrwrting_sys1"),
  app_submission = c("application_submission"),
  lien          = c("lien", "lien_status"),
  tot_units     = c("tot_units", "total_units"),
  construction  = c("construction_method"),
  disc_points   = c("discount_points"),
  denial_reason1 = c("denial1"),
  denial_reason2 = c("denial2"),
  denial_reason3 = c("denial3"),
  lender_credits = c("lender_credits"),
  loan_costs    = c("tot_ln_costs", "total_loan_costs"),
  property_state = c("property_state", "state")
)
# optional columns: carried when present, silently skipped when absent
optional_cols <- c("denial_reason2", "denial_reason3")
found <- vapply(rename, function(cands) {
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}, character(1))
miss_opt <- intersect(names(found)[is.na(found)], optional_cols)
if (length(miss_opt)) {
  cat("(optional columns not in this file, skipped:",
      paste(miss_opt, collapse = ", "), ")\n")
  found <- found[!names(found) %in% miss_opt]
}
if (anyNA(found)) {
  bad <- names(found)[is.na(found)]
  sugg <- vapply(bad, function(b) {
    frags <- setdiff(unique(c(b, unlist(strsplit(rename[[b]], "_")))),
                     c("app", "code", "type"))
    frags <- frags[nchar(frags) >= 3]
    hits <- unique(unlist(lapply(frags, grep, x = names(raw),
                                 value = TRUE, ignore.case = TRUE)))
    paste0("  ", b, "  ~  {",
           if (length(hits)) paste(head(hits, 8), collapse = ", ")
           else "no lookalikes found", "}")
  }, character(1))
  stop("No candidate found in the raw file for: ", paste(bad, collapse = ", "),
       "\nRaw columns that look related:\n", paste(sugg, collapse = "\n"),
       "\nAdd the true name FIRST in that entry of the rename list, rerun.",
       call. = FALSE)
}
cat("Rename map resolved:\n")
print(data.frame(ours = names(found), raw = unname(found)), row.names = FALSE)
dat <- raw[, .SD, .SDcols = unname(found)]
setnames(dat, unname(found), names(found))

# --- 2. types -------------------------------------------------------------------
num_cols <- c("loan_amount", "income", "credit_score", "dti", "rate_spread",
              "interest_rate", "action_type", "race1", "ethnicity1", "sex",
              "loan_purpose", "loan_type", "occupancy",
              "race2", "ethnicity2", "credit_score_co", "ltv_combined",
              "ln_term", "property_value", "intro_rate_period", "aus1",
              "app_submission", "lien", "tot_units", "construction",
              "disc_points", "lender_credits", "loan_costs")
dat[, (num_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
    .SDcols = num_cols]

# --- 3. outcomes from HMDA action_type codes --------------------------------------
# 1 originated | 2 approved, not accepted | 3 denied | 4 withdrawn | 5 incomplete
dat[, denied    := as.integer(action_type == 3)]
dat[, withdrawn := as.integer(action_type %in% c(4, 5))]
dat[, in_denial_universe     := action_type %in% 1:3]   # lender decisioned it
dat[, in_withdrawal_universe := action_type %in% 1:5]
dat[, priced := as.integer(action_type == 1 & !is.na(rate_spread))]

# --- 4. race/ethnicity: Popick fn.10 via fields 1-2, FFIEC subcodes ------------
# hispanic (any race) if either ethnicity field is Hispanic (1, 11-14); else
# classify each race field (asian 2/21-27, black 3, nhpi 4/41-44, aian 1,
# white 5); two DIFFERENT minority races -> multi -> excluded; white+minority
# -> the minority; white requires an explicit not-Hispanic ethnicity.
.rc <- function(code) fcase(code %in% c(2, 21:27), "asian",
                            code == 3,             "black",
                            code %in% c(4, 41:44), "nhpi",
                            code == 1,             "aian",
                            code == 5,             "white",
                            default = NA_character_)
dat[, hisp := (ethnicity1 %in% c(1, 11:14)) | (ethnicity2 %in% c(1, 11:14))]
dat[, `:=`(r1 = .rc(race1), r2 = .rc(race2))]
dat[, group := fcase(
  hisp,                                              "hispanic",
  !is.na(r1) & !is.na(r2) & r1 != r2 & r1 == "white", r2,
  !is.na(r1) & !is.na(r2) & r1 != r2 & r2 == "white", r1,
  !is.na(r1) & !is.na(r2) & r1 != r2,                 NA_character_,  # multi
  !is.na(r1) | !is.na(r2),                            fcoalesce(r1, r2),
  default = NA_character_)]
dat[group == "white" & !(ethnicity1 %in% 2 | ethnicity2 %in% 2),
    group := NA_character_]
dat[, pclass := relevel(factor(group,
       levels = c("white", "black", "hispanic", "asian", "aian", "nhpi")),
       ref = "white")]
dat[, c("hisp", "r1", "r2") := NULL]

# --- 5. Popick sample + derived controls -------------------------------------------
n0 <- nrow(dat)
dat <- dat[!is.na(group)]                                   # known race/ethnicity
dat <- dat[sex %in% 1:2]                                    # Popick keeps F/M
dat <- dat[loan_amount > 0]

# credit score: max of valid applicant/co-applicant scores (SAS rule)
.okcs <- function(x) !is.na(x) & x > 300 & x < 900
dat[, credit_score := pmax(fifelse(.okcs(credit_score), credit_score, NA_real_),
                           fifelse(.okcs(credit_score_co), credit_score_co,
                                   NA_real_), na.rm = TRUE)]
dat[is.infinite(credit_score), credit_score := NA_real_]

# AUS / broker / fixed-rate / additional-lien (SAS definitions)
dat[, aus        := as.integer(!is.na(aus1) & !(aus1 %in% c(1111, 6)))]
dat[, broker     := as.integer(app_submission %in% 2)]
dat[, fixed_rate := as.integer(is.na(intro_rate_period))]
dat[, other_lien := as.integer(
      !is.na(property_value) & property_value > 0 & loan_amount > 0 &
      !is.na(ltv_combined) &
      round(100 * loan_amount / property_value, 1) < round(ltv_combined, 1))]
dat[, year_month := format(action_date, "%Y-%m")]

# EARLY_BANKRUPT indicator (from the production SAS: CS<600 & DTI>40 & CLTV>90)
dat[, early_bankrupt := as.integer(!is.na(credit_score) & credit_score < 600 &
                                   !is.na(dti) & dti > 40 &
                                   !is.na(ltv_combined) & ltv_combined > 90)]
cat(sprintf("Kept %s of %s rows after filters (%.1f%%)\n",
            format(nrow(dat), big.mark = ","), format(n0, big.mark = ","),
            100 * nrow(dat) / n0))
print(dat[, .N, by = group][order(-N)])
cat(sprintf("Denial rate (decisioned apps): %.1f%% | withdrawn: %.1f%%\n",
            100 * dat[in_denial_universe == TRUE, mean(denied)],
            100 * dat[in_withdrawal_universe == TRUE, mean(withdrawn)]))

# --- 6. reference joins: PMMS benchmark, jumbo flag, MSA -----------------------------
for (rf in c("pmms_weekly.csv", "jumbo_2025.csv", "msa_crosswalk.csv"))
  if (!file.exists(out(rf)))
    stop(rf, " not found in ", work_dir, " -- run 00_reference.R first.")

# county code -> 5-digit fips, whether stored as text or number
dat[, fips := {x <- trimws(as.character(county))
               fifelse(grepl("^[0-9]{1,5}$", x),
                       sprintf("%05d", suppressWarnings(as.integer(x))), x)}]

# PMMS: latest weekly rate at or before the action date (rolling join),
# giving each loan a market benchmark: rate_over_pmms
pmms <- fread(out("pmms_weekly.csv"))
pmms[, date := as.Date(date)]
setorder(pmms, date)
dat[, pmms := pmms[dat, x.pmms, on = .(date = action_date), roll = TRUE]]
dat[, rate_over_pmms := interest_rate - pmms]

# FHFA limits: jumbo = above the county one-unit conforming limit
# (2025 baseline $806,500 where the county is missing from the file)
jmb <- fread(out("jumbo_2025.csv"), colClasses = list(character = "fips"))
dat[jmb, on = "fips", one_unit_limit := i.one_unit_limit]
dat[, jumbo := as.integer(loan_amount > fifelse(is.na(one_unit_limit),
                                                806500, one_unit_limit))]

# MSA crosswalk: CBSA id + metro flag (non-matched counties = non-metro)
msa <- fread(out("msa_crosswalk.csv"), colClasses = list(character = c("fips", "cbsa")))
dat[msa, on = "fips", `:=`(cbsa = i.cbsa, metro = i.metro)]
dat[is.na(metro), metro := 0L]
dat[is.na(cbsa), cbsa := paste0("nonmetro_", substr(fips, 1, 2))]

cat(sprintf(paste0("Reference joins: pmms matched %.1f%% | county limit found ",
                   "%.1f%% | metro share %.1f%%\n"),
            100 * mean(!is.na(dat$pmms)), 100 * mean(!is.na(dat$one_unit_limit)),
            100 * mean(dat$metro)))
cat(sprintf("Jumbo share: %.1f%%  (2025 benchmark from the prior run: ~5.9%%)\n",
            100 * mean(dat$jumbo)))

# --- 7. loan-category taxonomy (Popick Types 1-10; requires the jumbo flag) --------
# All types require: 360-mo term, fixed rate, 1-4 units, owner-occupied
# principal residence, first lien, site-built.
dat[, type := fcase(
  ln_term != 360 | fixed_rate != 1 | !tot_units %in% 1:4 | occupancy != 1 |
    lien != 1 | construction != 1,                    NA_integer_,
  loan_type == 1 & loan_purpose == 1  & jumbo == 0,  1L,
  loan_type == 2 & loan_purpose == 1,                2L,
  loan_type == 3 & loan_purpose == 1,                3L,
  loan_type == 1 & loan_purpose == 31 & jumbo == 0,  4L,
  loan_type == 2 & loan_purpose == 31,               5L,
  loan_type == 1 & loan_purpose == 32 & jumbo == 0,  6L,
  loan_type == 2 & loan_purpose == 32,               7L,
  loan_type == 1 & loan_purpose == 1  & jumbo == 1,  8L,
  loan_type == 1 & loan_purpose == 31 & jumbo == 1,  9L,
  loan_type == 1 & loan_purpose == 32 & jumbo == 1, 10L,
  default = NA_integer_)]
type_labels <- c("1"="conv_purchase","2"="fha_purchase","3"="va_purchase",
  "4"="conv_refi_nocashout","5"="fha_refi_nocashout","6"="conv_refi_cashout",
  "7"="fha_refi_cashout","8"="conv_purchase_jumbo",
  "9"="conv_refi_nocashout_jumbo","10"="conv_refi_cashout_jumbo")
dat <- dat[!is.na(type)]
dat[, loan_cat := factor(unname(type_labels[as.character(type)]),
                         levels = unname(type_labels))]
dat[, originated := as.integer(action_type == 1)]
cat(sprintf("Popick sample after taxonomy: %s rows\n",
            format(nrow(dat), big.mark = ",")))
print(dat[, .N, by = loan_cat][order(-N)])

# --- 8. lender features: origination counts + offers-other-types -------------------
feats <- dat[originated == 1, .(total_origs = .N,
              conv = sum(loan_type == 1), fha = sum(loan_type == 2),
              va = sum(loan_type == 3), usda = sum(loan_type == 4)), by = lei]
dat[feats, on = "lei", `:=`(total_origs = i.total_origs, .conv = i.conv,
                            .fha = i.fha, .va = i.va, .usda = i.usda)]
for (v in c("total_origs", ".conv", ".fha", ".va", ".usda"))
  dat[is.na(get(v)), (v) := 0]
dat[, lender_orig_bin := cut(total_origs, c(-Inf, 500, 2000, 5000, 10000, Inf),
                             right = TRUE, dig.lab = 6)]
dat[, offers_other_types := fcase(
  loan_type == 1, as.integer(.conv > 0 & (.fha + .va + .usda) > 0),
  loan_type == 2, as.integer(.fha  > 0 & (.conv + .va + .usda) > 0),
  loan_type == 3, as.integer(.va   > 0 & (.conv + .fha + .usda) > 0),
  loan_type == 4, as.integer(.usda > 0 & (.conv + .fha + .va) > 0),
  default = 0L)]
dat[, c(".conv", ".fha", ".va", ".usda") := NULL]

# --- 9. Popick Table-3 bins + geography FE + pricing components --------------------
dat[, cs_bin  := cut(credit_score, c(-Inf,580,620,660,700,740,Inf),
                     right = FALSE, dig.lab = 5)]
dat[, dti_bin := cut(dti,          c(-Inf,28,36,43,45,50,Inf),
                     right = TRUE, dig.lab = 5)]
dat[, ltv_bin := cut(ltv_combined, c(-Inf,70,80,90,95,96.5,Inf),
                     right = TRUE, dig.lab = 5)]
dat[, income_bin := cut(income,    c(-Inf,40,60,90,140,210,Inf),
                        right = TRUE, dig.lab = 5)]
dat[, cs_bin := relevel(cs_bin, ref = tail(levels(cs_bin), 1))]  # ref = best
dat[, state_msa  := paste0(property_state, "_",
                           fifelse(metro == 1L, "MSA", "notMSA"))]
dat[, ir_spread_pmms := interest_rate - pmms]
dat[, disc_pts_pct  := 100 * disc_points    / loan_amount]
dat[, lend_cred_pct := 100 * lender_credits / loan_amount]
dat[, loan_cost_pct := 100 * loan_costs     / loan_amount]

# --- 10. save ------------------------------------------------------------------------
saveRDS(dat, out("analysis_2025.rds"), compress = FALSE)
cat("Saved ->", out("analysis_2025.rds"), "\n")
