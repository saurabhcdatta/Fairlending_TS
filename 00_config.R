# ============================================================================
# 00_config.R  —  Configuration for the NCUA HMDA fair-lending pipeline
# ----------------------------------------------------------------------------
# Edit paths and parameters HERE only. Everything downstream reads `cfg`.
# Dataset construction follows the NCUA SAS adaptation of Popick (2022),
# generalized from single-year (2025) to the 2019-2025 panel.
# ============================================================================

suppressPackageStartupMessages({
  library(haven)       # read_sas(), write_dta()
  library(arrow)       # parquet + open_dataset()
  library(data.table)  # fast in-memory wrangling
  library(fixest)      # feglm()/feols() with absorbed FE + clustered SE
  library(dplyr)       # arrow dataset -> collect
  library(stringr)
  library(glue)
})

cfg <- list()

# ---- Threads (32GB workstation: multithread within libraries, never fork) --
# data.table: 0 = use all logical cores. arrow respects its own option.
# fixest::feglm/feols take nthreads from cfg$model$nthreads at call time.
setDTthreads(0)
options(arrow.use_threads = TRUE)
cfg$nthreads <- max(1L, parallel::detectCores(logical = TRUE) - 1L)

# ---- Paths (forward slashes are fine on Windows) ---------------------------
cfg$paths <- list(
  sas_dir     = "S:/Projects/HMDA/Time_Series/Data",   # legacy sas7bdat copies
  parquet_dir = "S:/Projects/OCFP_Fair_Lending/2025_New/data/parquet_panel",
  ref_dir     = "S:/Projects/OCFP_Fair_Lending/2025_New/data/reference",
  out_dir     = "S:/Projects/OCFP_Fair_Lending/2025_New/output"
)

# Reference inputs (replacing hardcoded SAS blocks; see README for sources):
#  - pmms_weekly.csv : cols `date` (release date, YYYY-MM-DD), `pmms` (rate, %)
#       Freddie Mac PMMS archive / FRED series MORTGAGE30US, 2019-present.
#       Replaces ~300 hardcoded `if Act_Date>=... then PMMS=...` lines.
#  - msa_crosswalk.csv : NBER CBSA-county walk; cols fipsstatecode,
#       fipscountycode, cbsacode, metropolitanmicropolitanstatis
#  - jumbo_<year>.csv : FHFA conforming limits per county-year; cols
#       fips_state_code, fips_county_code, One_Unit_Limit, Two_Unit_Limit,
#       Three_Unit_Limit, Four_Unit_Limit  (same layout as Jumbo_2025.csv)
cfg$ref_files <- list(
  pmms  = "pmms_weekly.csv",
  msa   = "msa_crosswalk.csv",
  jumbo = "jumbo_%d.csv"        # sprintf pattern over data_year
)

# ---- Source data -------------------------------------------------------------
# "stata": read the RAW per-year Stata files on \\hqwinfs1 directly
#          (eliminates the SAS import hop AND the duplicate sas7bdat copies).
# "sas"  : legacy fallback -- the hmda19..25.sas7bdat copies on S:.
cfg$source_mode <- "stata"

# Raw Stata releases (release-stamped paths; update when a new release lands).
.agency <- "//hqwinfs1/economist/Projects/HMDA/Agency Data"
cfg$raw_files <- c(
  "2019" = file.path(.agency, "2019", "hmda_2019_11_28_2020_final.dta"),
  "2020" = file.path(.agency, "2020", "all_agency_hmda_2020_06_26_2021_final.dta"),
  "2021" = file.path(.agency, "2021", "all_agency_hmda_2021_04_28_2022_final.dta"),
  "2022" = file.path(.agency, "2022", "HMDA_05_31_2023",
                     "all_agency_hmda_2022_05_31_2023_final.dta"),
  "2023" = file.path(.agency, "2023", "HMDA_06_30_2024",
                     "all_agency_hmda_2023_06_30_2024_final.dta"),
  "2024" = file.path(.agency, "2024", "HMDA_05_31_2025",
                     "all_agency_hmda_2024_05_31_2025_final.dta"),
  "2025" = file.path(.agency, "2025", "HMDA_05_31_2026",
                     "all_agency_hmda_2025_05_31_2026_final.dta")
)

# Legacy sas7bdat copies (used only when source_mode == "sas").
cfg$sas_files <- c(
  "2019" = "hmda19.sas7bdat", "2020" = "hmda20.sas7bdat",
  "2021" = "hmda21.sas7bdat", "2022" = "hmda22.sas7bdat",
  "2023" = "hmda23.sas7bdat", "2024" = "hmda24.sas7bdat",
  "2025" = "hmda25.sas7bdat"
)
cfg$years <- as.integer(names(cfg$sas_files))

# ---- Build-stage settings (from the proven v2 builder) ----------------------
cfg$build <- list(
  compression       = "zstd",   # better cold-storage ratio than snappy
  compression_level = 3L,
  overwrite         = FALSE,    # skip years already converted
  # NULL = read ALL columns (parquet panel is a complete archive of the raw
  # release -- recommended). To cut the network read/parse cost instead, set
  # to the modeling columns, e.g.:  unique(c(ANALYSIS_COLS, NUMERIC_FROM_CHAR))
  # after sourcing 01/02. Tradeoff: the panel then isn't a full archive.
  col_select        = NULL
)

# ---- Sentinel / placeholder codes ------------------------------------------
# VERIFY against the FFIEC Filing Instructions Guide. The SAS program excludes
# DTI / CLTV / income records coded 9999 or 8888 in its final WHERE clause.
cfg$sentinels <- list(
  na_strings      = c("", " ", ".", "NA", "N/A", "Exempt", "EXEMPT", "exempt",
                      "1111", "-1111"),
  exempt_code     = 1111,
  credit_score_lo = 0,      # valid scores are (lo, hi) exclusive, per SAS
  credit_score_hi = 900,    # (>0 and <900 -> codes 7777/8888/9999 fall out)
  age_na          = c(8888, 9999, 1111),
  factor_na       = c(9999, 8888)   # DTI / CLTV / income exclusion codes
)

# ---- HMDA enumerations ------------------------------------------------------
cfg$codes <- list(
  action = c(originated = 1L, approved_not_accepted = 2L, denied = 3L,
             withdrawn = 4L, closed_incomplete = 5L, purchased = 6L,
             preapp_denied = 7L, preapp_approved_not_accepted = 8L),
  loan_type = c(conventional = 1L, fha = 2L, va = 3L, rhs_fsa = 4L),
  loan_purpose = c(purchase = 1L, home_improvement = 2L,
                   refi_no_cashout = 31L, refi_cashout = 32L),
  lien = c(first = 1L, subordinate = 2L),
  occupancy = c(principal = 1L, second = 2L, investment = 3L),
  construction = c(site_built = 1L, manufactured = 2L)
)

# ---- Loan "Type" taxonomy (extends Popick; from the NCUA SAS program) -------
# All require 360-month term, fixed rate, 1-4 units, owner-occupied principal,
# first lien, site-built. Types 1-7 are non-jumbo; 8-10 are conventional jumbo.
cfg$type_labels <- c(
  "1"  = "conv_purchase",          "2"  = "fha_purchase",
  "3"  = "va_purchase",            "4"  = "conv_refi_nocashout",
  "5"  = "fha_refi_nocashout",     "6"  = "conv_refi_cashout",
  "7"  = "fha_refi_cashout",       "8"  = "conv_purchase_jumbo",
  "9"  = "conv_refi_nocashout_jumbo", "10" = "conv_refi_cashout_jumbo"
)
# Types used for the pricing / 2SLS export in the SAS program (conv non-jumbo)
cfg$pricing_types <- c(1L, 4L, 6L)

# ---- Race / ethnicity knobs (Popick fn. 10 via the SAS implementation) ------
cfg$race <- list(
  race_fields    = 1:2,   # SAS uses race/ethnicity fields 1 and 2 only
  asian_codes    = c(2, 21:27),  # SAS literal has {2,21,22,23,25,25,26,27};
                                 # 24 missing + 25 duplicated = apparent typo.
                                 # We use the full FFIEC set 21-27. See README.
  require_gender = TRUE    # final sample keeps Gender in {Female, Male}
)

# ---- Credit-factor bins ------------------------------------------------------
# Scheme "popick": fixed Table-3 bins (the SAS "_a" / Goodstein alternative).
# Scheme "sextile": type-specific empirical sextiles computed from the data --
#   this REPLACES the ~250 hardcoded per-type cutpoint lines in the SAS program
#   with reproducible quantiles (written to out_dir for documentation).
cfg$bins <- list(
  scheme       = "popick",
  credit_score = c(-Inf, 580, 620, 660, 700, 740, Inf),   # right = FALSE
  dti          = c(-Inf, 28, 36, 43, 45, 50, Inf),
  ltv          = c(-Inf, 70, 80, 90, 95, 96.5, Inf),
  income       = c(-Inf, 40, 60, 90, 140, 210, Inf),      # SAS: 40/60/90/140/210
  sextile_probs = (1:5) / 6
)

# Lender annual origination-count bins (reference = >10000)
cfg$lender_orig_bins <- c(-Inf, 500, 2000, 5000, 10000, Inf)

# High-cost lender: share of originations with rate spread in [1.5, 99) > 10%
cfg$hc_lender <- list(spread_lo = 1.5, spread_hi = 99, share = 0.10)

# ---- Modeling knobs ---------------------------------------------------------
cfg$model <- list(
  # "popick" = state x MSA-status FE (the SAS state-MSA dummies, as one factor);
  # "county" = county FE; "tract" = census-tract FE.
  geography       = "popick",
  time            = "year_month",
  cluster_var     = "lei",
  sig_level       = 0.01,
  reference_group = "white"
)

cfg$pclass_compare <- c("black", "hispanic", "asian", "aian", "nhpi")

invisible(cfg)
