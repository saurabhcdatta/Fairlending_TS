# HMDA 2025 fair-lending screen — standalone R chain

Self-contained: copy this folder anywhere, edit the paths in `settings.R`
(raw_dta, work_dir, oce_file, xwalk_file), and run. Each numbered script
reads the previous one's output; run one at a time, inspect between steps,
rerun any step alone. Packages: data.table, haven, fixest, ggplot2
(+ readstata13 auto-installed by step 01; systemfit optional for the SUR
replication; xgboost or ranger only for the optional 03b ML track).

```
settings.R        ALL paths (4 lines to edit)
dta_header.R      column names/types straight from the .dta binary header
read_hmda_2025.R  step 0: inspect the raw file (strL-safe); saves column CSV
00_reference.R    run once: pmms_weekly / jumbo_2025 / msa_crosswalk into
                  work_dir (FRED / FHFA / NBER 2023 csv; CSV-only). If the
                  proxy blocks a download it prints the exact browser URL +
                  filename to stage into work_dir, then rerun (an Excel
                  Save-As-CSV of the Census list1 also works; header row is
                  auto-found)
00_assets.R       run once: OCE pull + HMDA xwalk (.dta read directly in R)
                  -> cu_assets_2025.csv incl. cu_type (1 = federal insured,
                  2 = state insured). SCREENS AND RANKINGS ARE SEPARATE BY
                  cu_type: top-200 universe per type, peer nulls +
                  shrinkage + BH within type, ranks restart within type.
                  Models stay pooled (full-market underwriting benchmark).
01_extract.R      raw .dta -> hmda_cu_2025.rds (chunked, strL-excluded,
                  readstata13 auto-selected, save verified on disk; later
                  chunks slower by design -- watch the %)
02_prepare.R      -> analysis_2025.rds: rename map with candidate vectors
                  (gender_app / property_county_fip confirmed), Popick fn.10
                  race from fields 1-2 with FFIEC subcodes, sample
                  restrictions, Types 1-10 taxonomy, lender features,
                  Table-3 bins, EARLY_BANKRUPT, PMMS spread / jumbo / MSA
03a_popick_models.R       RACE-BLIND models by loan category (race in NO model):
                  denial + withdrawal logits, four pricing components, full
                  Popick controls, state-MSA + year-month FE, LEI-clustered.
                  Evidence = MEAN RESIDUAL GAP vs white (Welch), printed per
                  model, forest pages + full coefficient tables in
                  regression_output_2025.pdf, gap tables as CSV; residuals
                  (with ULI) for the screens; sur_pricing() SUR replication
                  (deliberately NOT the SAS 2SLS -- rationale in file) with
                  OLS fallback for thin cells
03b_ml_residuals.R  OPTIONAL ML robustness track: cross-fitted gradient
                  boosting with LEI-GROUPED folds (no CU sees its own loans
                  in training), curated race-blind features, coarse
                  geography, monotone constraints + per-loan SHAP on
                  xgboost (ranger fallback), calibration + importance
                  diagnostics, econ-vs-ML agreement printed. Writes 03's
                  schema; make_default=TRUE swaps it in (econ backed up);
                  rerun 04a + 06. Cells flagged by BOTH tracks are the
                  strongest field cases.
03c_unsupervised.R  OPTIONAL unsupervised strengtheners (base-R kmeans, no
                  packages): (A) CU PEER CLUSTERS on business-model features
                  -- set peer_group <- "cluster" in 04a to benchmark against
                  actual peers; (B) STEERING SCREEN -- pricing-regime
                  clustering + placement test among PRIME-ELIGIBLE borrowers
                  (do minorities land in the high-cost regime more than
                  whites at the same CU?); add "steering" to flag_source in
                  06 to mine its loans. Placement evidence complements the
                  residual screens (levels).
07_ensemble.R     THE ROBUST LIST: ensembles whatever streams have run --
                  popick (03a->04a->06 tagged), ml (03b->04a->06 tagged),
                  steering (03c). Robust tier = same lei x group flagged by
                  >= 2 streams (convergence = priority, NOT unanimity;
                  steering catches placement patterns level-screens cannot).
                  ensemble_flags / ensemble_loans / ensemble_rankings, per
                  cu_type. Per-stream workflow: 03a -> 04a("popick") ->
                  06("popick"); 03b make_default -> 04a("ml") -> 06("ml");
                  03c; then 07.
04_screen.R       flags_2025.csv: TWO tracks side by side, per cu_type.
                  v2: EB-shrunk gap floors + BH tiers (high/flag/watch).
                  LEGACY SAS: the production SAS rules ported exactly
                  (denial exp-gap odds >= 1.1; withdrawal worst decile;
                  pricing SIGN GAUNTLET rate 10bp vs other-cost composite;
                  min_rec 100; pooled Types 1/4/6) -> also
                  legacy_sas_flags_2025.csv. All thresholds in FILTERS.
04a_scientific_screen.R  ALTERNATIVE to 04 (no SAS rules): Efron EMPIRICAL
                  NULL (peer-relative within cu_type; switchable to
                  theoretical), PERMUTATION GUARD for small cells,
                  POSTERIOR MATERIALITY P(true gap >= floor) via
                  Paule-Mandel. Same flags_2025.csv out; set
                  flag_source <- "v2" in 06. Rationale per filter in header.
05_report.R       report_flag_gaps.png (tier-colored), gap densities,
                  summary_2025.csv by cu_type x screen
06_rank_outliers.R  THE RANKING, per cu_type: CUs ordered by OUTLIER LOANS
                  (by ULI) -- overall (outlier_rankings_2025.csv), per
                  screen (outlier_rank_by_screen_2025.csv), per product
                  (outlier_rank_by_product_2025.csv); per-screen PNG charts;
                  outlier_loans_2025.csv = EVERY outlier ULI with its
                  underwriting profile for stage-2 investigation. Flag
                  source (v2/sas/both) and loan-level cutoffs in FILTERS.
08_exec_report.R  EXECUTIVE GRAPHICS (run after 07): KPI cover, robust-list
                  leaderboard by charter, method-convergence grid, problem-mix
                  by screen, size-vs-findings context. Five PNGs + one
                  multi-page exec_report_2025.pdf.
09_validation.R   THE VALIDATION DOSSIER (run after 04a): PLACEBO test
                  (labels permuted within CU -- screen must flag ~nothing),
                  INJECTION POWER study (known gaps planted into real
                  residuals -- operating characteristics), E-VALUE bounds
                  (how strong an omitted factor must be to nullify each
                  flag), and cross-year PERSISTENCE when flags_<year>.csv
                  files coexist. Backs up and restores all real outputs.
smoke_test.R      fake data (incl. cu_type mix), planted violators in
                  DIFFERENT cu_types, fake reference files; runs the whole
                  chain unchanged; restores settings.R even on crash
```

## Run order on the workstation

```r
# 0. Rscript smoke_test.R          -- must end SMOKE TEST PASSED
# 1. source("read_hmda_2025.R")    -- inspect; hmda_2025_columns.csv
# 2. source("00_reference.R"); source("00_assets.R")     -- once each
# 3. source("01_extract.R")        -- the long one; ends "Saved -> (x GB)"
# 4. source("02_prepare.R")        -- check: ~18-19% withdrawn, ~15% denied,
#                                     jumbo near ~5.9%, taxonomy counts
# 5. source("03a_popick_models.R")         -- console gaps per model + PDF
# 6. source("04_screen.R")  OR  source("04a_scientific_screen.R")
# 7. source("05_report.R"); source("06_rank_outliers.R")
# optional ML rescreen: source("03b_ml_residuals.R") then rerun 6.-7. (04a)
```

## Facts confirmed on the real 2025 file

15.2 GB, ~15M rows, dta format 118. Raw names LEI / ULI (uppercase),
credit_score_app, gender_app, property_county_fip. 10 strL columns (all
free-text) excluded; haven cannot open the file at all -> readstata13
engine, verified row-identical. cu_type arrives via the OCE pull.
