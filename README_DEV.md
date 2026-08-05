# HMDA Fair-Lending Screen -- DEVELOPMENT CHAIN (Popick + enhanced filters)

The block-by-block companion to the production `hmda_simple` folder.
Same estimators, same statistics, one stream: race-blind Popick models
(5) -> the enhanced screen (6) -> loan-level ranking + evidence (7).
The ensemble machinery (ML residuals 03b, unsupervised/steering 03c,
ensemble 07, multi-stream tabs) is deliberately absent -- it returns to
the production chain, not here. The production zip remains the
deployable unit of record; this folder is for methodology work.

Built for RStudio: open a script, select a block, Ctrl+Enter, inspect.
Every script prints diagnostics as it goes, runs `[check OK]` /
`!! CHECK FAILED` assertions on its own output, and leaves its
intermediate objects in the environment (listed at the end of each
script and below). Scripts still run whole via source() in order 1..7.

## Setup

Paths live in `0_settings.R` -- part of the numbered chain, not a
separate config file. The optional `prior_work_dir` there points at a
folder from an earlier successful run (e.g. the production out/2025):
1_reference.R then reuses its reference files and staged downloads
instead of hitting FRED/FHFA/Census, which the agency network often
blocks, and its diagnostics verify what was copied (including that the
PMMS file actually covers the 2025 action dates). Edit the four paths, run it once at the start of
the session; it defines the paths and `out()` in your environment and
reports which input files it can see. Scripts 1-8 use those values
directly and auto-source 0_settings.R only if you forgot to run it
(`if (!exists("out"))`), so nothing re-reads config behind your back.
Packages: data.table, fixest, ggplot2, haven (+ readstata13
auto-installed by 3; openxlsx2 or writexl for the workbook in 7).

## The chain

| script | what it does | leaves in the environment |
|---|---|---|
| 0_settings.R | the four machine/vintage paths; run once per session | raw_dta, oce_file, xwalk_file, work_dir, out() |
| 1_reference.R | PMMS weekly rates, FHFA limits, MSA crosswalk + plausibility checks | pmms_d, jmb_d, msa_d |
| 2_assets.R | CU assets + charter type from the OCE pull (cached; auto-rebuilds if stale) | cu_d |
| 3_extract.R | chunked read of the raw .dta, CU rows only (the long one) | hmda_cu |
| 4_prepare.R | analysis table: filters (with a printed sample waterfall `wf`), joins, Popick taxonomy, bins | dat, wf, na_prof |
| 5_models.R | BLOCK 1 denial logits, BLOCK 2 withdrawal, BLOCK 3 pricing (4 components), BLOCK 4 gap tables + saves, BLOCK 5 regression PDF | mods_denial/mods_wdr/mods_price (fitted models), den/wdr/pri_gaps, den/wdr/pri_resid, coef_tables |
| 6_screen.R | the ENHANCED FILTERS: empirical null vs charter-type peers, FDR, permutation guard, posterior materiality; one block per screen; gap-density plot | flags, cells_denial/withdrawal/pricing, assets, top_uni |
| 7_rank.R | outlier loans by ULI (excess-calibrated), rankings, why-engine, comparators, rebuttals, summaries, Excel workbook | cells, outliers, loans, rank_tab/scr_tab/prd_tab, cur |
| 8_validation.R | placebo + injection-recovery dossier (reruns 6 on perturbed residuals; restores everything) | validation CSVs |
| 9_smoke_check.R | fake-data end-to-end proof: run `Rscript 9_smoke_check.R` after ANY edit; must end DEV SMOKE CHECK PASSED (snapshots and restores your session's path settings) | -- |

Model/spec constants live at the top of 5 (controls, FE, MIN_N) and the
screen thresholds at the top of 6 (FILTERS block: fdr_q, materiality
floors, perm_below, null_type). Change, re-select the affected blocks,
rerun -- downstream scripts only need rerunning from the first one whose
input changed.

## Reading the checks

`[check OK]` lines are invariants of a healthy run (rates inside bands,
gaps computing without NA poisoning, residuals centered, flag share
<= 10%, exports matching the evidence-implied loan counts, p/q in
[0,1]). A `!! CHECK FAILED` prints loudly and raises an R warning but
does not stop the run -- inspect the object it names before trusting
anything downstream. Bands are set wide enough that real 2025 data and
the fake smoke data both pass; several prints also show the real-2025
reference value for eyeballing.

## Validation knobs (8_validation.R)

Production record: B_placebo=10, gaps 0.02/0.05/0.10, 5 cells/gap.
For quick drafts set env vars before sourcing:
`Sys.setenv(VAL_B_PLACEBO=3, VAL_INJECT_GAPS="0.10", VAL_INJECT_CELLS=3)`

## Differences from production (so nobody is surprised later)

* Popick stream only: no 03b/03c/06a/06b/07/08, no stream tags, no
  tagged file copies, no ensemble workbook tabs. Sheet names in the
  workbook are unprefixed (Summary, Denial, ...).
* Legacy SAS parity screen (04) not carried over; the enhanced screen
  is the only filter here.
* Peer groups are charter type only (business-model cluster peers were
  part of the unsupervised track).
* Everything else -- specifications, empirical null, permutation guard,
  posterior materiality, excess calibration, evidence columns -- is
  identical to production 2026-07-22a.
