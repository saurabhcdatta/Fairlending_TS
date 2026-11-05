# =============================================================================
# 12_coverage.R  (DEV chain)  --  how much of 2025 credit-union mortgage lending
# the screening universe covers: the number and the counts for the limits slide
# ("the 200 largest institutions per charter type: N of M filers, accounting
# for X% of credit-union mortgage applications").
#
# Inputs: hmda_cu_2025.rds (3_extract.R) and cu_assets_2025.csv (2_assets.R).
# Run any time after 3; numbered 12 because it is a report, not a stage --
# nothing downstream reads it. If 4 and 6 have run in this session it also
# cross-checks itself against their `wf` and `top_uni`.
#
# Denominator = every application credit unions filed in 2025, deduplicated
# exactly as 4_prepare.R does (one row per lender x loan id), BEFORE any
# sample restriction -- the same "1.8 million" the funnel slide opens with,
# not the Popick modelling sample. Universe rule = 6_screen.R's knobs.
#
# Output: coverage_2025.csv; prints the sentence for the slide.
# =============================================================================

library(data.table)
if (!exists("out")) source("0_settings.R")   # paths: run/edit 0_settings.R first

.chk <- function(ok, msg) {
  cat(sprintf("%s %s\n", if (isTRUE(ok)) "  [check OK]  " else "  !! CHECK FAILED:", msg))
  if (!isTRUE(ok)) warning("CHECK FAILED: ", msg, call. = FALSE, immediate. = TRUE)
  invisible(isTRUE(ok))
}

# --- 1. the universe rule: 6_screen.R's knobs if they are in the session ------
cov_types <- if (exists("screen_types")) screen_types else c(1L, 2L)  # federally insured only
cov_top_n <- if (exists("top_n")) top_n else 200L                     # largest N by assets PER cu_type
if (!is.finite(cov_top_n)) cov_top_n <- .Machine$integer.max          # top_n <- Inf = every CU, as in 6
cat(sprintf("Universe rule: cu_type in {%s}, top %s by assets per type%s\n",
            paste(cov_types, collapse = ", "),
            if (cov_top_n == .Machine$integer.max) "ALL" else cov_top_n,
            if (exists("screen_types")) "  (taken from 6_screen.R in this session)"
            else "  (defaults; keep equal to screen_types / top_n in 6_screen.R)"))

# --- 2. inputs ----------------------------------------------------------------
for (f in c("hmda_cu_2025.rds", "cu_assets_2025.csv"))
  if (!file.exists(out(f)))
    stop(f, " not found in ", work_dir, " -- run 2_assets.R and 3_extract.R first.",
         call. = FALSE)

cov_assets <- fread(out("cu_assets_2025.csv"), colClasses = list(character = "lei"))
if (!"cu_type" %in% names(cov_assets)) cov_assets[, cu_type := NA_integer_]
cov_assets[is.na(cu_type), cu_type := 0L]
cov_universe <- cov_assets[cu_type %in% cov_types][order(-assets_tot),
                            head(.SD, cov_top_n), by = cu_type]   # same line as 6_screen.R

raw <- as.data.table(readRDS(out("hmda_cu_2025.rds")))
setnames(raw, tolower(names(raw)))
# the three columns needed, resolved as 4_prepare.R's rename map resolves them
.pick <- function(cands) { hit <- cands[cands %in% names(raw)]
                           if (!length(hit)) stop("raw file has none of: ",
                                                  paste(cands, collapse = ", "), call. = FALSE)
                           hit[1] }
apps <- raw[, .(lei         = get(.pick(c("lei", "legal_entity_identifier"))),
                uli         = get(.pick(c("uli", "universal_loan_identifier"))),
                action_type = suppressWarnings(as.numeric(get(.pick("action_type")))))]
rm(raw)
n_raw <- nrow(apps)

# --- 3. dedupe exactly as 4_prepare.R: one row per (lei, uli), keep last --------
# (exact-duplicate rows and refiled loan ids both collapse to the last record;
#  the key is lender x loan id because NULIs repeat across lenders by design)
apps <- apps[!duplicated(apps[, .(lei, uli)], fromLast = TRUE)]
cat(sprintf("Raw CU records %s -> %s applications after the (lei, uli) dedupe\n",
            format(n_raw, big.mark = ","), format(nrow(apps), big.mark = ",")))
if (exists("wf")) {
  .chk(wf[step == "raw CU applications", rows] == n_raw,
       "raw row count matches 4_prepare.R's waterfall (`wf`) in this session")
  .wf_dedup <- wf[grepl("duplicate|keep last", step)]
  if (nrow(.wf_dedup))
    .chk(tail(.wf_dedup$rows, 1) == nrow(apps),
         "post-dedupe count matches 4_prepare.R's waterfall")
}

# --- 4. one row per filer, flagged --------------------------------------------
cov_filers <- apps[, .(rows      = .N,
                       purchased = sum(action_type == 6, na.rm = TRUE)), by = lei]
cov_filers[cov_assets, on = "lei", `:=`(cu_type = i.cu_type, name = i.name,
                                        assets_tot = i.assets_tot)]
cov_filers[is.na(cu_type), cu_type := -1L]          # filer absent from the assets crosswalk
cov_filers[, fed_insured := cu_type %in% cov_types]
cov_filers[, in_universe := lei %in% cov_universe$lei]
if (exists("top_uni"))
  .chk(setequal(cov_filers[in_universe == TRUE, lei], intersect(top_uni$lei, cov_filers$lei)),
       "universe matches 6_screen.R's `top_uni` in this session")

# --- 5. the coverage table ------------------------------------------------------
cov_n_all <- nrow(cov_filers)
cov_r_all <- sum(cov_filers$rows)
cov_r_fed <- cov_filers[fed_insured == TRUE, sum(rows)]
coverage <- rbind(
  cov_filers[,                    .(stage = "all CU filers in the 2025 extract",
                                    CUs = .N, applications = sum(rows))],
  cov_filers[fed_insured == TRUE, .(stage = "federally insured (cu_type 1-2)",
                                    CUs = .N, applications = sum(rows))],
  cov_filers[in_universe == TRUE, .(stage = sprintf("screening universe: top %d per type", cov_top_n),
                                    CUs = .N, applications = sum(rows))])
coverage[, share_of_all_apps := round(100 * applications / cov_r_all, 1)]
coverage[, share_of_fed_apps := round(100 * applications / cov_r_fed, 1)]
coverage[, share_of_all_CUs  := round(100 * CUs / cov_n_all, 1)]

cat("\nCoverage of the screening universe (deduped applications, all action types):\n")
print(coverage)
cat("\nUniverse by charter type (on the asset list vs. actually filed HMDA in 2025):\n")
print(merge(cov_universe[, .(on_list = .N), by = cu_type],
            cov_filers[in_universe == TRUE,
                       .(filed_2025 = .N, applications = sum(rows),
                         smallest_M = round(min(assets_tot) / 1e6, 1)), by = cu_type],
            by = "cu_type", all = TRUE)[order(cu_type)])
cat("\nFilers outside the universe, by why:\n")
print(cov_filers[in_universe == FALSE,
                 .(CUs = .N, applications = sum(rows)),
                 by = .(why = fcase(cu_type == -1L, "not in the assets crosswalk",
                                    cu_type ==  3L, "non-federally insured (type 3)",
                                    cu_type ==  0L, "cu_type unknown (0)",
                                    default = "fed-insured but below the top-N cut"))
                 ][order(-applications)])
cat(sprintf(paste0("(purchased loans, action_type 6, in the extract: %s rows -- not applications;",
                   " if material, replace `rows` with `rows - purchased` in block 5 so both",
                   " numerator and denominator exclude them)\n"),
            format(sum(cov_filers$purchased), big.mark = ",")))

# --- 6. the sentence for the slide, the file, the checks ------------------------
cat(sprintf(paste0("\nFOR THE LIMITS SLIDE: the %d largest institutions per charter type -- %s of %s",
                   " filers (%s of the %s federally insured) -- account for %.1f%% of all 2025",
                   " credit-union mortgage applications (%.1f%% of federally insured filers'",
                   " applications).\n"),
            cov_top_n,
            format(coverage[3, CUs], big.mark = ","), format(coverage[1, CUs], big.mark = ","),
            format(coverage[3, CUs], big.mark = ","), format(coverage[2, CUs], big.mark = ","),
            coverage[3, share_of_all_apps], coverage[3, share_of_fed_apps]))

fwrite(coverage, out("coverage_2025.csv"))
.chk(coverage[3, CUs] <= length(cov_types) * cov_top_n,
     "universe holds at most top_n CUs per charter type")
.chk(cov_filers[fed_insured == TRUE, mean(in_universe)] < 1,
     "some federally insured filers fall below the top-N cut (else top_n is not binding)")
.chk(coverage[3, share_of_all_apps] > 50,
     "universe covers most applications (else the lei join to cu_assets is off)")
.chk(cov_filers[cu_type == -1L, sum(rows)] / cov_r_all < 0.10,
     "under 10% of applications come from filers missing in the assets crosswalk")
cat("Saved ->", out("coverage_2025.csv"), "\n")
cat("Left in the environment: coverage, cov_filers, cov_universe, apps\n")
