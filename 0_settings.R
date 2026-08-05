# =============================================================================
# 0_settings.R  (DEV chain)  --  step 0: paths for THIS machine + vintage.
#
# Edit the four paths, run this script ONCE at the start of the session
# (Ctrl+A, Ctrl+Enter). It defines the paths and the out() helper in your
# environment; scripts 1-8 use them directly and auto-source this file only
# if you forgot to run it. No separate config file to keep track of.
# =============================================================================

raw_dta    <- "EDIT-ME"   # e.g. "//hqwinfs1/economist/Projects/HMDA/Agency Data/2025/HMDA_05_31_2026/all_agency_hmda_2025_05_31_2026_final.dta"
oce_file   <- "EDIT-ME"   # OCE combined .dta (quarterly pull)
xwalk_file <- "EDIT-ME"   # HMDA-CU crosswalk .dta (join_number -> LEI)
work_dir   <- "EDIT-ME"   # out/<year> folder for THIS vintage; never mix years

# OPTIONAL: a folder from an EARLIER successful run (e.g. the production
# out/2025). 1_reference.R reuses its reference files instead of downloading
# (the FRED/FHFA/Census pulls are often blocked on the agency network).
# Leave "" to build fresh.
prior_work_dir <- ""

if (any(c(raw_dta, oce_file, xwalk_file, work_dir) == "EDIT-ME"))
  stop("0_settings.R still has EDIT-ME placeholders.\n",
       "  Edit the four paths at the top, run this script, then continue.",
       call. = FALSE)

dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
out <- function(name) file.path(work_dir, name)

cat("[0_settings] outputs go to:", work_dir, "\n")
for (f in c(raw = raw_dta, oce = oce_file, xwalk = xwalk_file))
  cat(sprintf("  %-6s %-9s %s\n", names(f),
              if (file.exists(f)) "found" else "!! MISSING", f))
