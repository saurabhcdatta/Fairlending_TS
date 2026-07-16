# =============================================================================
# 08_exec_report.R -- THE EXAMINER REPORT. Run after 07_ensemble.R.
#
# A self-explaining briefing for non-technical readers:
#   exec_0_process.png      HOW THE SCREEN WORKS -- flow chart with live counts
#   exec_1_overview.png     KPI cover
#   exec_2_start_here.png   the "start here" table: top institutions, key stats
#   exec_3_leaderboard.png  robust-loan leaderboard by charter
#   exec_4_convergence.png  which methods agree (why to trust the list)
#   exec_5_problem_mix.png  what KIND of problem each institution shows
#   exec_6_dollars.png      severity: dollars at stake + contradicted reasons
#   exec_7_size.png         findings vs institution size (context)
#   exec_8_method_notes.png plain-language methodology + honest limits
#   exec_report_2025.pdf    all pages, in order
# =============================================================================

library(data.table)
library(ggplot2)
cat("== 08_exec_report.R VERSION 2026-07-15b",
    "(process chart + evidence pages + reading guide) ==\n")

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
top_n_chart <- 12
# ------------------------------------------------------------------------------

.need <- function(f) {
  if (!file.exists(out(f)))
    stop(f, " not found -- run the chain through 07_ensemble.R first.",
         call. = FALSE)
  fread(out(f), colClasses = list(character = "lei"))
}
.opt <- function(f) if (file.exists(out(f)))
  fread(out(f), colClasses = list(character = "lei")) else NULL
ens_rk <- .need("ensemble_rankings_2025.csv")
ens_fl <- .need("ensemble_flags_2025.csv")
ens_ln <- .need("ensemble_loans_2025.csv")
flags  <- .need("flags_2025.csv")
smy <- .opt("outlier_summary_by_cu_ensemble_2025.csv")
if (is.null(smy)) smy <- .opt("outlier_summary_by_cu_2025.csv")

navy <- "#1F3864"; red <- "#C0392B"; orange <- "#E67E22"
teal <- "#1F7A6D"; gray <- "#B7BEC9"; paleblue <- "#DCE9F7"
type_lab <- c("0" = "charter unknown", "1" = "Federal charter",
              "2" = "State charter")
thm <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(colour = "grey30"),
        plot.caption = element_text(colour = "grey45", size = 9),
        panel.grid.minor = element_blank(),
        legend.position = "top")
pages <- list()

# ---- counts that drive the story -------------------------------------------------
n_cus     <- uniqueN(flags$lei)
n_cells   <- nrow(flags)
n_flag    <- flags[, sum(flag)]
n_loans   <- nrow(ens_ln)
n_robust  <- sum(ens_ln$robust)
n_rob_cu  <- ens_rk[robust_loans > 0, .N]
streams_run <- sort(unique(unlist(strsplit(ens_fl$streams, "+",
                                           fixed = TRUE))))

# ---- 0. PROCESS CHART: how the screen works, in one picture ------------------------
box <- function(x, y, w, h, fill) annotate("rect", xmin = x - w/2,
  xmax = x + w/2, ymin = y - h/2, ymax = y + h/2, fill = fill,
  colour = navy, linewidth = 0.4)
txt <- function(x, y, lab, size = 3.6, col = "white", face = "bold")
  annotate("text", x = x, y = y, label = lab, size = size, colour = col,
           fontface = face, lineheight = 0.95)
.pbox <- function(x, y, lab, size = 2.9)
  annotate("text", x = x, y = y, label = lab, size = size,
           colour = "grey25", lineheight = 0.95)
arrow_ <- function(x1, y1, x2, y2) annotate("segment", x = x1, y = y1,
  xend = x2, yend = y2, colour = navy, linewidth = 0.7,
  arrow = arrow(length = unit(0.12, "inches"), type = "closed"))
p0 <- ggplot() + xlim(0, 10) + ylim(0, 13.2) + theme_void() +
  labs(title = "  How the fair-lending screen works",
       subtitle = "  Every step is automated, statistical, and reproducible -- no institution is hand-picked") +
  theme(plot.title = element_text(face = "bold", size = 18, colour = navy),
        plot.subtitle = element_text(colour = "grey35")) +
  box(5, 12.2, 8.6, 1.1, navy) +
  txt(5, 12.4, "2025 HMDA LENDING DATA") +
  txt(5, 11.95, sprintf("every application at %s credit unions",
                        format(n_cus, big.mark = ",")), size = 3) +
  arrow_(5, 11.6, 5, 11.05) +
  box(5, 10.4, 8.6, 1.25, teal) +
  txt(5, 10.7, "THREE INDEPENDENT RACE-BLIND MODELS") +
  txt(5, 10.25, "economics (Popick)  |  machine learning  |  pricing-placement",
      size = 3) +
  .pbox(5, 9.55, paste("Each predicts outcomes from financial factors only --",
                     "race is never an input.\nA loan is interesting only",
                     "when the OUTCOME differs from the race-blind prediction.")) +
  arrow_(5, 9.15, 5, 8.6) +
  box(5, 7.95, 8.6, 1.25, navy) +
  txt(5, 8.25, sprintf("STATISTICAL SCREEN: %s cells tested -> %s flagged",
                       format(n_cells, big.mark = ","),
                       format(n_flag, big.mark = ","))) +
  txt(5, 7.8, "peer-relative benchmarks | false-discovery control | materiality floors",
      size = 3) +
  .pbox(5, 7.1, paste("A cell = one borrower group at one institution.",
                    "Flags require BOTH statistical significance\nAND a gap",
                    "large enough to matter -- small samples cannot flag by luck.")) +
  arrow_(5, 6.7, 5, 6.15) +
  box(5, 5.5, 8.6, 1.25, red) +
  txt(5, 5.8, sprintf("EXCESS-CALIBRATED SELECTION: %s loans for file review",
                      format(n_loans, big.mark = ","))) +
  txt(5, 5.35, "one reviewed loan per statistically estimated excess adverse outcome",
      size = 3) +
  .pbox(5, 4.65, paste("The review count is DERIVED from the evidence, not a",
                     "quota: a 5.5pp excess across 5,089\napplicants means",
                     "~280 loans -- the 280 most unexpected ones.")) +
  arrow_(5, 4.25, 5, 3.7) +
  box(5, 3.05, 8.6, 1.25, teal) +
  txt(5, 3.35, "EVIDENCE PACKAGE PER LOAN") +
  txt(5, 2.9, "why the model expected approval | matched white comparator | rebuttal of stated reason",
      size = 3) +
  arrow_(5, 2.4, 5, 1.85) +
  box(5, 1.2, 8.6, 1.25, navy) +
  txt(5, 1.5, sprintf("ENSEMBLE: %s ROBUST loans at %d institutions",
                      format(n_robust, big.mark = ","), n_rob_cu)) +
  txt(5, 1.05, "robust = the same borrowers flagged by 2+ independent methods",
      size = 3)
pages[["exec_0_process"]] <- p0

# ---- 1. KPI cover ------------------------------------------------------------------
kpi <- data.table(x = 1, y = 5:1, txt = c(
  sprintf("%s credit unions screened across %s statistical cells",
          format(n_cus, big.mark = ","), format(n_cells, big.mark = ",")),
  sprintf("%d analysis streams run: %s", length(streams_run),
          paste(streams_run, collapse = ", ")),
  sprintf("%d institutions show MULTI-METHOD (robust) findings", n_rob_cu),
  sprintf("%s loans identified for file review (%s robust)",
          format(n_loans, big.mark = ","), format(n_robust, big.mark = ",")),
  "Robust = the same institution and borrower group flagged by 2+ independent methods"))
# ---- 0b. INSTRUMENT VALIDATION (renders when 09 has run) ------------------------
pl_f <- out("validation_placebo_2025.csv"); pw_f <- out("validation_power_2025.csv")
if (file.exists(pl_f) && file.exists(pw_f)) {
  pl <- fread(pl_f); pw <- fread(pw_f)
  vt <- rbind(
    data.table(test = "Placebo: labels shuffled\n(must find ~nothing)",
               grp = c("Real labels", "Shuffled (mean)", "Shuffled (worst)"),
               val = c(pl[metric == "real_flags", value],
                       pl[metric == "placebo_mean", value],
                       pl[metric == "placebo_max", value])),
    if (nrow(pw)) data.table(
      test = "Injection: planted gaps\n(must be detected)",
      grp = sprintf("%.0f pp planted", pw$gap_pp),
      val = 100 * pw$detected / pmax(pw$injected, 1)))
  ev_cap <- if (file.exists(out("validation_evalues_2025.csv")) &&
                file.size(out("validation_evalues_2025.csv")) > 10) {
    evv <- fread(out("validation_evalues_2025.csv"))
    if (nrow(evv)) sprintf(
      "Omitted-variable robustness (E-values), top cells: %s",
      paste(head(sprintf("%s %s %.1f", evv$name, evv$group, evv$evalue), 3),
            collapse = "; ")) else NULL
  } else NULL
  pages[["exec_0b_validation"]] <- ggplot(vt,
      aes(grp, val, fill = grepl("Real|planted", grp))) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = round(val, 1)), vjust = -0.4, size = 3.6,
              colour = navy) +
    facet_wrap(~ test, scales = "free") +
    scale_fill_manual(values = c(`TRUE` = red, `FALSE` = gray)) +
    scale_y_continuous(expand = expansion(mult = c(0, .18))) +
    labs(title = "The instrument was tested before you read the findings",
         subtitle = "Left: with race labels randomly shuffled the screen goes silent. Right: % of planted violations detected.",
         x = NULL, y = NULL, caption = ev_cap) + thm
} else cat("(09_validation.R has not run -- validation page skipped;",
           "run 09 then rerun 08 to include it)\n")

pages[["exec_1_overview"]] <- ggplot(kpi, aes(x, y, label = txt)) +
  geom_text(hjust = 0, size = c(5.2, 5.2, 5.6, 5.2, 3.8),
            fontface = c("plain", "plain", "bold", "plain", "italic"),
            colour = c(navy, navy, red, navy, "grey40")) +
  xlim(1, 10) + ylim(0.5, 5.8) + theme_void() +
  labs(title = "  2025 Fair-Lending Screen -- Executive Summary",
       subtitle = "  Race-blind models + machine learning + unsupervised placement analysis") +
  theme(plot.title = element_text(face = "bold", size = 18, colour = navy),
        plot.subtitle = element_text(colour = "grey35"))

# ---- 2. START HERE: the table an examiner reads first ------------------------------
if (!is.null(smy)) {
  st <- copy(smy)[order(-total_outliers)][1:min(8, .N)]
  st <- merge(st, ens_rk[, .(name, robust_loans2 = robust_loans)],
              by = "name", all.x = TRUE, sort = FALSE)
  hdr <- sprintf("%-28s %-8s %6s %6s %8s %8s %10s",
                 "institution", "charter", "loans", "robust", "extreme",
                 "contra.", "$/yr")
  rows <- st[, sprintf("%-28s %-8s %6d %6d %8d %8d %10s",
                       substr(name, 1, 28), substr(charter, 1, 8),
                       total_outliers,
                       fifelse(is.na(robust_loans2), 0L,
                               as.integer(robust_loans2)),
                       extreme_cases, contradicted_reasons,
                       format(excess_dollars_yr, big.mark = ","))]
  tab <- data.table(y = seq(length(rows) + 1, 1),
                    lab = c(hdr, rows),
                    face = c("bold", rep("plain", length(rows))))
  pages[["exec_2_start_here"]] <- ggplot(tab, aes(1, y, label = lab)) +
    geom_text(hjust = 0, family = "mono", size = 3.4,
              aes(fontface = face), colour = navy) +
    xlim(1, 10) + ylim(0, length(rows) + 2) + theme_void() +
    labs(title = "  Where to start",
         subtitle = paste("  loans = for file review | robust = flagged by 2+ methods |",
                          "extreme = model gave the outcome <=5% odds\n ",
                          "contra. = institution's stated reason contradicted by its",
                          "own approvals | $/yr = pricing excess"),
         caption = "full detail: outlier_loans_2025.xlsx (Summary tabs)") +
    theme(plot.title = element_text(face = "bold", size = 18, colour = navy),
          plot.subtitle = element_text(colour = "grey35", size = 10),
          plot.caption = element_text(colour = "grey45"))
}

# ---- 3. leaderboard ---------------------------------------------------------------
lb <- ens_rk[total_loans > 0]
lb <- lb[order(cu_type, -robust_loans, -total_loans),
         head(.SD, top_n_chart), by = cu_type]
lb[, single := total_loans - robust_loans]
lb_s <- melt(lb, id.vars = c("cu_type", "name"),
             measure.vars = c("robust_loans", "single"))
lb_s[, variable := factor(variable, c("single", "robust_loans"),
                          c("one method", "2+ methods (robust)"))]
pages[["exec_3_leaderboard"]] <- ggplot(lb_s,
    aes(reorder(name, value, sum), value, fill = variable)) +
  geom_col(width = 0.72) + coord_flip() +
  geom_text(data = lb, inherit.aes = FALSE,
            aes(name, total_loans, label = total_loans),
            hjust = -0.15, size = 3.4, colour = navy) +
  facet_wrap(~ cu_type, scales = "free_y",
             labeller = labeller(cu_type = type_lab)) +
  scale_fill_manual(values = c("one method" = gray,
                               "2+ methods (robust)" = red), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, .12))) +
  labs(title = "Credit unions ranked by loans needing review",
       subtitle = "Red = the same borrowers were flagged by two or more independent methods",
       x = NULL, y = "loans identified for file review",
       caption = "counts are excess-calibrated: one loan per statistically estimated excess adverse outcome") +
  thm
pages[["exec_3_leaderboard"]] <- pages[["exec_3_leaderboard"]]

# ---- 4. convergence ----------------------------------------------------------------
top_cu <- ens_rk[order(-robust_loans, -total_loans)][1:min(top_n_chart, .N),
                                                     .(lei, name)]
cv <- ens_fl[lei %in% top_cu$lei,
             .(streams = unlist(strsplit(streams, "+", fixed = TRUE))),
             by = .(lei, group)][, .(cells = .N), by = .(lei, streams)]
cv <- merge(cv, top_cu, by = "lei")
cv[, streams := factor(streams, c("popick", "ml", "steering"),
                       c("Econometric\n(Popick)", "Machine\nlearning",
                         "Steering\n(placement)"))]
pages[["exec_4_convergence"]] <- ggplot(cv,
    aes(streams, reorder(name, cells, sum), fill = cells)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = cells), colour = "white", fontface = "bold") +
  scale_fill_gradient(low = teal, high = navy, guide = "none") +
  labs(title = "Independent methods reaching the same conclusion",
       subtitle = "Borrower groups each method flags per institution -- agreement across columns = robustness",
       x = NULL, y = NULL,
       caption = "methods share the HMDA data but differ completely in modeling approach") +
  thm + theme(panel.grid = element_blank())

# ---- 5. problem mix ----------------------------------------------------------------
pm <- ens_ln[lei %in% top_cu$lei,
             .(screen = unlist(strsplit(screens, "+", fixed = TRUE))),
             by = .(lei, uli)][, .(loans = .N), by = .(lei, screen)]
pm <- merge(pm, top_cu, by = "lei")
pm[, screen := factor(screen, c("denial", "withdrawal", "pricing",
                                "steering"),
                      c("Denials", "Withdrawals", "Pricing", "Steering"))]
pages[["exec_5_problem_mix"]] <- ggplot(pm,
    aes(reorder(name, loans, sum), loans, fill = screen)) +
  geom_col(width = 0.72) + coord_flip() +
  scale_fill_manual(values = c(Denials = red, Withdrawals = orange,
                               Pricing = navy, Steering = teal),
                    name = NULL, drop = FALSE) +
  labs(title = "What kind of problem does each institution show?",
       subtitle = "Loans for review, by the screen that identified them",
       x = NULL, y = "loans identified") + thm

# ---- 6. severity: dollars + contradicted reasons ------------------------------------
if (!is.null(smy) && smy[, sum(excess_dollars_yr, na.rm = TRUE)] > 0) {
  dd <- smy[excess_dollars_yr > 0 | contradicted_reasons > 0]
  dd <- dd[order(-excess_dollars_yr)][1:min(top_n_chart, .N)]
  pages[["exec_6_dollars"]] <- ggplot(dd,
      aes(reorder(name, excess_dollars_yr), excess_dollars_yr)) +
    geom_col(fill = red, width = 0.7) + coord_flip() +
    geom_text(aes(label = sprintf("$%s/yr | %d contradicted reasons",
                                  format(excess_dollars_yr, big.mark = ","),
                                  contradicted_reasons)),
              hjust = -0.03, size = 3.2, colour = navy) +
    scale_y_continuous(labels = function(x)
      paste0("$", format(x, big.mark = ",")),
      expand = expansion(mult = c(0, .35))) +
    labs(title = "What the findings cost borrowers",
         subtitle = "Excess interest per year + excess fees on pricing outliers; contradicted = stated denial reason defeated by the CU's own approvals",
         x = NULL, y = "excess dollars per year (pricing screen)") + thm
}

# ---- 7. size vs findings -----------------------------------------------------------
sz <- merge(ens_rk, unique(flags[, .(lei, assets_tot)]), by = "lei")
sz <- sz[total_loans > 0 & !is.na(assets_tot)]
# ---- 6b. strength of the file evidence ------------------------------------------
smy_f <- if (file.exists(out("outlier_summary_by_cu_ensemble_2025.csv"))) {
  out("outlier_summary_by_cu_ensemble_2025.csv")
} else out("outlier_summary_by_cu_2025.csv")
if (file.exists(smy_f)) {
  smev <- fread(smy_f)
  ev_cols <- intersect(c("extreme_cases", "contradicted_reasons",
                         "weaker_profile_pairs"), names(smev))
  if (length(ev_cols) >= 2 && nrow(smev)) {
    ev <- melt(head(smev[order(-total_outliers)], top_n_chart),
               id.vars = "name", measure.vars = ev_cols)
    ev[, variable := factor(variable,
          c("extreme_cases", "contradicted_reasons", "weaker_profile_pairs"),
          c("Extreme surprise (<=5% odds)", "Stated reason CONTRADICTED",
            "Weaker white profile approved"))]
    pages[["exec_6b_evidence"]] <- ggplot(ev,
        aes(reorder(name, value, sum), value, fill = variable)) +
      geom_col(position = "dodge", width = 0.75) + coord_flip() +
      scale_fill_manual(values = c(navy, red, orange), name = NULL) +
      guides(fill = guide_legend(nrow = 1)) +
      labs(title = "How strong is the file evidence?",
           subtitle = "Three classes of evidence the institution must answer loan-by-loan",
           x = NULL, y = "loans",
           caption = "CONTRADICTED = the CU's own approved white borrowers defeat its stated denial reason") +
      thm
  }
}

pages[["exec_7_size"]] <- ggplot(sz, aes(assets_tot / 1e9, total_loans,
    size = pmax(robust_loans, 1), colour = robust_loans > 0)) +
  geom_point(alpha = 0.75) +
  geom_text(data = sz[order(-total_loans)][1:min(6, .N)],
            aes(label = name), vjust = -1.1, size = 3.1, colour = navy,
            show.legend = FALSE) +
  scale_x_log10(labels = function(x) paste0("$", x, "B")) +
  scale_colour_manual(values = c(`TRUE` = red, `FALSE` = gray),
                      labels = c(`TRUE` = "robust finding",
                                 `FALSE` = "single-method"), name = NULL) +
  scale_size_continuous(guide = "none") +
  labs(title = "Findings are not just a function of size",
       subtitle = "Each point is a credit union with loans identified; assets on a log scale",
       x = "total assets (log scale)", y = "loans identified for review") +
  thm

# ---- 8. methodology notes: plain language + honest limits ---------------------------
notes <- c(
  "WHAT THE MODELS DO. Each application's outcome is predicted from financial factors",
  "only -- credit score, debt-to-income, loan-to-value, income, product, market, month.",
  "Race is never an input. Evidence = minority outcomes systematically worse than the",
  "race-blind prediction at the SAME institution.",
  "",
  "WHY THE FLAGS ARE TRUSTWORTHY. Gaps are benchmarked against peers, multiplicity is",
  "controlled (false-discovery rate), and a flag requires BOTH significance AND a gap",
  "large enough to matter. Under placebo tests (race labels shuffled), the screen",
  "flags approximately nothing.",
  "",
  "WHY THESE LOAN COUNTS. One reviewed loan per statistically estimated excess adverse",
  "outcome -- counts derive from the evidence, scale with institution size and gap,",
  "and shrink where evidence is weak.",
  "",
  "WHAT ROBUST MEANS. Flagged by 2+ methodologically independent streams. Convergence",
  "= the finding is not an artifact of any one model's assumptions.",
  "",
  "HONEST LIMITS. HMDA lacks reserves, employment detail, and full credit files; the",
  "screen identifies UNEXPLAINED disparity, not proof of discrimination. File review",
  "is the adjudication step -- this report is the map, not the verdict.")
nt <- data.table(y = seq(length(notes), 1), lab = notes)
pages[["exec_8_method_notes"]] <- ggplot(nt, aes(1, y, label = lab)) +
  geom_text(hjust = 0, size = 3.6, colour = navy,
            fontface = fifelse(grepl("^[A-Z]{3,}", nt$lab), "bold", "plain")) +
  xlim(1, 10) + ylim(0, length(notes) + 1) + theme_void() +
  labs(title = "  Methodology in plain language",
       subtitle = "  and the limits stated before anyone has to ask") +
  theme(plot.title = element_text(face = "bold", size = 18, colour = navy),
        plot.subtitle = element_text(colour = "grey35"))

# ---- save -------------------------------------------------------------------------
for (nm in names(pages)) {
  ggsave(out(paste0(nm, ".png")), pages[[nm]], width = 11, height = 8,
         dpi = 160)
  cat("Saved ->", out(paste0(nm, ".png")), "\n")
}
pdf(out("exec_report_2025.pdf"), width = 11, height = 8)
for (p in pages) print(p)
dev.off()
cat("Saved ->", out("exec_report_2025.pdf"), "(", length(pages), "pages )\n")
