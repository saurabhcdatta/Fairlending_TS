# =============================================================================
# 08_exec_report.R -- EXECUTIVE GRAPHICS. Run after 07_ensemble.R.
#
# Produces presentation-ready charts (PNG) and a one-file multi-page PDF:
#   exec_1_overview.png     KPI cover: what was screened, what was found
#   exec_2_leaderboard.png  THE LIST: CUs by robust outlier loans, by charter
#   exec_3_convergence.png  WHY TRUST IT: which methods agree, per CU
#   exec_4_problem_mix.png  WHAT KIND of problem each top CU has
#   exec_5_size_vs_find.png Are we just flagging the biggest CUs? (context)
#   exec_report_2025.pdf    all pages in one file
# =============================================================================

library(data.table)
library(ggplot2)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
top_n_chart <- 12          # CUs shown per chart
# ------------------------------------------------------------------------------

.need <- function(f) {
  if (!file.exists(out(f)))
    stop(f, " not found -- run the chain through 07_ensemble.R first.",
         call. = FALSE)
  fread(out(f), colClasses = list(character = "lei"))
}
ens_rk <- .need("ensemble_rankings_2025.csv")
ens_fl <- .need("ensemble_flags_2025.csv")
ens_ln <- .need("ensemble_loans_2025.csv")
flags  <- .need("flags_2025.csv")

navy <- "#1F3864"; red <- "#C0392B"; orange <- "#E67E22"
teal <- "#1F7A6D"; gray <- "#B7BEC9"
type_lab <- c("0" = "charter unknown", "1" = "Federal charter",
              "2" = "State charter")
thm <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(colour = "grey30"),
        plot.caption = element_text(colour = "grey45", size = 9),
        panel.grid.minor = element_blank(),
        legend.position = "top")

pages <- list()

# ---- 1. KPI cover ---------------------------------------------------------------
streams_run <- sort(unique(unlist(strsplit(ens_fl$streams, "+", fixed = TRUE))))
kpi <- data.table(
  x = 1, y = c(5:1),
  txt = c(sprintf("%s credit unions screened across %s statistical cells",
                  format(uniqueN(flags$lei), big.mark = ","),
                  format(nrow(flags), big.mark = ",")),
          sprintf("%d analysis streams run: %s",
                  length(streams_run), paste(streams_run, collapse = ", ")),
          sprintf("%d institutions show MULTI-METHOD (robust) findings",
                  ens_rk[robust_loans > 0, .N]),
          sprintf("%s individual loans identified for file review (%s robust)",
                  format(nrow(ens_ln), big.mark = ","),
                  format(sum(ens_ln$robust), big.mark = ",")),
          "Robust = the same institution and borrower group is flagged by 2+ independent methods"))
p1 <- ggplot(kpi, aes(x, y, label = txt)) +
  geom_text(hjust = 0, size = c(5.2, 5.2, 5.6, 5.2, 3.8),
            fontface = c("plain", "plain", "bold", "plain", "italic"),
            colour = c(navy, navy, red, navy, "grey40")) +
  xlim(1, 10) + ylim(0.5, 5.8) + theme_void() +
  labs(title = "  2025 Fair-Lending Screen -- Executive Summary",
       subtitle = "  Race-blind models + machine learning + unsupervised placement analysis") +
  theme(plot.title = element_text(face = "bold", size = 18, colour = navy),
        plot.subtitle = element_text(colour = "grey35"))
pages[["exec_1_overview"]] <- p1

# ---- 2. the leaderboard -----------------------------------------------------------
lb <- ens_rk[robust_loans > 0 | total_loans > 0]
lb <- lb[order(cu_type, -robust_loans, -total_loans),
         head(.SD, top_n_chart), by = cu_type]
lb_m <- melt(lb, id.vars = c("cu_type", "name"),
             measure.vars = c("robust_loans", "total_loans"))
lb_m <- lb_m[!(variable == "total_loans")]        # robust drives the story
lb[, single := total_loans - robust_loans]
lb_s <- melt(lb, id.vars = c("cu_type", "name"),
             measure.vars = c("robust_loans", "single"))
lb_s[, variable := factor(variable, c("single", "robust_loans"),
                          c("one method", "2+ methods (robust)"))]
p2 <- ggplot(lb_s, aes(reorder(name, value, sum), value, fill = variable)) +
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
       caption = "Loan counts are excess-calibrated: one loan per statistically estimated excess adverse outcome") +
  thm
pages[["exec_2_leaderboard"]] <- p2

# ---- 3. convergence: which methods agree ------------------------------------------
top_cu <- ens_rk[order(-robust_loans, -total_loans)][1:min(top_n_chart, .N),
                                                     .(lei, name)]
cv <- ens_fl[lei %in% top_cu$lei,
             .(streams = unlist(strsplit(streams, "+", fixed = TRUE))),
             by = .(lei, group)]
cv <- cv[, .(cells = .N), by = .(lei, streams)]
cv <- merge(cv, top_cu, by = "lei")
cv[, streams := factor(streams, c("popick", "ml", "steering"),
                       c("Econometric\n(Popick)", "Machine\nlearning",
                         "Steering\n(placement)"))]
p3 <- ggplot(cv, aes(streams, reorder(name, cells, sum), fill = cells)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = cells), colour = "white", fontface = "bold") +
  scale_fill_gradient(low = teal, high = navy, guide = "none") +
  labs(title = "Independent methods reaching the same conclusion",
       subtitle = "Number of borrower groups each method flags at each institution -- agreement across columns = robustness",
       x = NULL, y = NULL,
       caption = "Methods share the HMDA data but differ completely in modeling approach") +
  thm + theme(panel.grid = element_blank())
pages[["exec_3_convergence"]] <- p3

# ---- 4. the nature of each CU's problem --------------------------------------------
pm <- ens_ln[lei %in% top_cu$lei,
             .(screen = unlist(strsplit(screens, "+", fixed = TRUE))),
             by = .(lei, uli)][, .(loans = .N), by = .(lei, screen)]
pm <- merge(pm, top_cu, by = "lei")
pm[, screen := factor(screen, c("denial", "withdrawal", "pricing", "steering"),
                      c("Denials", "Withdrawals", "Pricing", "Steering"))]
p4 <- ggplot(pm, aes(reorder(name, loans, sum), loans, fill = screen)) +
  geom_col(width = 0.72) + coord_flip() +
  scale_fill_manual(values = c(Denials = red, Withdrawals = orange,
                               Pricing = navy, Steering = teal), name = NULL) +
  labs(title = "What kind of problem does each institution show?",
       subtitle = "Loans for review by the screen that identified them",
       x = NULL, y = "loans identified") +
  thm
pages[["exec_4_problem_mix"]] <- p4

# ---- 5. context: size vs findings ----------------------------------------------------
sz <- merge(ens_rk, unique(flags[, .(lei, assets_tot)]), by = "lei")
sz <- sz[total_loans > 0 & !is.na(assets_tot)]
p5 <- ggplot(sz, aes(assets_tot / 1e9, total_loans,
                     size = pmax(robust_loans, 1),
                     colour = robust_loans > 0)) +
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
       subtitle = "Each point is a credit union with loans identified; institution assets on a log scale",
       x = "total assets (log scale)", y = "loans identified for review") +
  thm
pages[["exec_5_size_vs_find"]] <- p5

# ---- save: PNGs + one PDF -------------------------------------------------------------
for (nm in names(pages)) {
  ggsave(out(paste0(nm, ".png")), pages[[nm]],
         width = 11, height = 6.5, dpi = 160)
  cat("Saved ->", out(paste0(nm, ".png")), "\n")
}
pdf(out("exec_report_2025.pdf"), width = 11, height = 6.5)
for (p in pages) print(p)
dev.off()
cat("Saved ->", out("exec_report_2025.pdf"), "\n")
