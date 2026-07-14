# =============================================================================
# 05_report.R  --  two pictures and a summary table. Nothing fancy.
#
# Output:  report_flag_gaps.png     flagged CU x group gaps, worst first
#          report_gap_density.png   where flagged cells sit vs everyone else
#          summary_2025.csv         one line per screen
# =============================================================================

library(data.table)
library(ggplot2)

source("settings.R")

flags <- fread(out("flags_2025.csv"))

# --- 1. flagged cells, worst first ------------------------------------------------
top <- flags[flag == 1][order(-gap)][1:min(20, sum(flags$flag))]
if (nrow(top) > 0 && !all(is.na(top$lei))) {
  top[, label := paste(fifelse(!is.na(name) & name != "", name,
                               as.character(cu_number)), group, sep = " / ")]
  p1 <- ggplot(top, aes(x = reorder(label, gap),
                        y = gap, fill = tier)) +
    scale_fill_manual(values = c(high = "#C0392B", flag = "#E67E22")) +
    geom_col() + coord_flip() +
    labs(title = "Flagged credit union x group residual gaps, 2025",
         subtitle = "Gap = group mean residual minus white mean residual at the same CU",
         x = NULL, y = "Residual gap") +
    theme_minimal(base_size = 11)
  ggsave(out("report_flag_gaps.png"), p1, width = 9, height = 6, dpi = 150)
  cat("Saved ->", out("report_flag_gaps.png"), "\n")
} else cat("No flags -- skipping the flag chart.\n")

# --- 2. flagged vs unflagged gap distribution ----------------------------------------
p2 <- ggplot(flags, aes(x = gap, fill = factor(flag))) +
  geom_histogram(bins = 60, alpha = 0.75, position = "identity") +
  facet_wrap(~ screen, scales = "free") +
  scale_fill_manual(values = c(`0` = "grey70", `1` = "#C0392B"),
                    labels = c("not flagged", "flagged"), name = NULL) +
  labs(title = "Residual gaps across all tested CU x group cells, 2025",
       x = "Residual gap vs white applicants at the same CU", y = "Cells") +
  theme_minimal(base_size = 11)
ggsave(out("report_gap_density.png"), p2, width = 9, height = 5, dpi = 150)
cat("Saved ->", out("report_gap_density.png"), "\n")

# --- 3. one-line-per-screen summary ---------------------------------------------------
grp <- if ("cu_type" %in% names(flags)) c("cu_type", "screen") else "screen"
summary <- flags[, .(cells_tested = .N, cus_tested = uniqueN(lei),
                     high = sum(tier == "high"), flag = sum(tier == "flag"),
                     watch = sum(tier == "watch"),
                     flagged_cus = uniqueN(lei[flag == 1]),
                     median_gap = median(gap)), by = grp]
print(summary)
fwrite(summary, out("summary_2025.csv"))
cat("Saved ->", out("summary_2025.csv"), "\n")
