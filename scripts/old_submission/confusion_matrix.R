"""
This requires input from the python scripts to get the correctly transformed datasets.
Just a confusion matrix plot.
"""

setwd("C:\\Users\\tmcsween21\\OneDrive - Oulun yliopisto\\agrel_spinenet\\scripts")

library(cvms)
library(ggplot2)


######## Pfirrmann grades ########
file <- "../data/dd_long.csv"
df <- read.csv(file)

targets <- df$Human_Grade
predictions <- df$Machine_Grade

cm <- as.data.frame(table(predictions, targets))

ggplot(cm, aes(targets, predictions, fill = Freq)) + 
  geom_tile() +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "#C9F0FF", high = "#23408F") +
  theme_minimal() +
  theme(legend.position = "none")

cm <- confusion_matrix(
        targets,
        predictions,
        c_levels = NULL,
        do_one_vs_all = TRUE,
        parallel = FALSE
      )

plot_confusion_matrix(cm,
                      add_row_percentages = T,
                      add_col_percentages = T,
                      diag_percentages_only = F,
                      place_x_axis_above = F,
                      palette = "Blues",
                      intensity_by = "counts",
                      theme_fn = ggplot2::theme_minimal,
                      font_counts = font(size = 10),
                      font_normalized = font(),
                      font_row_percentages = font(),
                      font_col_percentages = font(),
                      arrow_size = 0.048,
                      arrow_nudge_from_text = 0.065,
                      darkness = 0.8)

######## Modic changes ########
file <- "../data/mc_long.csv"
df <- read.csv(file)

targets <- df$Human_Grade
predictions <- df$Machine_Grade

cm <- confusion_matrix(
  targets,
  predictions,
  c_levels = NULL,
  do_one_vs_all = TRUE,
  parallel = FALSE
)

plot_confusion_matrix(cm,
                      add_row_percentages = T,
                      add_col_percentages = T,
                      diag_percentages_only = F,
                      place_x_axis_above = F,
                      palette = "Blues",
                      intensity_by = "counts",
                      theme_fn = ggplot2::theme_minimal,
                      darkness = 0.8)

