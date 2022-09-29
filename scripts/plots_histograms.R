# setwd("/mnt/sda1/agrel_spinenet_resubmission/scripts")
setwd("C:\\Users\\tmcsween21\\OneDrive - Oulun yliopisto\\agrel_spinenet_resubmission")

library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(glue)

source("scripts\\old_submission\\plot_functions.R")

data <- read.csv('data\\df.csv')

print_column_names(data)

#### Colour settings ####
paired <- c(brewer.pal(n = 12, name = "Paired"))
print(paired)
pairedA <- paired[c(2,6, 4, 10)]
pairedB <- paired[c(2,4,10,6)]
pairedC <- paired[c(2,8,10,6)]
pairedD <- paired[c(2,11,10,8)]

#### Plot settings ####
colours <- "Set1"
base_size <- 20
w <- 9
h <- 6
fs <- 20
output_dir <- "figures\\"
filename <- "220928"

show_legend <- TRUE
legend_position <- "bottom"
legend_title <- element_blank() #NULL
x_ax_title <- element_blank() #NULL 

#### Pfirrmann grade histograms: ####
df <- data

df_dd_h <- df[ , c(2,24)]
df_dd_h <- rename(df_dd_h, Value = gt_pf)
df_dd_h$Group <- "Reference (radiologists)"

df_dd_m <- df[ , c(2,3)]
df_dd_m <- rename(df_dd_m, Value = Pfirrmann)
df_dd_m$Group <- "Prediction (SpineNet)"

df_dd <- rbind(df_dd_h, df_dd_m)

# Shortcut: change the values to give the facets better titles (don't know how to do this otherwise)
df_dd$Value[df_dd$Value == 2] <- "Pfirrmann grade 2"
df_dd$Value[df_dd$Value == 3] <- "Pfirrmann grade 3"
df_dd$Value[df_dd$Value == 4] <- "Pfirrmann grade 4"
df_dd$Value[df_dd$Value == 5] <- "Pfirrmann grade 5"

# Change the order so "reference" comes first:
df_dd$Group <- factor(df_dd$Group, levels=c("Reference (radiologists)",
                                            "Prediction (SpineNet)"))

p1 <- ggplot(data=df_dd, aes(x=Level, fill=Group)) +
  geom_histogram(stat = "count", position = 'dodge', color="black", show.legend = F) +
  labs(y = "")+
  facet_wrap(~Value) +
  # scale_fill_brewer(palette = dark2B, guide = guide_legend()) +
  # scale_fill_manual(values = oulu_blue_theme_2, guide = guide_legend()) +
  scale_fill_manual(values = pairedC, guide = guide_legend()) +
  theme_tufte(base_size = base_size) +
  theme(legend.position=legend_position) +
  theme(legend.title = legend_title) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=0))
print(p1)
plot_name <- glue("{output_dir}Pfirrmann_grade_histograms_{filename}.svg")
# ggsave(plot_name, plot = p1, width = w, height = h, units = "in")

#### Modic changes ####
df_mc_h_u <- df[ , c(2,28)]
df_mc_h_u <- rename(df_mc_h_u, Value = gt_mc_r)
df_mc_h_u$Zone <- "R"
df_mc_h_u$Group <- "Reference (radiologists)"

df_mc_h_l <- df[ , c(2,26)]
df_mc_h_l <- rename(df_mc_h_l, Value = gt_mc_c)
df_mc_h_l$Zone <- "C"
df_mc_h_l$Group <- "Reference (radiologists)"

df_mc_h <- rbind(df_mc_h_u, df_mc_h_l)

df_mc_m_u <- df[ , c(2,15)]
df_mc_m_u <- rename(df_mc_m_u, Value = UpperMarrow)
df_mc_m_u$Zone <- "R"
df_mc_m_u$Group <- "Prediction (SpineNet)"

df_mc_m_l <- df[ , c(2,17)]
df_mc_m_l <- rename(df_mc_m_l, Value = LowerMarrow)
df_mc_m_l$Zone <- "C"
df_mc_m_l$Group <- "Prediction (SpineNet)"

df_mc_m <- rbind(df_mc_m_u, df_mc_m_l)

df_mc <- rbind(df_mc_h, df_mc_m)

df_mc$Level <- substr(df_mc$Level,1,nchar(df_mc$Level)-3)

df_mc$Level <- paste(df_mc$Level, df_mc$Zone, sep = '')

df_mc[df_mc == "-"] <- NA
df_mc <- df_mc[complete.cases(df_mc), ]

df_mc$Value[df_mc$Value == 0] <- "Modic changes absent"
df_mc$Value[df_mc$Value == 1] <- "Modic changes present"

df_mc_0 <- df_mc[df_mc$Value == "Modic changes absent", ]
df_mc_1 <- df_mc[df_mc$Value == "Modic changes present",]

scale <- scale_x_discrete(limits = c("L1R",
                                     "L1C",
                                     "L2R",
                                     "L2C",
                                     "L3R",
                                     "L3C",
                                     "L4R",
                                     "L4C",
                                     "L5R",
                                     "L5C"))

df_mc$Group <- factor(df_mc$Group, levels=c("Reference (radiologists)",
                                            "Prediction (SpineNet)"))

p3 <- ggplot(data=df_mc, aes(x=Level, fill=Group)) +
  geom_histogram(stat = "count", position = 'dodge', color="black", show.legend = show_legend) +
  labs(y = "")+
  scale +
  facet_wrap(~Value, nrow = 2) +
  # scale_fill_brewer(palette = colours, guide = guide_legend()) +
  scale_fill_manual(values = pairedC, guide = guide_legend()) +
  theme_tufte(base_size = base_size) +
  theme(legend.position=legend_position) +
  theme(legend.title = legend_title) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=0))
print(p3)
plot_name <- glue("{output_dir}Modic_change_histograms{filename}.svg")
# ggsave(plot_name, plot = p3, width = w, height = h, units = "in")

library("cowplot")
pp <- plot_grid(p1, p3,
                ncol = 1, nrow = 2)
print(pp)
plot_name <- glue("{output_dir}dd_mc_combined_histograms_{filename}.tiff")
ggsave(plot_name, plot = pp, width = w, height = 12, units = "in", dpi = 1200)

##### Modic change width histogram #####

df_mc_w_u <- df[ , c(2,29)]
df_mc_w_u$Zone <- "R"
df_mc_w_u <- rename(df_mc_w_u, Value = gt_mc_r_w)

df_mc_w_l <- df[ , c(2,30)]
df_mc_w_l$Zone <- "C"
df_mc_w_l <- rename(df_mc_w_l, Value = gt_mc_c_w)

df_mc_w <- rbind(df_mc_w_u, df_mc_w_l)

df_mc_w$Level <- substr(df_mc_w$Level,1,nchar(df_mc_w$Level)-3)

df_mc_w$Level <- paste(df_mc_w$Level, df_mc_w$Zone, sep = '')

df_mc_w[df_mc_w == "-"] <- NA
df_mc_w <- df_mc_w[!(df_mc_w$Value == 0),]
df_mc_w <- df_mc_w[complete.cases(df_mc_w), ]

p5 <- ggplot(data=df_mc_w, aes(x=Level)) +
  geom_histogram(stat = "count", position = 'dodge', color="black",fill="#1F78B4", alpha = 0.8) +
  labs(y = "Count")+
  scale +
  facet_wrap(~Value) +
  theme_tufte(base_size = base_size) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=90))
print(p5)
plot_name <- glue("{output_dir}Modic_changes_size_histogram_{filename}.tiff")
ggsave(plot_name, plot = p5, width = w, height = h, units = "in", dpi = 1200)

#### Model performance metrics Pfirrmann grades ####
file <- "data\\pfirrmann_grading_results.csv"
df <- read.csv(file)

print_column_names(df)

metrics <- unique( df$metric )

df <- rename(df, Group = group)
df <- rename(df, Metric = metric)
df <- rename(df, Level = level)
df <- rename(df, Value = values)
df <- rename(df, Lower_CI = values_ci_l)
df <- rename(df, Upper_CI = values_ci_u)
df <- rename(df, p_value = p_values)
df <- rename(df, H0 = no_information_rate)

df$Group[df$Group == "all"] <- "All participants"
df$Group[df$Group == "lbp"] <- "LBP sub-group"
df$Metric[df$Metric == "cohen_kappa_score"] <- "Cohen's Kappa"
df$Metric[df$Metric == "matthews_corrcoef"] <- "Matthew's CC"
df$Metric[df$Metric == "accuracy_score"] <- 'Accuracy'
df$Metric[df$Metric == "balanced_accuracy_score"] <- 'Balanced accuracy'
df$Metric[df$Metric == "ccc"] <- "Lin's CCC"
df$Metric[df$Metric == "gwetsac"] <- "Gwet's AC1"
df$Metric[df$Metric == "sensitivity_micro"] <- "Sensitivity"
df$Metric[df$Metric == "specificity_micro"] <- "Specificity"
df$Level[df$Level == "all"] <- "All"
df$Level[df$Level == "L1-L2"] <- "L1"
df$Level[df$Level == "L2-L3"] <- "L2"
df$Level[df$Level == "L3-L4"] <- "L3"
df$Level[df$Level == "L4-L5"] <- "L4"
df$Level[df$Level == "L5-S1"] <- "L5"

df3 <- df                             # Replicate data
df3$Metric <- factor(df3$Metric,      # Reordering group factor levels
                     levels = c("Accuracy",
                                "Balanced accuracy",
                                "Sensitivity",
                                "Specificity",
                                "Cohen's Kappa",
                                "Matthew's CC",
                                "Lin's CCC",
                                "Gwet's AC1"))

df3 <- subset(df3, Metric!="Matthew's CC")

p6 <- ggplot(data=df3, aes(x=Level, y = Value, fill=Group)) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = show_legend, alpha = 1) +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), size = 0.6, width=.4,
                position=position_dodge(1)) +
  ggtitle("Pfirrmann Grades") +
  labs(y = "Value") +
  scale_x_discrete(limits = c("All", "L1", "L2", "L3", "L4", "L5")) +
  facet_wrap(~Metric, ncol = 3, nrow = 3) +
  # scale_fill_brewer(palette = colours, guide = guide_legend()) +
  scale_fill_manual(values = pairedC, guide = guide_legend()) +
  theme_tufte(base_size = base_size) +
  theme(legend.position=legend_position) +
  theme(legend.title = legend_title) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=0)) +
  theme(plot.title = element_text(hjust = 0.5))
print(p6)
plot_name <- glue("{output_dir}Pfirrmann_grade_metrics_supplementary_material_{filename}.tiff")
# ggsave(plot_name, plot = p6, width = 10, height = h+2, units = "in", dpi = 1200)


df3 <- subset(df3, Metric!="Accuracy")
df3 <- subset(df3, Metric!="Gwet's AC1")
# df3 <- subset(df3, Metric!="Sensitivity")
# df3 <- subset(df3, Metric!="Specificity")
df3 <- subset(df3, Metric!="Matthew's CC")

p6 <- ggplot(data=df3, aes(x=Level, y = Value, fill=Group)) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = show_legend, alpha = 1) +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), size = 0.6, width=.4,
                position=position_dodge(1)) +
  ggtitle("Pfirrmann Grades") +
  labs(y = "Value") +
  scale_x_discrete(limits = c("All", "L1", "L2", "L3", "L4", "L5")) +
  facet_wrap(~Metric, ncol = 3, nrow = 3) +
  # scale_fill_brewer(palette = colours, guide = guide_legend()) +
  scale_fill_manual(values = pairedC, guide = guide_legend()) +
  theme_tufte(base_size = base_size) +
  theme(legend.position=legend_position) +
  theme(legend.title = legend_title) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=0)) +
  theme(plot.title = element_text(hjust = 0.5))
print(p6)
plot_name <- glue("{output_dir}Pfirrmann_grade_metrics_and_sensitivity_and_specificity_{filename}.tiff")
ggsave(plot_name, plot = p6, width = w, height = 10, units = "in", dpi = 1200)

#### Model performance metrics Modic changes ####

file <- "data\\mc_grading_results.csv"
df <- read.csv(file)

print_column_names(df)

metrics <- unique( df$metric )

df <- rename(df, Group = group)
df <- rename(df, Metric = metric)
df <- rename(df, Level = level)
df <- rename(df, Value = values)
df <- rename(df, Lower_CI = values_ci_l)
df <- rename(df, Upper_CI = values_ci_u)
df <- rename(df, p_value = p_values)
df <- rename(df, H0 = no_information_rate)

df$Group[df$Group == "all"] <- "All participants"
df$Group[df$Group == "lbp"] <- "LBP sub-group"
df$Group[df$Group == "size"] <- "Large MC"
df$Group[df$Group == "lbp_size"] <- "LBP and large MC sub-group"
df$Metric[df$Metric == "cohen_kappa_score"] <- "Cohen's Kappa"
df$Metric[df$Metric == "matthews_corrcoef"] <- "Matthew's CC"
df$Metric[df$Metric == "accuracy_score"] <- 'Accuracy'
df$Metric[df$Metric == "balanced_accuracy_score"] <- 'Balanced accuracy'
df$Metric[df$Metric == "gwetsac"] <- "Gwet's AC1"
df$Metric[df$Metric == "sensitivity"] <- "Sensitivity"
df$Metric[df$Metric == "specificity"] <- "Specificity"
df$Level[df$Level == "all"] <- "All"
df$Level[df$Level == "L1-L2"] <- "L1"
df$Level[df$Level == "L2-L3"] <- "L2"
df$Level[df$Level == "L3-L4"] <- "L3"
df$Level[df$Level == "L4-L5"] <- "L4"
df$Level[df$Level == "L5-S1"] <- "L5"

df$Metric <- factor(df$Metric,      # Reordering group factor levels
                     levels = c("Accuracy",
                                "Balanced accuracy",
                                "Sensitivity",
                                "Specificity",
                                "Cohen's Kappa",
                                "Matthew's CC",
                                "Lin's CCC",
                                "Gwet's AC1"))

df$Group <- factor(df$Group,      # Reordering group factor levels
                    levels = c("All participants",
                               "Large MC",
                               "LBP sub-group",
                               "LBP and large MC sub-group"))

df <- subset(df, Metric!="Matthew's CC")

p7 <- ggplot(data=df, aes(x=Level, y = Value, fill=Group)) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = show_legend, alpha = 1) +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), size = 0.6, width=.4,
                position=position_dodge(1)) +
  ggtitle("Modic Changes") +
  labs(y = "Value")+
  scale_x_discrete(limits = c("All", "L1", "L2", "L3", "L4", "L5")) +
  facet_wrap(~Metric, ncol = 2) +
  # scale_fill_brewer(palette = "Dark2", guide = guide_legend()) +
  scale_fill_manual(values = pairedD, guide = guide_legend()) +
  theme_tufte(base_size = base_size) +
  theme(legend.position=legend_position) +
  theme(legend.title = legend_title) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=0)) +
  theme(plot.title = element_text(hjust = 0.5))
print(p7)
plot_name <- glue("{output_dir}mc_metrics_supplementary_{filename}.tiff")
ggsave(plot_name, plot = p7, width = 10, height = h+2, units = "in", dpi = 1200)


df <- subset(df, Group!="LBP sub-group")
df <- subset(df, Group!="Large MC")
df <- subset(df, Metric!="Accuracy")
# df <- subset(df, Metric!="Sensitivity")
# df <- subset(df, Metric!="Specificity")
df <- subset(df, Metric!="Matthew's CC")
df <- subset(df, Metric!="Gwet's AC1")

p7 <- ggplot(data=df, aes(x=Level, y = Value, fill=Group)) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = show_legend, alpha = 1) +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), size = 0.6, width=.4,
                position=position_dodge(1)) +
  ggtitle("Modic Changes") +
  labs(y = "Value")+
  scale_x_discrete(limits = c("All", "L1", "L2", "L3", "L4", "L5")) +
  facet_wrap(~Metric, ncol = 4) +
  # scale_fill_brewer(palette = "Dark2", guide = guide_legend()) +
  scale_fill_manual(values = pairedC, guide = guide_legend()) +
  theme_tufte(base_size = base_size) +
  theme(legend.position=legend_position) +
  theme(legend.title = legend_title) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=0)) +
  theme(plot.title = element_text(hjust = 0.5))
print(p7)
plot_name <- glue("{output_dir}mc_metrics_with_sensitivity_and_specificity_{filename}.tiff")
ggsave(plot_name, plot = p7, width = w, height = 5, units = "in", dpi = 1200)










