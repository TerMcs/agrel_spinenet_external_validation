setwd("C:\\Users\\tmcsween21\\OneDrive - Oulun yliopisto\\agrel_spinenet")

library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(glue)
library(scales)
library(cvms)
library(ggimage)
library(rsvg)
source("scripts/plot_functions.R")

##### Histograms #####
file <- "data\\df_ints_original_names.csv"
# file <- "data\\lbp_subgroup.csv"
# file <- "data\\size_subgroup.csv"
# file <- "data\\lbp_size_subgroup.csv"
df <- read.csv(file)

print_column_names(df)

##### Oulu theme colours (for poster) ########
oulu_theme_cols <- c("#662D91",
                     "#4BBCA9", 
                     "#EB008C", 
                     "#00AEEF", 
                     "#7575D1", 
                     "#4597A0", 
                     "#94AAE5", 
                     "#FFFA99",
                     "#FFF200",
                     "#23408F",
                     "#CAD4F2",
                     "#5F7FD8",
                     "#E1CEF0",
                     "#A66CD2",
                     "#C9F0FF",
                     "#5CD3FF",
                     "#FFC8E9",
                     "#FF5ABC",
                     "#39B54A",
                     "#D6F2DA",
                     "#83D88F")
show_col(oulu_theme_cols)
oulu_theme_colours <- c("#FF5ABC", "#5CD3FF")
oulu_theme_colours <- c("#5F7FD8", "#28E2E5")
oulu_theme_colours <- c("#EB008C", 
                        "#00AEEF")
show_col(oulu_theme_colours)

##### Colour and other settings ############
display.brewer.all(colorblindFriendly = T)
dark2 <- c(brewer.pal(n = 8, name = "Dark2"))
print(dark2)
show_col(dark2)
dark2A <- dark2[c(1,2)]
dark2B <- dark2[c(3,6)]
show_col(dark2B)

set2 <- c(brewer.pal(n = 8, name = "Set2"))
print(set2)
show_col(set2)
set2A <- set2[c(2,3)]
set2B <- set2[c(3,6)]
show_col(set2A)

paired <- c(brewer.pal(n = 12, name = "Paired"))
print(paired)
show_col(paired)
pairedA <- paired[c(2,6, 4, 10)]
pairedB <- paired[c(2,4,10,6)]
pairedC <- paired[c(2,8,10,6)]
pairedD <- paired[c(2,11,10,8)]
show_col(pairedB)

colours <- "Set1"
base_size <- 20
w <- 9
h <- 6
fs <- 20
output_dir <- "figures\\"
filename <- "220308b"

show_legend <- TRUE
legend_position <- "bottom"
legend_title <- element_blank() #NULL
x_ax_title <- element_blank() #NULL 

mc_h<-c(3,73:82)
mc_m<-c(3,35:44)
dd_h<-c(3,68:72)
dd_m<-c(3,5:9)
mc_height<-c(3,86,89,92,95,98,101,104,107,110,113)
mc_width<-c(3,87,90,93,96,99,102,105,108,111,114)
mc_type<-c(3,85,88,91,94,97,100,103,106,109,112)
pain <- c(3,83)

##### Disc Degeneration histograms ##### 

df_dd_h <- df[,dd_h]
df_dd_h <- rename(df_dd_h,
                  "L1-L2" = L1L2ddmanev,
                  "L2-L3" = L2L3ddmanev,
                  "L3-L4" = L3L4ddmanev,
                  "L4-L5" = L4L5ddmanev,
                  "L5-S1" = L5S1ddmanev
                 )

df_dd_m <- df[,dd_m]
df_dd_m <- rename(df_dd_m,
                  "L1-L2" = DDL12,
                  "L2-L3" = DDL23,
                  "L3-L4" = DDL34,
                  "L4-L5" = ddL45,
                  "L5-S1" = ddL5S1
)

df_dd_h <- df_dd_h %>%
  gather(key="Level", value="Value", 2:6)
df_dd_h$Group <- "Reference (radiologists)"

df_dd_m <- df_dd_m %>%
  gather(key="Level", value="Value", 2:6)
df_dd_m$Group <- "Prediction (SpineNet)"

df_dd <- rbind(df_dd_h, df_dd_m)
df_dd[df_dd == 1] <- 2
df_dd[df_dd == "-"] <- NA
df_dd <- df_dd[complete.cases(df_dd), ]

# Shortcut: change the values to give the facets better titles (don't know how to do this otherwise)
df_dd$Value[df_dd$Value == 2] <- "Pfirrmann grade 2"
df_dd$Value[df_dd$Value == 3] <- "Pfirrmann grade 3"
df_dd$Value[df_dd$Value == 4] <- "Pfirrmann grade 4"
df_dd$Value[df_dd$Value == 5] <- "Pfirrmann grade 5"

# Change the order so "reference" comes first:
df_dd$Group <- factor(df_dd$Group, levels=c("Reference (radiologists)",
                                            "Prediction (SpineNet)"))


# Plot 4 histograms (one for each Pfirrmann grade)
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
plot_name <- glue("{output_dir}dd{filename}_220315b.tiff")
ggsave(plot_name, plot = p1, width = w, height = h, units = "in")

##### Modic change presence histograms #####

df_mc_h <- df[,mc_h]
df_mc_h <- rename(df_mc_h,
                  "L1R" = uL12mcmanev,
                  "L1C" = lL12mcmanev,
                  "L2R" = uL23mcmanev,
                  "L2C" = lL23mcmanev,
                  "L3R" = uL34mcmanev,
                  "L3C" = lL34mcmanev,
                  "L4R" = uL45mcmanev,
                  "L4C" = lL45mcmanev,
                  "L5R" = uL5S1mcmanev,
                  "L5C" = lL5S1mcmanev
)

df_mc_m <- df[,mc_m]
df_mc_m <- rename(df_mc_m,
                  "L1R" = umcL12,
                  "L1C" = lmcL12,
                  "L2R" = umcL23,
                  "L2C" = lmcL23,
                  "L3R" = umcL34,
                  "L3C" = lmcL34,
                  "L4R" = umcL45,
                  "L4C" = lmcL45,
                  "L5R" = umcL5S1,
                  "L5C" = lmcL5S1
)

df_mc_h <- df_mc_h %>%
  gather(key="Level", value="Value", 2:11)
df_mc_h$Group <- "Reference (radiologists)"

df_mc_m <- df_mc_m %>%
  gather(key="Level", value="Value", 2:11)
df_mc_m$Group <- "Prediction (SpineNet)"



df_mc <- rbind(df_mc_h, df_mc_m)
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

# Change the order so "reference" comes first:
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
plot_name <- glue("{output_dir}mc{filename}_220315.tiff")
ggsave(plot_name, plot = p3, width = w, height = h, units = "in")

############# combine plots on one page ##########

library("cowplot")
pp <- plot_grid(p1, p3,
          ncol = 1, nrow = 2)
plot_name <- glue("{output_dir}_dd_mc_combined_220315.tiff")
ggsave(plot_name, plot = pp, width = w, height = 12, units = "in")


##### Modic change height histogram #####
# Height (although this was not used in any statistical analysis so shouldn't be included)
# df_mc_height <- df[,mc_height]
# df_mc_height <- rename(df_mc_height,
#                        "L1-L2 R" = v1rh,
#                        "L1-L2 C" = v1ch,
#                        "L2-L3 R" = v2rh,
#                        "L2-L3 C" = v2ch,
#                        "L3-L4 R" = v3rh,
#                        "L3-L4 C" = v3ch,
#                        "L4-L5 R" = v4rh,
#                        "L4-L5 C" = v4ch,
#                        "L5-S1 R" = v5rh,
#                        "L5-S1 C" = v5ch)
# 
# df_mc_height[df_mc_height == "-"] <- NA
# df_mc_height[df_mc_height == "2.3"] <- NA
# df_mc_height <- df_mc_height[complete.cases(df_mc_height), ]
# 
# df_mc_height <- df_mc_height %>%
#   gather(key="Level", value="Value", 2:11)
# df_mc_height$Group <- "Height"
# 
# df_mc_height <- df_mc_height[!(df_mc_height$Value == 0),]
# 
# # df_mc_height$Value[df_mc_height$Value == 0] <- "Not visible" # remove?
# df_mc_height$Value[df_mc_height$Value == 1] <- "Less than 25% VB"
# df_mc_height$Value[df_mc_height$Value == 2] <- ""
# df_mc_height$Value[df_mc_height$Value == 3] <- ""
# df_mc_height$Value[df_mc_height$Value == 4] <- ""
# 
# p4 <- ggplot(data=df_mc_height, aes(x=Level)) +
#   geom_histogram(stat = "count", position = 'dodge') +
#   labs(y = "Count")+
#   scale +
#   facet_wrap(~Value) +
#   theme_tufte(base_size = 20) +
#   theme(legend.title = element_blank()) +
#   theme(text = element_text(size = fs)) +
#   theme(axis.text.x = element_text(angle=0))
# print(p4)

##### Modic change width histogram #####
df_mc_width <- df[,mc_width]
df_mc_width <- rename(df_mc_width,
                       "L1R" = v1rw,
                       "L1C" = v1cw,
                       "L2R" = v2rw,
                       "L2C" = v2cw,
                       "L3R" = v3rw,
                       "L3C" = v3cw,
                       "L4R" = v4rw,
                       "L4C" = v4cw,
                       "L5R" = v5rw,
                       "L5C" = v5cw)

# "L1-L2 R" = v1rw,
# "L1-L2 C" = v1cw,
# "L2-L3 R" = v2rw,
# "L2-L3 C" = v2cw,
# "L3-L4 R" = v3rw,
# "L3-L4 C" = v3cw,
# "L4-L5 R" = v4rw,
# "L4-L5 C" = v4cw,
# "L5-S1 R" = v5rw,
# "L5-S1 C" = v5cw

df_mc_width <- df_mc_width %>%
  gather(key="Level", value="Value", 2:11)
df_mc_width$Group <- "Width"

# df_mc_width[df_mc_width == "-"] <- NA
df_mc_width <- df_mc_width[!(df_mc_width$Value == 0),]
# df_mc_width <- df_mc_width[complete.cases(df_mc_width), ]

p5 <- ggplot(data=df_mc_width, aes(x=Level)) +
  geom_histogram(stat = "count", position = 'dodge', color="black",fill="#1F78B4", alpha = 0.8) +
  labs(y = "Count")+
  scale +
  facet_wrap(~Value) +
  theme_tufte(base_size = base_size) +
  theme(text = element_text(size = fs)) +
  theme(axis.title.x = x_ax_title) +
  theme(axis.text.x = element_text(angle=90))
print(p5)
plot_name <- glue("{output_dir}mc_size{filename}.tiff")
# ggsave(plot_name, plot = p5, width = w, height = h, units = "in")

##### Pain severity #####

p <- df[,pain]
p <- rename(p, "PainSeverity" = C6646Q1_98_6_2)

p6 <- ggplot(data=p, aes(x=PainSeverity)) +
  geom_histogram(stat = "count", position = 'dodge', color="black",fill="#66C2A5") +
  labs(x = "Pain Severity", y = "Count")+
  theme_tufte(base_size = 20) +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = fs + 15)) +
  theme(axis.text.x = element_text(angle=0))
print(p6)

# ggsave("../output/pain.pdf", plot = p6, width = 9.57, height = 8, units = "in")



colours <- "Accent"

base_size <- 20
w <- 9.57
h <- 6
fs <- 30
output_dir <- "output\\"
filename <- "_v3"

show_legend <- TRUE
legend_position <- "bottom"
legend_title <- element_blank() #NULL
x_ax_title <- element_blank() 

##### Metrics DD #####

file <- "results\\results_dd.csv"
df <- read.csv(file)



print_column_names(df)

df$Group[df$Group == "all"] <- "All participants"
df$Group[df$Group == "lbp"] <- "LBP sub-group"
df$Metric[df$Metric == "kappa"] <- "Cohen's Kappa"
df$Metric[df$Metric == "mcc"] <- "Matthew's CC"
df$Metric[df$Metric == "ccc"] <- "Lin's CCC"
df$Metric[df$Metric == "gwets AC"] <- "Gwet's AC1"
df$Metric[df$Metric == "sensitivity"] <- "Sensitivity"
df$Metric[df$Metric == "specificity"] <- "Specificity"

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

# "Balanced accuracy" = bal_acc,
# "Cohen's Kappa" = kappa,
# "Lin's CC" = lcc,
# "Gwet's AC 2" = gwets2

df3 <- subset(df3, Metric!="Accuracy")
df3 <- subset(df3, Metric!="Gwet's AC1")
df3 <- subset(df3, Metric!="Accuracy")
df3 <- subset(df3, Metric!="Sensitivity")
df3 <- subset(df3, Metric!="Specificity")
df3 <- subset(df3, Metric!="Matthew's CC")
df3 <- subset(df3, Level!="L2-S1")
df3 <- subset(df3, Level!="L1-L5")

p6 <- ggplot(data=df3, aes(x=Level, y = Value, fill=Group)) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = show_legend, alpha = 1) +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), size = 0.6, width=.4,
                position=position_dodge(1)) +
  ggtitle("Pfirrmann Grades") +
  labs(y = "Value")+
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
plot_name <- glue("{output_dir}dd_metrics{filename}_220315.tiff")
ggsave(plot_name, plot = p6, width = w, height = h, units = "in")

##### Metrics MC #####

file <- "results\\results_mc.csv"
df <- read.csv(file)

df$Group[df$Group == "all"] <- "All participants"
df$Group[df$Group == "lbp"] <- "LBP"
df$Group[df$Group == "mc_big"] <- "Large MC"
df$Group[df$Group == "mc_big_lbp"] <- "LBP and large MC sub-group"
df$Metric[df$Metric == "accuracy"] <- "Accuracy"
df$Metric[df$Metric == "kappa"] <- "Cohen's Kappa"
df$Metric[df$Metric == "mcc"] <- "Matthew's CC"
df$Metric[df$Metric == "gwets AC"] <- "Gwet's AC1"
df$Metric[df$Metric == "sensitivity"] <- "Sensitivity"
df$Metric[df$Metric == "specificity"] <- "Specificity"

# df <- subset(df, Group!="LBP")
# df <- subset(df, Group!="Large MC")
# df <- subset(df, Metric!="Matthew's correlation coefficient")
# df <- subset(df, Metric!="Accuracy")
# df <- subset(df, Metric!="Sensitivity")
# df <- subset(df, Metric!="Specificity")
df <- subset(df, Metric!="Matthew's CC")
# df <- subset(df, Metric!="Gwet's AC1")
df <- subset(df, Level!="L2-S1")
df <- subset(df, Level!="L1-L5")

df$Metric <- factor(df$Metric,      # Reordering group factor levels
                     levels = c("Accuracy",
                                "Balanced accuracy",
                                "Sensitivity",
                                "Specificity",
                                "Cohen's Kappa",
                                "Matthew's CC",
                                "Gwet's AC1"))


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
plot_name <- glue("{output_dir}mc_metrics_{filename}_suppl.tiff")
ggsave(plot_name, plot = p7, width = w, height = 9, units = "in")



##### Confusion matrices #####
fs = 1
intensity_by <- "counts"
colours <- "Blues"# "normalized"
font <- "serif"
base_size <- 20
w <- 4
h <- 5
fs <- 3.5
fspc <- 6
output_dir <- "..\\figures\\"


file1 <- "data/dd_long.csv"
df1 <- read.csv(file1)

targets1 <- df1$Human_Grade
predictions1 <- df1$Machine_Grade

cm_dd <- confusion_matrix(
  targets1,
  predictions1,
  c_levels = NULL,
  do_one_vs_all = F,
  parallel = T
)

cm_dd <- plot_confusion_matrix(cm_dd,
                      add_row_percentages = F,
                      add_col_percentages = F,
                      diag_percentages_only = F,
                      place_x_axis_above = F,
                      palette = colours,
                      intensity_by = intensity_by,
                      theme_fn = ggplot2::theme_minimal,
                      font_counts = font(size = fs, family = font),
                      font_normalized = font(size = fspc,family = font),
                      font_row_percentages = font(family = font),
                      font_col_percentages = font(family = font),
                      arrow_size = 0.09,
                      arrow_nudge_from_text = 0.09,
                      darkness = 1) 
print(cm_dd)
plot_name <- glue("{output_dir}dd_cm.tiff")
ggsave(plot_name, plot = cm_dd, width = h, height = h, units = "in")

file <- "data/mc_long.csv"
df <- read.csv(file)

targets <- df$Human_Grade
predictions <- df$Machine_Grade

cm_mc <- confusion_matrix(
  targets,
  predictions,
  c_levels = NULL,
  do_one_vs_all = F,
  parallel = T
)

cm_mc <- plot_confusion_matrix(cm_mc,
                               add_row_percentages = F,
                               add_col_percentages = F,
                               diag_percentages_only = F,
                               place_x_axis_above = F,
                               palette = colours,
                               intensity_by = intensity_by,
                               theme_fn = ggplot2::theme_minimal,
                               font_counts = font(size = fs, family = font),
                               font_normalized = font(size = fspc,family = font),
                               font_row_percentages = font(family = font),
                               font_col_percentages = font(family = font),
                               arrow_size = 0.09,
                               arrow_nudge_from_text = 0.09,
                               darkness = 1)
print(cm_mc)
plot_name <- glue("{output_dir}mc_cm.tiff")
ggsave(plot_name, plot = cm_mc, width = h, height = h, units = "in")
  