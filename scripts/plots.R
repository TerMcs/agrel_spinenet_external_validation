setwd("/mnt/sda1/agrel_spinenet_resubmission/")

library(tidyr)
library(tidyverse)
library(ggplot2)
library(pROC)
library(ggthemes)

df <- read.csv('data/df.csv')

#### ROC curves ####
#### MCs ####

level_select <- function(df, level) {
  df <- subset(df, Level == level)
  return(df)
}

labels <- c('All', 'L1-L2', 'L2-L3', 'L3-L4', 'L4-L5', 'L5-S1')
levels <- list('L1-L2', 'L2-L3', 'L3-L4', 'L4-L5', 'L5-S1')

df_umc <- df[,c('project_ID', 'Level', 'UpperMC_probability', 'UpperMC_correct')] 
df_umc <- rename(df_umc, MC_probability = UpperMC_probability, MC_correct = UpperMC_correct)

df_lmc <- df[,c('project_ID', 'Level', 'LowerMC_probability', 'LowerMC_correct')] 
df_lmc <- rename(df_lmc, MC_probability = LowerMC_probability, MC_correct = LowerMC_correct)

df_mc <- bind_rows(df_umc, df_lmc)

roccurve_All <- roc(df_mc$MC_correct, df_mc$MC_probability)
auc_All <- round(auc(df_mc$MC_correct, df_mc$MC_probability),2)

rocs <- list(roccurve_All)
aucs <- list(auc_All)
for (level in levels) {
  dfx <- level_select(df = df_mc, level = level)
  roccurve <- roc(dfx$MC_correct, dfx$MC_probability)
  assign(paste('roccurve_', substr(level,start = 1, stop = 2), sep = ''), roccurve)
  rocs[[length(rocs) + 1]] <- roccurve
  auc <- round(auc(dfx$MC_correct, dfx$MC_probability),2)
  assign(paste('auc_', substr(level,start = 1, stop = 2), sep = ''), auc)
  aucs[[length(aucs) + 1]] <- auc
}
print(aucs)


p1 <- ggroc(rocs, aes = c("color"), legacy.axes = TRUE)+
  scale_color_discrete(labels=labels)+
  theme_tufte() +
  coord_fixed() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black", linetype="dotted")+
  ggtitle("Receiver operator characteristic curve for Modic changes") +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       color = "Spinal levels")

p1


#### Pfirrman grades 2 ####
roccurve_All <- roc(df$Pfirrmann_class_2, df$Pfirrmann_probability_2)
auc_All <- round(auc(df$Pfirrmann_class_2, df$Pfirrmann_probability_2),2)

rocs <- list(roccurve_All)
aucs <- list(auc_All)
for (level in levels) {
  dfx <- level_select(df = df, level = level)
  roccurve <- roc(dfx$Pfirrmann_class_2, dfx$Pfirrmann_probability_2)
  assign(paste('roccurve_', substr(level,start = 1, stop = 2), sep = ''), roccurve)
  rocs[[length(rocs) + 1]] <- roccurve
  auc <- round(auc(dfx$Pfirrmann_class_2, dfx$Pfirrmann_probability_2),2)
  assign(paste('auc_', substr(level,start = 1, stop = 2), sep = ''), auc)
  aucs[[length(aucs) + 1]] <- auc
}
print(aucs)


p2 <- ggroc(rocs, aes = c("color"), legacy.axes = TRUE)+
  scale_color_discrete(labels=labels)+
  theme_tufte() +
  coord_fixed() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black", linetype="dotted")+
  ggtitle("Receiver operator characteristic curve for Pfirrmann changes") +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       color = "Spinal levels")

p2

#### Pfirrman grades 3 ####
roccurve_All <- roc(df$Pfirrmann_class_3, df$Pfirrmann_probability_3)
auc_All <- round(auc(df$Pfirrmann_class_3, df$Pfirrmann_probability_3),2)

rocs <- list(roccurve_All)
aucs <- list(auc_All)
for (level in levels) {
  dfx <- level_select(df = df, level = level)
  roccurve <- roc(dfx$Pfirrmann_class_3, dfx$Pfirrmann_probability_3)
  assign(paste('roccurve_', substr(level,start = 1, stop = 2), sep = ''), roccurve)
  rocs[[length(rocs) + 1]] <- roccurve
  auc <- round(auc(dfx$Pfirrmann_class_3, dfx$Pfirrmann_probability_3),2)
  assign(paste('auc_', substr(level,start = 1, stop = 2), sep = ''), auc)
  aucs[[length(aucs) + 1]] <- auc
}
print(aucs)


p3 <- ggroc(rocs, aes = c("color"), legacy.axes = TRUE)+
  scale_color_discrete(labels=labels)+
  theme_tufte() +
  coord_fixed() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black", linetype="dotted")+
  ggtitle("Receiver operator characteristic curve for Pfirrmann changes") +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       color = "Spinal levels")

p3

#### Pfirrman grades 4 ####
roccurve_All <- roc(df$Pfirrmann_class_4, df$Pfirrmann_probability)
auc_All <- round(auc(df$Pfirrmann_class_4, df$Pfirrmann_probability),2)

rocs <- list(roccurve_All)
aucs <- list(auc_All)
for (level in levels) {
  dfx <- level_select(df = df, level = level)
  roccurve <- roc(dfx$Pfirrmann_class_4, dfx$Pfirrmann_probability)
  assign(paste('roccurve_', substr(level,start = 1, stop = 2), sep = ''), roccurve)
  rocs[[length(rocs) + 1]] <- roccurve
  auc <- round(auc(dfx$Pfirrmann_class_4, dfx$Pfirrmann_probability),2)
  assign(paste('auc_', substr(level,start = 1, stop = 2), sep = ''), auc)
  aucs[[length(aucs) + 1]] <- auc
}
print(aucs)


p4 <- ggroc(rocs, aes = c("color"), legacy.axes = TRUE)+
  scale_color_discrete(labels=labels)+
  theme_tufte() +
  coord_fixed() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black", linetype="dotted")+
  ggtitle("Receiver operator characteristic curve for Pfirrmann changes") +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       color = "Spinal levels")

p4

#### Pfirrman grades 5 ####
roccurve_All <- roc(df$Pfirrmann_class_5, df$Pfirrmann_probability)
auc_All <- round(auc(df$Pfirrmann_class_5, df$Pfirrmann_probability),2)

rocs <- list(roccurve_All)
aucs <- list(auc_All)
for (level in levels) {
  dfx <- level_select(df = df, level = level)
  roccurve <- roc(dfx$Pfirrmann_class_5, dfx$Pfirrmann_probability)
  assign(paste('roccurve_', substr(level,start = 1, stop = 2), sep = ''), roccurve)
  rocs[[length(rocs) + 1]] <- roccurve
  auc <- round(auc(dfx$Pfirrmann_class_5, dfx$Pfirrmann_probability),2)
  assign(paste('auc_', substr(level,start = 1, stop = 2), sep = ''), auc)
  aucs[[length(aucs) + 1]] <- auc
}
print(aucs)


p5 <- ggroc(rocs, aes = c("color"), legacy.axes = TRUE)+
  scale_color_discrete(labels=labels)+
  theme_tufte() +
  coord_fixed() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black", linetype="dotted")+
  ggtitle("Receiver operator characteristic curve for Pfirrmann changes") +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       color = "Spinal levels")

p5
print(aucs)


#
library(pROC)

auc <- multiclass.roc(df$gt_pf, df$Pfirrmann_probability)
auc

roc <- plot.multiclass.roc(df$gt_pf, df$Pfirrmann_probability)

plot(auc)
print(auc$rocs)

auc <- multiclass.roc(df$Pfirrmann_class_4, df$Pfirrmann_probability_4)
roc <- plot.roc(df$Pfirrmann_class_4, df$Pfirrmann_probability_4)

print(auc$auc)
roc

auc <- multiclass.roc(df$Pfirrmann_class_3, df$Pfirrmann_probability_3)
roc <- plot.roc(df$Pfirrmann_class_3, df$Pfirrmann_probability_3)

print(auc$auc)
roc

auc <- multiclass.roc(df$Pfirrmann_class_5, df$Pfirrmann_probability_5)
roc <- plot.roc(df$Pfirrmann_class_5, df$Pfirrmann_probability)

print(auc$auc)
roc
