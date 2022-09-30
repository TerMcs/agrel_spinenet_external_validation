setwd("/mnt/sda1/agrel_spinenet_resubmission")
library(haven)
library(ggplot2)
library(tidyverse)

file <- "./data/df.csv"
df1 <- read_csv(file)
file <- "../data/nfbc/p0741_c6646_poiminta.sas7bdat"
df2 <- read_sas(file)

df = merge(x=df1,y=df2,by="project_ID")
df <- df[!duplicated(df$project_ID),]


for (i in 1:ncol(df)) {
  print(paste(i," - ",
              colnames(df[i]),
              " - ",
              class(df[[i]]),
              " - ",
              attributes(df[[i]])))
}

df <- df[!duplicated(df[c('project_ID')]), ]

summarise(df, mpg_mean=mean(mpg),mpg_median=median(mpg))
summarise_all(df["gender"])
str(df["gender"])
summary(df["gender"])
table(df["gender"])
str(df["C6646Q1_39"])
summary(df["C6646Q1_39"])
table(df["C6646Q1_39"])
summary(df["C6646Q1_39_1"])
table(df["C6646Q1_39_1"])
table(df["C6646Q1_98_6_2.y"])
summary(df["C6646C_weigth_height_007"])

df1 <- subset(df, C6646C_weigth_height_007 < 25)
summary(df1["C6646C_weigth_height_007"])
sd(df1$C6646C_weigth_height_007)

df2 <- subset(df, C6646C_weigth_height_007 >= 25 & C6646C_weigth_height_007 <= 30)
summary(df2["C6646C_weigth_height_007"])
sd(df2$C6646C_weigth_height_007)

df3 <- subset(df, C6646C_weigth_height_007 > 30)
summary(df3["C6646C_weigth_height_007"])
sd(df3$C6646C_weigth_height_007)

