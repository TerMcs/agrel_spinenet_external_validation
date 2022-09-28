library(haven)
library(ggplot2)
library(tidyverse)

setwd("//kaappi.oulu.fi/nfbc$/Projects/P0469/")
file <- "p0469_spinenet_koneluenta_gradin.sas7bdat"
df1 <- read_sas(file)
file <- "p0469_c6646_poiminta.sas7bdat"
df2 <- read_sas(file)

df = merge(x=df1,y=df2,by="project_ID")

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
summarise_all(df["gender.x"])
str(df["gender.x"])
summary(df["gender.x"])
table(df["gender.x"])
str(df["C6646Q1_39"])
summary(df["C6646Q1_39"])
table(df["C6646Q1_98_6_2"])
