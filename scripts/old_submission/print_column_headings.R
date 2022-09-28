setwd("C:\\Users\\tmcsween21\\OneDrive - Oulun yliopisto\\agrel_spinenet")

library(haven)
library(ggplot2)
library(tidyverse)

file <- "D:\\NFBC\\p0741_c6646_poiminta.sas7bdat"

df <- read_sas(file, NULL)
print(files[j])
for (i in 1:ncol(df)) {
  print(paste(i," - ",
              colnames(df[i]),
              " - ",
              class(df[[i]]),
              " - ",
              attributes(df[[i]])))
}