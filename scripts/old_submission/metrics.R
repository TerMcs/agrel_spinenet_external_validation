"""
This has been completely replaced by the python scripts, because there were errors, especially in the Modic change sizes.
Have kept it here for reference, might be some useful functions. 
"""

################### Library  ########################################

library(caret)
library(gmodels)
library(epiR)
library(Hmisc)
library(irrCAC)
library(mltools)
library(tidyr)
library(ggplot2)

################### File prep function  ########################################

prep_df <- function (file, columns, dd, lbp_level) {
  
  # Load file
  df <- read.csv(file)
  
  # Filter by LBP incidence
  df <- df[df$C6646Q1_98_6_2 >= lbp_level, ]
  
  # create dataframes with relevant columns
  df <- df[, columns]
  
  # replace "-" with NA
  df[df == "-"] <- NA
  
  # ensure all entries are integers
  for (j in 1:ncol(df)) {
    for (i in 1:nrow(df)) {
      df[i, j] <- as.integer(df[i,j])
    }
  }
  
  if (dd) {
    # Combine machine grade 1 and 2 as 2 to match human evaluations
    df[df == 1] <- 2
  }

  df_before <- df
  df <- df[complete.cases(df), ]

  incomplete_data <- nrow(df_before) - nrow(df)
  print("Original number of observations:")
  print(nrow(df_before))
  print("Number of observations removed due to incomplete data:")
  print(incomplete_data)
  
  return(df)
}


###############   Main statistics function ####################################

cm <- function (df, dd, machine_cols, human_cols, save_name) { 
  
  # Column vectors should follow order of vertebral level from rostral to caudal
  # machine first, then human ratings.

  if (dd) {
    if (length(machine_cols) > 1) {
      machine <- gather(df[, machine_cols])
      human <- gather(df[, human_cols])
      
      machine <- as.vector(machine$value)
      human <- as.vector(human$value)
    } else {
      machine <- df_dd[[machine_cols]]
      human <- df_dd[[human_cols]]
    }
    
    machine_summary <- describe(machine)
    human_summary <- describe(human)
    
    machine_f  <- factor(machine)
    human_f  <- factor(human)
    
    new_df <- data.frame(human, machine)
    names(new_df)[1] <- "Human_Grade"
    names(new_df)[2] <- "Machine_Grade"
    write.csv(new_df, save_name)
    
    # Confusion matrix/cross table:
    cm <- confusionMatrix(data=machine_f, 
                       reference=human_f)
    
    # extract the matrix:
    list <- cm[2]
    table <- list$table
    vector <- c(table[[1]], table[[5]], table[[9]], table[[13]],
                table[[2]], table[[6]], table[[10]], table[[14]],
                table[[3]], table[[7]], table[[11]], table[[15]],
                table[[4]], table[[8]], table[[12]], table[[16]])
    matrix <- matrix(vector, nrow=4, ncol = 4, byrow = TRUE)
    
    # Matthew's correlation coefficient:
   # mcc <- mcc(confusionM = matrix)
    mcc <- mcc(preds = machine_f, actuals = human_f)
    
    # Gwet's AC2 (ordinal weights) - for ordinal data: 
    gwetsAC2 <- gwet.ac1.table(matrix, weights = ordinal.weights(1:ncol(matrix)),
                   conflev = 0.95, N = Inf)
    
    # Lin's correlation coefficient
    human <- as.numeric(human)
    machine <- as.numeric(machine)
    lcc <- epi.ccc(human, machine, 
                   ci = "z-transform", conf.level = 0.95,
                   rep.measure = FALSE, 
                   subjectid)
    print("Summary info for machine predictions:")
    print(machine_summary)
    print("Summary info for human predictions:")
    print(human_summary)
    print("Disc degeneration results:")
    print(matrix)
    # print("Confusion matrix (confusionMatrix from caret package)")
    # print(cm)
    print("Lin's correlation coefficient:")
    print(lcc$rho.c)
    # print("Matthew's correlation coefficient:")
    # print(mcc)
    print("Gwet's AC2:")
    print(gwetsAC2)
    # output <- list(cm, mcc, lcc$rho.c, gwetsAC2, matrix)
    output <- list(lcc$rho.c, gwetsAC2, matrix)
  }
  
  else {
    machine <- gather(df[, machine_cols])
    human <- gather(df[, human_cols])
    
    machine <- as.vector(machine$value)
    human <- as.vector(human$value)
    
    machine_summary <- describe(machine)
    human_summary <- describe(human)
    
    machine_f  <- factor(machine)
    human_f  <- factor(human)
    
    new_df <- data.frame(human, machine)
    names(new_df)[1] <- "Human_Grade"
    names(new_df)[2] <- "Machine_Grade"
    write.csv(new_df, save_name)
    
    # Confusion matrix/cross table:
    cm <- confusionMatrix(data=machine_f, 
                          reference=human_f)
    
    # extract the matrix:
    list <- cm[2]
    table <- list$table
    vector <- c(table[[1]], table[[3]], table[[2]], table[[4]])
    matrix <- matrix(vector, nrow=2, ncol = 2, byrow = TRUE)
    
    # Matthew's correlation coefficient:
    mcc <- mcc(confusionM = matrix)
    
    # Gwet's AC1 - for binomial/nominal data:
    gwetsAC1 <- gwet.ac1.table(matrix,
                   conflev = 0.95, N = Inf)
    
    print("Summary info for machine predictions:")
    print(machine_summary)
    print("Summary info for human predictions:")
    print(human_summary)
    print("Modic changes results:")
    print(matrix)
    # print("Confusion matrix (confusionMatrix from caret package)")
    # print(cm)
    # print("Matthew's correlation coefficient:")
    # print(mcc)
    print("Gwet's AC1:")
    print(gwetsAC1)
    # output <- list(machine_summary, human_summary, cm, mcc, gwetsAC1, matrix())
    output <- list(gwetsAC1, matrix)
  }
  return(output)
}

############### Set file and column variables ##############################################

file <- "../Data/df.csv"

############### Disc degeneration stats ##############################################

# df_dd <- prep_df(file = file, columns = columns_dd, dd = TRUE, lbp_level = 0)

# machine_cols <- c(1:5) # If choosing subset of columns make sure they correspond
# human_cols <- c(6:10)
# cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = machine_cols, human_cols = human_cols)

### all
df_dd <- prep_df(file = file, columns = c(8,7,6,5,4,67,68,69,70,71), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1:5), human_cols = c(6:10), save_name = "../data/all_dd_lbp1.csv")

### L1-L5
df_dd <- prep_df(file = file, columns = c(8,7,6,5,67,68,69,70), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1:4), human_cols = c(5:8), save_name = "../data/L1-L5_dd_lbp1.csv")

### L2-S1
df_dd <- prep_df(file = file, columns = c(7,6,5,4,68,69,70,71), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1:4), human_cols = c(5:8), save_name = "../data/L2-S1_dd_lbp1.csv")

### L1-L2
df_dd <- prep_df(file = file, columns = c(8,67), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1), human_cols = c(2), save_name = "../data/L1-L2_dd_lbp1.csv")

### L2-L3
df_dd <- prep_df(file = file, columns = c(7,68), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1), human_cols = c(2), save_name = "../data/L2-L3_dd_lbp1.csv")

### L3-L4
df_dd <- prep_df(file = file, columns = c(6,69), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1), human_cols = c(2), save_name = "../data/L3-L4_dd_lbp1.csv")

### L4-L5
df_dd <- prep_df(file = file, columns = c(5,70), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1), human_cols = c(2), save_name = "../data/L4-L5_dd_lbp1.csv")

### L5-S1
df_dd <- prep_df(file = file, columns = c(4,71), dd = TRUE, lbp_level = 1)
cm_dd <- cm(df = df_dd, dd = TRUE, machine_cols = c(1), human_cols = c(2), save_name = "../data/L5-S1_dd_lbp1.csv")


################ Modic changes stats #################################################

### all
df_mc <- prep_df(file = file,
                 columns = c(38,43,37,42,36,41,35,40,34,39,
                             72,73,74,75,76,77,78,79,80,81),
                 dd = FALSE, lbp_level = 1)
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(1:10), human_cols = c(11:20), save_name = "../data/all_mc_lbp1.csv")

### L1-L5
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(1:8), human_cols = c(11:18), save_name = "../data/L1-L5_mc_lbp1.csv")

### L2-S1
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(3:10), human_cols = c(13:20), save_name = "../data/L2-S1_mc_lbp1.csv")

### L1-L2
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(1,2), human_cols = c(11,12), save_name = "../data/L1-L2_mc_lbp1.csv")

### L2-L3
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(3,4), human_cols = c(13,14), save_name = "../data/L2-L3_mc_lbp1.csv")

### L3-L4
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(5,6), human_cols = c(15,16), save_name = "../data/L3-L4_mc_lbp1.csv")

### L4-L5
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(7,8), human_cols = c(17,18), save_name = "../data/L4-L5_mc_lbp1.csv")

### L5-S1
cm_mc <- cm(df = df_mc, dd = FALSE, machine_cols = c(9,10), human_cols = c(19,20), save_name = "../data/L5-S1_mc_lbp1.csv")
################ Modic change size filter ###########################################################

# Function that takes height and width values and returns dataframe with manual evaluation score 0 
# when MC size is less than given width or given height.

MC_filter <- function(file, height, width, height_column, width_column,
                      man_eval_column, lbp_level) {
  
  # Load file
  df <- read.csv(file)
  
  # Filter by LBP incidence
  df <- df[df$C6646Q1_98_6_2 >= lbp_level, ]
  
  # replace "-" with NA
  df[df == "-"] <- 0
  
  # get rid of the commas
  for (i in width_column) {
    df[,i] <- as.character(gsub(",","", df[,i]))
  }
  
  # count the string characters = number of zones affected:
  for (j in width_column) {
    for(i in 1:nrow(df)) {       # for-loop over rows
      df[i, j] <- nchar(df[i, j])
    }
  }
  
  for (i in width_column) {
    df[,i] <- as.integer(df[,i])
  }
  
  for(i in 1:nrow(df)) {
    for (j in 1:10) {
      if (df[i, height_column[j]] < height | df[i, width_column[j]] < width) {
        df[i, man_eval_column[j]] <- 0
      }
    }
  }

  
  return(df)
}

################ Modic changes stats large size subset #################################################

# Filter small Modic changes first:
df_h1_w2 <- MC_filter(file=file, height=1, width=2, 
                      height_column=c(85, 88, 91, 94, 97, 100, 103, 106, 109, 112), 
                      width_column=c(86, 89, 92, 95, 98, 101, 104, 107, 110, 113), 
                      man_eval_column=c(72, 73, 74, 75, 76, 77, 78, 79, 80, 81),
                      lbp_level=1)

### all
df_big_mc <- df_h1_w2[,c(38,43,37,42,36,41,35,40,34,39,
               72,73,74,75,76,77,78,79,80,81)]

cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(1:10), human_cols = c(11:20), save_name = "../data/all_mc_big_lbp1.csv")

### L1-L5
cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(1:8), human_cols = c(11:18), save_name = "../data/L1-L5_mc_big_lbp1.csv")

### L2-S1
cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(3:10), human_cols = c(13:20), save_name = "../data/L2-S1_mc_big_lbp1.csv")

### L1-L2
cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(1,2), human_cols = c(11,12), save_name = "../data/L1-L2_mc_big_lbp1.csv")

### L2-L3
cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(3,4), human_cols = c(13,14), save_name = "../data/L2-L3_mc_big_lbp1.csv")

### L3-L4
cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(5,6), human_cols = c(15,16), save_name = "../data/L3-L4_mc_big_lbp1.csv")

### L4-L5
cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(7,8), human_cols = c(17,18), save_name = "../data/L4-L5_mc_big_lbp1.csv")

### L5-S1
cm_big_mc <- cm(df = df_big_mc, dd = FALSE, machine_cols = c(9,10), human_cols = c(19,20), save_name = "../data/L5-S1_mc_big_lbp1.csv")

