# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 17/03/2021
# Author: Louis MacGregor

# This script runs the full analysis, preparing the data
# running the logistic regression analyses and the cox
# regression analses.
setwd("z:/brian/data/")
MasterAnalysisOn <- 1
# Start by lopping through the data preperation operations
for (i.master in c(1,3,5)) {
  for (j.master in c(0,1,2)) {
    dateRange <- i.master # 1, 3 or 5 days
    readingwanted <- j.master # 0-worst, 1-first, 2-mean
    source("one_row_per_id_all.R")
  }
}


STOP

# Now loop through the logistic regression models with different
# options specified
# Start by lopping through the data preparation operations
for (i in c(1,3,5)) {
  for (j in c(0,1,2)) {
    dateRange <- i # 1, 3 or 5 days
    readingwanted <- j # 0-worst, 1-first, 2-mean
    source("LogisticLABMARCSall.R")
  }
}
# NEED TO ADD A SAVE MECHANISM TO SAVE KEY OUTPUTS/GRAPHS FOR EACH RUN
