# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 17/03/2021
# Author: Louis MacGregor

# This script runs the full analysis, preparing the data
# running the logistic regression analyses and the cox
# regression analses

# Set main directory where the files etc will be saved and the code is located
mainDir <<- ("C:/Users/lm13381/OneDrive - University of Bristol/Documents/AMR Project/LABMARCS/Code/LABMARCS-main/LABMARCS-main")

# Start by lopping through the data preperation options i.e. window of testing after COVID+
# test date and the compression type or multiple tets results.
# NOTE: In AvonCap.csv and 'one_row_per_id_all.Rmd' ALL Ã must be changed to A as knitr::purl
# has issues recognising UTF-8 characters on windows.
# Also, ensure the working directory i.e. setwd() within 'one_row_per_id_all.Rmd'
# matchs mainDir
for (dateRange in c(1,3,5)) {
  for (readingwanted in c(0,1,2)) {
    dateRange <<- dateRange # 1, 3 or 5 days
    readingwanted <<- readingwanted # 0-worst, 1-first, 2-mean
    source(knitr::purl("C:/Users/lm13381/OneDrive - University of Bristol/Documents/AMR Project/LABMARCS/Code/LABMARCS-main/LABMARCS-main/one_row_per_id_all.Rmd"))
  }
}

# Now loop through the regression models with these different data preparation
# options specified (Could add imputation type, comorbidities on/off, outcome type etc)
# the key outputs from these runs are saved for comparison
for (dateRange in c(1,3,5)) {
  for (readingwanted in c(0,1,2)) {
    dateRange <<- dateRange # 1, 3 or 5 days
    readingwanted <<- readingwanted # 0-worst, 1-first, 2-mean
    setwd(mainDir)
    source("LogisticLABMARCSall.R")
    source("CoxLABMARCSall.R")
  }
}

# Loop through saved model outputs and identify the best combo of data preparation options
# for the Logistics and Cox regression LASSO models. In these examples we identify the model's
# mean expected AUC or c-statistic and view them all side by side

# Mean AUCs (Logistic regression)
count <- 0
MeanAUCRNCV <- vector("numeric", length=9) # length equal to number of models to compare
cindex <- vector("numeric", length=9) # length equal to number of models to compare
LabelResult <- vector("character", length=9) # length equal to number of models to compare
for (dateRange in c(1,3,5)) {
  for (readingwanted in c(0,1,2)) {
    count <- count+1
    dateRange <<- dateRange # 1, 3 or 5 days
    readingwanted <<- readingwanted # 0-worst, 1-first, 2-mean
    # Name of folder for saving results from this model
    subDir <- "LogisticOutputs"
    # Name of sub sub folders containing results for micro-choices in the models
    # e.g. compression type of the data, or the number of days in the window for results
    subsubDir <- paste(dateRange,"days",readingwanted)
    setwd(file.path(mainDir, subDir, subsubDir))
    # Load AUC t-test results
    AUCRNCV <- readRDS("AUCRNCV.rds")
    # Extract the estimate of the mean from the t-test results
    MeanAUCRNCV[count] <-AUCRNCV[["estimate"]]
    # Label the model in question
    LabelResult[count] <- paste(dateRange,"days",readingwanted) # INDEX THIS
  }
}

# Show the estimated mean AUCs and model labels
LabelResult
MeanAUCRNCV

# Mean c-statistics (Cox regression)
count <- 0
for (dateRange in c(1,3,5)) {
  for (readingwanted in c(0,1,2)) {
    count <- count+1
    dateRange <<- dateRange # 1, 3 or 5 days
    readingwanted <<- readingwanted # 0-worst, 1-first, 2-mean
    # Name of folder for saving results from this model
    subDir <- "CoxOutputs"
    # Name of sub sub folders containing results for micro-choices in the models
    # e.g. compression type of the data, or the number of days in the window for results
    subsubDir <- paste(dateRange,"days",readingwanted)
    setwd(file.path(mainDir, subDir, subsubDir))
    # Load the mean c stat
    CstatRNCV[count] <- readRDS("CstatRNCV.rds")
    # Label the model in question
    LabelResult[count] <- paste(dateRange,"days",readingwanted) # INDEX THIS
  }
}

# Show the estimated mean AUCs and model labels
LabelResult
CstatRNCV