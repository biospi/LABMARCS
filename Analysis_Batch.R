# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 17/03/2021
# Authors: Louis MacGregor & Brian Sullivan

# This script runs the full analysis, preparing the data
# running the logistic regression analyses and the cox
# regression analyses.
library(tibble)
#Flag to inform LABMARCS_DataPreparation.R & LABMARCS_LogisticRegression.R
#that batch processing is being done and use the variables we set here
BatchAnalysisOn <- 1
  
#several paths to set 
#location for Rscripts and intermediate files
work_path <- 'C:/Users/bs16044/OneDrive - University of Bristol/HDR-UK-AMR/LABMARCS/source/'
setwd(work_path)

#original data location
data_path <- paste(work_path, 'data/', sep = '')

#save location for graphs, tables etc
output_path <- paste(work_path, 'output/', sep = '')

# PART 1: Data Preparation
#Data prep only needs to be run once (assuming no changes have been made) and
#can then be turned off if you wish to focus on stats output
if (1) {
# Loop through the data preparation operations
  for (day_idx in c(1,3,5,7)) { 
    for (reading_idx in c(0,1,2)) { 
      dateRange <- day_idx # number of days
      readingwanted <- reading_idx # 0-worst, 1-first, 2-mean
      print(paste('PREPARING: Day:',dateRange,'Reading:', readingwanted,'-----'))
      source(paste(work_path,"DataPreparation.R", sep = ''))
    }
  }
}



#PART 2: Data Analysis
# Set to 1 if stats analyses, graphs are wanted, else only will do data prep above

# Cross Validation Parameters
# Set number of outer fold repeats, outer folds and inner folds
repeats <- 20 #1000KCH How many times should we shuffle the data
outsidefolds <- 5 #10KCH How many splits of the shuffled data (5=20%)
# can't go lower as small test sets may only have example of one class
insidefolds <- 5 #10KCH (only relevant for LASSO)

if (1) {
  
  #create tibble for univariate results across batch
  univar_batch_result_tb <- tibble(
    day = integer(),
    outcome = integer(),
    readingtype = integer(),
    variable = character(),
    variable_dum = character(),
    oddsratio = numeric(),
    pval = numeric(),
    levels = character()
  )
  
  #Create data frame & counter for batch analysis results across cross val models
  m_ctr <- 0
  batch_df <- data.frame(ModelType = NA, Reading = NA, Day = NA, Outcome = NA,
                         Accuracy = NA, AUC = NA, Brier = NA, Sensitivity =  NA, 
                         Specificity = NA)
  glm_fulldata_batch_df <- batch_df
  glm_traindata_batch_df1 <- batch_df
  glm_traindata_batch_df2 <- batch_df
  
  #Create data structure to save cross validation results
  cv_batch_df1 <- data.frame(ModelType = NA, Reading = NA, Day = NA, Outcome = NA,
                             MedianAUC = NA,  AUC_Q2_5 = NA, AUC_Q50 = NA, AUC_Q97_5 = NA, 
                             MedianBrier =  NA, Brier_Q2_5 = NA, Brier_Q50 = NA, Brier_Q97_5 = NA, 
                             MedianLambda =  NA, Lambda_Q2_5 = NA, Lambda_Q50 = NA, Lambda_Q97_5 = NA,
                             MeanSensitivity =  NA, Sensitivity_Q2_5 = NA, Sensitivity_Q50 = NA, Sensitivity_Q97_5 = NA,
                             MeanSpecificity =  NA, Specificity_Q2_5 = NA, Specificity_Q50 = NA, Specificity_Q97_5 = NA)

  cv_batch_df2 <- cv_batch_df1 
  cv_batch_df3 <- cv_batch_df1 
  cv_batch_df4 <- cv_batch_df1 
  
  varratios_stat_df <- data.frame() 
  # Now loop through the logistic regression models with different
  # options specified
  # Start by lopping through the data preparation operations
  
  for (day_idx in c(1,3,5,7)) {
    for (reading_idx in c(0,1,2)) { # 0-worst, 1-first, 2-mean
      for (outcome_idx in c(1,3) ) {  #ALL Severe (1)  or Death (3)   
        dateRange <- day_idx # 1, 3 or 5 days
        readingwanted <- reading_idx # 0-worst, 1-first, 2-mean
        # outcomeselection (1) all severe outcomes (2) ICU admission (3) death
        outcomeselection <- outcome_idx
        print(paste('---- Day:',dateRange,'Reading:', readingwanted,'Outcome:',
                    outcomeselection, '------'))
        source("DataAnalysisPipeline.R")
      }
    }
  }

  
  #plotting text 
  str_p <- paste('_R',as.character(repeats),'_OF', as.character(outsidefolds), '_IF', 
                 as.character(insidefolds), '_', sep = '')
  
  #All severe - ICU/DEATH  
  #create compendium roc curve plot for train conditions, each row is day, col is reading type
  roc_cv_p_T_1_1_0 + roc_cv_p_T_1_1_1 + roc_cv_p_T_1_1_2 + 
    roc_cv_p_T_3_1_0 + roc_cv_p_T_3_1_1 + roc_cv_p_T_3_1_2 + 
    roc_cv_p_T_5_1_0 + roc_cv_p_T_5_1_1 + roc_cv_p_T_5_1_2 + 
    roc_cv_p_T_7_1_0 + roc_cv_p_T_7_1_1 + roc_cv_p_T_7_1_2 +  
    plot_layout(ncol = 3) + plot_annotation(title = 'All Severe: Train',
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_GLM_AllSevere_CV_Train_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  

  #create compendium roc curve plot for generalise conditions
  roc_cv_p_G_1_1_0 + roc_cv_p_G_1_1_1 + roc_cv_p_G_1_1_2 + 
    roc_cv_p_G_3_1_0 + roc_cv_p_G_3_1_1 + roc_cv_p_G_3_1_2 + 
    roc_cv_p_G_5_1_0 + roc_cv_p_G_5_1_1 + roc_cv_p_G_5_1_2 + 
    roc_cv_p_G_7_1_0 + roc_cv_p_G_7_1_1 + roc_cv_p_G_7_1_2 +  
    #roc_cv_p_G_14_1_0 + roc_cv_p_G_14_1_1 + roc_cv_p_G_14_1_2 + 
    plot_layout(ncol = 3) + plot_annotation(title = 'All Severe: Generalise',
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_GLM_AllSevere_CV_Generalise_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  

  #DEATH
  #create compendium roc curve plot for train conditions, each row is day, col is reading type
  roc_cv_p_T_1_3_0 + roc_cv_p_T_1_3_1 + roc_cv_p_T_1_3_2 + 
    roc_cv_p_T_3_3_0 + roc_cv_p_T_3_3_1 + roc_cv_p_T_3_3_2 + 
    roc_cv_p_T_5_3_0 + roc_cv_p_T_5_3_1 + roc_cv_p_T_5_3_2 + 
    roc_cv_p_T_7_3_0 + roc_cv_p_T_7_3_1 + roc_cv_p_T_7_3_2 +  
    #roc_cv_p_T_14_3_0 + roc_cv_p_T_14_3_1 + roc_cv_p_T_14_3_2 +
    plot_layout(ncol = 3) + plot_annotation(title = 'Death: Train',
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_GLM_DEATH_CV_Train_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  
  
  #create compendium roc curve plot for generalise conditions
  roc_cv_p_G_1_3_0 + roc_cv_p_G_1_3_1 + roc_cv_p_G_1_3_2 + 
    roc_cv_p_G_3_3_0 + roc_cv_p_G_3_3_1 + roc_cv_p_G_3_3_2 + 
    roc_cv_p_G_5_3_0 + roc_cv_p_G_5_3_1 + roc_cv_p_G_5_3_2 + 
    roc_cv_p_G_7_3_0 + roc_cv_p_G_7_3_1 + roc_cv_p_G_7_3_2 +  
    #roc_cv_p_G_14_3_0 + roc_cv_p_G_14_3_1 + roc_cv_p_G_14_3_2 +
    plot_layout(ncol = 3) + plot_annotation(title = 'Death: Generalise', 
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_GLM_DEATH_CV_Generalise_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  
  #ROC compendium for LASSO
  #ICU  
  #create compendium roc curve plot for train conditions, each row is day, col is reading type
  roc_cv_p_TL_1_1_0 + roc_cv_p_TL_1_1_1 + roc_cv_p_TL_1_1_2 + 
    roc_cv_p_TL_3_1_0 + roc_cv_p_TL_3_1_1 + roc_cv_p_TL_3_1_2 + 
    roc_cv_p_TL_5_1_0 + roc_cv_p_TL_5_1_1 + roc_cv_p_TL_5_1_2 + 
    roc_cv_p_TL_7_1_0 + roc_cv_p_TL_7_1_1 + roc_cv_p_TL_7_1_2 +  
    #roc_cv_p_TL_14_1_0 + roc_cv_p_TL_14_1_1 + roc_cv_p_TL_14_1_2 +
    plot_layout(ncol = 3) + plot_annotation(title = 'All Severe: Train',
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_LASSO_AllSevere_CV_Train_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  
  
  #create compendium roc curve plot for generalise conditions
  roc_cv_p_GL_1_1_0 + roc_cv_p_GL_1_1_1 + roc_cv_p_GL_1_1_2 + 
    roc_cv_p_GL_3_1_0 + roc_cv_p_GL_3_1_1 + roc_cv_p_GL_3_1_2 + 
    roc_cv_p_GL_5_1_0 + roc_cv_p_GL_5_1_1 + roc_cv_p_GL_5_1_2 + 
    roc_cv_p_GL_7_1_0 + roc_cv_p_GL_7_1_1 + roc_cv_p_GL_7_1_2 +  
    #roc_cv_p_GL_14_1_0 + roc_cv_p_GL_14_1_1 + roc_cv_p_GL_14_1_2 + 
    plot_layout(ncol = 3) + plot_annotation(title = 'All Severe: Generalise',
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_LASSO_AllSevere_CV_Generalise_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  
  
  #DEATH
  #create compendium roc curve plot for train conditions, each row is day, col is reading type
  roc_cv_p_TL_1_3_0 + roc_cv_p_TL_1_3_1 + roc_cv_p_TL_1_3_2 + 
    roc_cv_p_TL_3_3_0 + roc_cv_p_TL_3_3_1 + roc_cv_p_TL_3_3_2 + 
    roc_cv_p_TL_5_3_0 + roc_cv_p_TL_5_3_1 + roc_cv_p_TL_5_3_2 + 
    roc_cv_p_TL_7_3_0 + roc_cv_p_TL_7_3_1 + roc_cv_p_TL_7_3_2 +  
    #roc_cv_p_TL_14_3_0 + roc_cv_p_TL_14_3_1 + roc_cv_p_TL_14_3_2 +
    plot_layout(ncol = 3) + plot_annotation(title = 'Death: Train',
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_LASSO_DEATH_CV_Train_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  
  
  #create compendium roc curve plot for generalise conditions
  roc_cv_p_GL_1_3_0 + roc_cv_p_GL_1_3_1 + roc_cv_p_GL_1_3_2 + 
    roc_cv_p_GL_3_3_0 + roc_cv_p_GL_3_3_1 + roc_cv_p_GL_3_3_2 + 
    roc_cv_p_GL_5_3_0 + roc_cv_p_GL_5_3_1 + roc_cv_p_GL_5_3_2 + 
    roc_cv_p_GL_7_3_0 + roc_cv_p_GL_7_3_1 + roc_cv_p_GL_7_3_2 +  
    #roc_cv_p_GL_14_3_0 + roc_cv_p_GL_14_3_1 + roc_cv_p_GL_14_3_2 +
    plot_layout(ncol = 3) + plot_annotation(title = 'Death: Generalise', 
                                            theme = theme(plot.title = element_text(size = 26)))
  
  ggsave(paste(output_path, 'ROC_Compendium_LASSO_DEATH_CV_Generalise_', str_p, '_', 'ROCs_for_N',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 60, height = 100, units = 'cm', dpi = 300)
  
}

