# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 31/01/2021
# Author: Louis MacGregor & Brian Sullivan

# Location of Rscripts & save intermediate processed files
if (!exists('work_path')) {
  work_path <- 'C:/Users/bs16044/OneDrive - University of Bristol/HDR-UK-AMR/LABMARCS/brian/'
}

#Location of graphs and tables outputted
if (!exists('output_path')) {
  output_path <- paste(work_path, 'output/', sep = '')
}

# Input variables (if transformed into a function)
# Set LookForUpdates to 1 if you want to search for and install newest 
#R version
LookForUpdates <- 0

# Set InstallPackAges to 1 if you want to install the packages listed 
#below all at once
InstallPackages <- 0

# Load libraries
LoadLibraries <- 1
  
# Choose to include Firth's bias in the models (constrains parameters 
#from >> numbers) #currently breaks - debug
IncludeFirthBias <- 0
  
# Use MICE imputation? 1 == yes, 0==none, complete case analysis 
ImputationAllowed <- 0

if (ImputationAllowed == 0) {
  imputation_str = 'None'
} else if (ImputationAllowed == 1) {
  imputation_str = 'MICE'
}

# Model exclusively looking at comordbities (looks only at 1 hospital)
# Loses 50% of data but appears to be a better model
Comorbidities <- 0
  
# Cross Validation Parameters
# Set number of outer fold repeats, outer folds and inner folds
repeats <- 250 #1000KCH How many times should we shuffle the data
outsidefolds <- 4 #10KCH How many splits of the shuffled data (5=20%)
# can't go lower as small test sets may only have example of one class
insidefolds <- 5 #10KCH (only relevant for LASSO)
n_models <- repeats*outsidefolds


# BatchAnalysisOn==1 prevents the dateRange and readingwanted variables from
# being overwritten, but the option to turn this off and run this script 
# without first running MasterAnalysis.R is available by setting 
# BatchAnalysisOn==0 and then manually selecting the dateRange and 
# readingwanted variables.

if (!exists("BatchAnalysisOn")) {
    dateRange <- 1 # 1, 3 or 5 days
    readingwanted <- 0 # 0-worst, 1-first, 2-mean
    # outcomeselection (1) all severe outcomes (2) ICU admission (3) death
    outcomeselection <- 2
    BatchAnalysisOn <- 0
  }

if (outcomeselection == 1) {
  outcome_str = 'AllSevere'
} else if (outcomeselection == 2) {
  outcome_str = 'ICU'
} else if (outcomeselection == 3) {
  outcome_str = 'Death'
}

if (readingwanted == 0) {
  readingwanted_str = 'Worst'
  } else if (readingwanted == 1) {
    readingwanted_str = 'First'
  } else if (readingwanted == 2) {
    readingwanted_str = 'Mean'
  }

if (LookForUpdates == 1) {
  # Update R
  install.packages("installr")
  library(installr)
  updateR()
}

if (InstallPackages == 1) {
  # Packages which may require installation
  install.packages("mice")
  install.packages("DescTools")
  install.packages("moments")
  install.packages("RANN")
  install.packages("VIM")
  install.packages("tidymodels")
  install.packages("tibble")
  install.packages("mbest")
  install.packages("nestfs")
}

if (LoadLibraries == 1) {
  # Load libraries
  library("mice")
  library("nestfs")
  library("dplyr")
  library("MASS")
  library("VIM")
  library("RANN")
  library("moments")
  library("DescTools")
  library("caret")
  library("corrplot")
  library("glmnet")
  library("pROC")
  library("tidymodels")
  library("mbest")
  library("ggplot2")
  library("pROC")
  library("md")
  library('fastDummies')
  library('data.table')
}
  
# DATA PROCESSING ------------------------------------------------------
# Read in data depending on day window & best/worst/mean measurement compression

setwd(work_path)

save_path = paste(output_path, 'Day-',dateRange,'_Reading-',readingwanted_str,
                  '_Outcome-',outcome_str, '_Imputation-',imputation_str, '_',sep = '')

readingwanted_ls = c('worst','first','mean')

#read in data processed from one_row_per_id_all.R
fulldata <- read.csv(file = paste('totalBinary',as.character(dateRange),
  as.character(readingwanted_ls[readingwanted + 1]), '.csv',sep = ''),na.strings = c(""))

#remove X column (not used)
fulldata <- subset(fulldata, select = -c(X))

#stor the raw copy just in case
fulldata_origin <- fulldata

# Initial inspection of the data
#head(fulldata)
#summary(fulldata)
#write.table(colnames(fulldata),file = 'fulldata_column_names.txt',row.names = FALSE)

# Select outcome variable
if (outcomeselection == 1) {
  fulldata$outcome <- fulldata$severeOutcome
} else if (outcomeselection == 2) {
    fulldata$outcome <- fulldata$went_to_icu  
} else {
    fulldata$outcome <- fulldata$died
}

# Remove rows without positive test result dates
fulldata <- fulldata[complete.cases(fulldata[13]),]

if (Comorbidities == 1) {
  # Only look at sites with comorbidities
  fulldata <- fulldata[(fulldata$Site == "NBT"),]
}

#exclude cases that started in hospital #removes about 27%
#fulldata <- fulldata[fulldata$OnAdmission == TRUE,]

#print some general patient stats 
sink(paste(save_path,'ALL_Patient_Numbers.txt',sep = ''))
site_ls = c('NBT','Weston','UHB1','UHB2')
for (site in site_ls) {
  print(site)
  print('All Died ICU Both')
  print(paste(as.character(sum(fulldata$Site == site)),
              as.character(sum(fulldata$Site == site & fulldata$died)),
              as.character(sum(fulldata$Site == site & fulldata$went_to_icu)),
              as.character(sum(fulldata$Site == site & fulldata$died & fulldata$went_to_icu))))
}
sink()

#save for filtering test/train data
siteData <- fulldata$Site

# Remove unneeded data columns 
if (1) { #omit if column info needed after data pruning for counting patient numbers
  fulldata <- subset(fulldata, select = -c(died,went_to_icu,severeOutcome,coinfection, #N (absent?)
                                           admissionDate,dischargeDate,ITU_Start,
                                           ITU_End,deathDate,ID,Site,positiveDate)) 
}                                  

#WCC is correlated with neutrophil & lymphocyte, removed in King's paper but
#our data don't show this? C02 and BE (bicarbonate excess are correlated) - C02 not taken often BE

if (Comorbidities == 0) {
  # Remove comorbidities as variables if we aren't considering them
  fulldata <- subset(fulldata, select = -c(PSI,NYHA_Heart_failure,CRB65_Score,
                                        NEWS2_Score,COPD,Asthma,Bronchiectasis,
                                        Respiratory_Disease_other, Hypertension,
                                        Chronic_Kidney_Disease,Liver_disease,
                                        Diabetes,CVA_Stroke,TIA_mini_stroke,
                                        Immunodeficiency, HIV_positive))
}

# number of events
eventsnumber <- sum(fulldata$outcome,na.rm = "TRUE")

# WHEN WE HAVE OTHER CONTINUOUS VARIABLES ADD THEM HERE
# Gender (Gender)
fulldata$Gender = factor(fulldata$Gender, levels = c("F", "M"))

# Age (Age)
# Age grouped, make into average # NEED TO GET WORKING FOR NEW AGE DATA
fulldata$Age[fulldata$Age == "20-21"] <- "20.5"
fulldata$Age[fulldata$Age == "99-102"] <- "100.5"
fulldata$Age <- as.numeric(fulldata$Age)

# Drop if age is missing First site then fulldata
siteData <- siteData[!is.na(fulldata$Age)]
fulldata <- fulldata[!is.na(fulldata$Age), ]


if (1) {
  sink(paste(save_path,'fulldata_variable_histogram_counts.txt',sep = ''))
  for (i in 3:dim(fulldata)[2]) {
      n <- names(fulldata)[i]
      print(paste(i, n))
      print(table(fulldata[,i]))
      print('------------------------------')
    }
  sink()
}


# All others ()
# EXCLUDE CONTINUOUS VARIABLES
if (1) { #omit if counting participants nums (factors won't allow logical indexing)
  for (i in 3:(length(fulldata) - 1)) {
    fulldata[,i] = as.factor(fulldata[,i])
  }
}

# BETTER PRACTICE IS TO SET CATEGORY LEVELS SO THAT ORs ARE >1
# I.E. REFERENCE CATAGORY IS THE LOWEST RISK.
#for (i in 3:(lehead(fulldata)par(mfrow=c(1,1))
pdf(paste(save_path, 'age_histogram.pdf',sep = ''))
nbreaks <- pretty(range(fulldata$Age),n = 20)
xlab = paste('Age, bin width=', as.character(nbreaks[2] - nbreaks[1]))
hist(fulldata$Age,main = "Distribution of Patient Ages",xlab = xlab, breaks = nbreaks)
dev.off()

# Display summary of the data in this format
summary(fulldata)

# DROP IF OUTCOME IS MISSING? DOES THIS OCCUR?

# Drop variables where 30% or more of the values are NA
fulldata <- fulldata[, colMeans(is.na(fulldata)) <= .30]
# Examine dropped variables
summary(fulldata[, colMeans(is.na(fulldata)) > .30])
# Check the updated data
summary(fulldata)

# ----------------------------------------------------------------------

# INPUT EXAMINATION ----------------------------------------------------

# Calculate the skewness of the continuous variables
skewness(fulldata$Age, na.rm = TRUE) # moderate negative skew
# Transform variables which have skew
fulldata$Age <- log10(max(fulldata$Age + 1) - fulldata$Age)
# Check new skewness values
skewness(fulldata$Age, na.rm = TRUE) # nearly approximately symmetric
  
# Plot histograms of continuous variables to examine distributions
nbreaks <- pretty(range(fulldata$Age),n = 20)
par(mfrow = c(1,1))
xlab <- paste('Age, bin width=', as.character(nbreaks[2] - nbreaks[1]))
pdf(paste(save_path, 'age_histogram_transformed.pdf',sep = ''))
hist(fulldata$Age,main = "Distribution of Patient Ages",xlab = xlab, breaks = nbreaks)
dev.off()
  
# Scale continuous variables to have mean 0 and sd of 1
fulldata$Age <- scale(fulldata$Age)[, 1]
  
# Check the scaling has worked as expected
#mean(fulldata$Age)
#sd(fulldata$Age)
  
# Plot regularized variables
# Plot histograms of continuous variables to examine distributions
nbreaks <- pretty(range(fulldata$Age),n = 20)
par(mfrow = c(1,1))
xlab <- paste('Age, bin width=', as.character(nbreaks[2] - nbreaks[1]))
pdf(paste(save_path, 'age_histogram_regularized.pdf',sep = '' ))
hist(fulldata$Age,main = "Distribution of Patient Ages",xlab = xlab, breaks = nbreaks)
dev.off()
  
# Plot boxplots to look for outliers
par(mfrow = c(1,1))
pdf(paste(save_path, 'age_boxplot_outliers.pdf',sep = ''))
boxplot(fulldata$Age,main = "Age", ylab = "Years")
dev.off()
  
# Examine continuous variables before winsorisation
summary(fulldata$Age)

# Winsorise the data at the 1st and 99% percentile (replace extreme values)
fulldata$Age <- Winsorize(fulldata$Age, minval = NULL, maxval = NULL,
                             probs = c(0.01, 0.99), na.rm = FALSE, type = 7)
  
# Examine continuous variables after winsorisation
summary(fulldata$Age)

 
# ASSIGN TRAINING AND TEST DATA SETS ----------------------------------

#The function below will remove variable columns that have 'NA/Test Not Taken'
#that exceeds some threshold (30% default). 
#source('LABMARCS_LogisticRegression_RemoveLowDataColumns.R')

#This removes a lot of data, for now we consider the results of
#a univariate GLM and include even if below this value, see 
#LABMARCS_LogisticRegression_CreateDummyVarList.R

site_ls <- c('NBT','Weston', 'UHB')
if (1) {
  sink(paste(save_path,'filtered_variable_histogram_counts.txt', sep = ''))
  for (i in 1:dim(fulldata)[2]) {
    for (j in site_ls) {
    data_focus <- fulldata[grepl(j, siteData, fixed = TRUE),] # train.data
    n <- names(data_focus)[i]
    print(paste(j, n))
    print(table(data_focus[,i]))
    print('------------------------------')
    }
  }
  sink()
}

#----------------------------------------------------------------------
#create dummy variables (glm can do for you but better to manually specify)
fulldata_origin_filter <- fulldata


#Dummy columns should be created manually instead of online via GLM as sometimes
#low data presence means some columns aren't created in cross-validation
#Further, some variables are not worth keeping due to low data coverage
#We have several ways to exclude see create_dummy_var_list.r

#Currently using: Only vars that are significant in Univariate GLM
fulldata_nodummy <- fulldata
source(paste(work_path, "LABMARCS_LogisticRegression_CreateDummyVarList.R", sep = ''))
tmp <- dummy_cols(fulldata)
fulldata <- tmp[,dum_var_ls]
 
#Use only select hospitals for train/test
logCond = siteData == 'NBT' | siteData == "UHB1" | siteData == "UHB2"
train.data <- fulldata[logCond, ]
test.data <- fulldata[!logCond, ] #this is the generalisation test 


# DATA IMPUTATION
if (ImputationAllowed == 0) {

  #If no imputation just keep complete cases
  train.data <- train.data[complete.cases(train.data), ]
  test.data <- test.data[complete.cases(test.data), ]
  
  } else { #Imputation done in separate file
      source(paste(work_path, "LABMARCS_LogisticRegression_ImputeData.R", sep = ''))
}

#If counting patient numbers keep site until here
#fulldata <- subset(fulldata, select = -c(Site))
#sink(paste(save_path,'FILTER_Patient_Numbers.txt', sep = ''))
#print(paste('Total Patients:',as.character(dim(fulldata)[1]) ))
#print(paste('Outcome TRUE:', as.character(sum(fulldata$outcome))))
#sink()

print("Finished pre-processing")

#prepare data structures for saving across batch loop
if (!BatchAnalysisOn) {
  m_ctr <- 1
  batch_df <- data.frame(ModelType = NA, Reading = NA, Day = NA, Outcome = NA,
                         Accuracy = NA, AUC = NA, Brier = NA, Sensitivity =  NA, 
                         Specificity = NA)
  glm_fulldata_batch_df <- batch_df
  glm_traindata_batch_df1 <- batch_df
  glm_traindata_batch_df2 <- batch_df
  
} else {
  m_ctr <- m_ctr + 1
}

#----------------------------------------------------------------------
# RUN SIMPLE PRELIMINARY MODELS ON ORIGINAL DATA COLUMNS WITH NO IMPUTATION
if (1) {
  #Run univariate tests to get an idea of each variables prediction power
  source(paste(work_path,'LABMARCS_LogisticRegression_Run_Univariate_Tests.R', sep = ''))

  #Run a GLM on all data - our benchmark for best performance
  SelectedData <- fulldata
  SelectedDataOutcome <- fulldata
  SelectedData_str <- paste('TrainFullData_TestFullData_N', as.character(dim(SelectedData)[1]) , sep = '')
  source(paste(work_path,'LABMARCS_LogisticRegression_GLM_On_Selected_Data.R', sep = ''))
  
  #save things to our summary data table
  glm_fulldata_batch_df[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
  write.table(glm_fulldata_batch_df, file = paste(output_path,'Batch_GLM_TrainFullData_TestFullData_Summary_Table.csv',sep = ''),
              row.names = FALSE, sep = ',')
  
  
  #Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
  SelectedData <- train.data
  SelectedDataOutcome <- train.data
  SelectedData_str <- paste('Train_TrainData_Test_TrainData_N', as.character(dim(SelectedData)[1]) , sep = '')
  source(paste(work_path,'LABMARCS_LogisticRegression_GLM_On_Selected_Data.R', sep = ''))
  
  #save things to our summary data table
  glm_traindata_batch_df1[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                      out_acc,out_auc, out_brier, out_sen, out_spec)
  write.table(glm_traindata_batch_df1, file = paste(output_path,'Batch_GLM_Train_TrainData_Test_TrainData_Summary_Table.csv',sep = ''),
              row.names = FALSE, sep = ',')

  #Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
  SelectedData <- train.data
  SelectedDataOutcome <- test.data
  SelectedData_str <- paste('Train_TrainData_Test_TestData_N', as.character(dim(SelectedData)[1]) , sep = '')
  source(paste(work_path,'LABMARCS_LogisticRegression_GLM_On_Selected_Data.R', sep = ''))
  
  #save things to our summary data table
  glm_traindata_batch_df2[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                      out_acc,out_auc, out_brier, out_sen, out_spec)
  write.table(glm_traindata_batch_df2, file = paste(output_path,'Batch_GLM_Train_TrainData_Test_TestData_Summary_Table.csv',sep = ''),
              row.names = FALSE, sep = ',')
  
}


#-RUN THE CROSS VALIDATION MODELS -------------------------------------------------
if (1) {
  
  if (!BatchAnalysisOn) {
    cv_batch_df1 <- data.frame(ModelType = NA, Reading = NA, Day = NA, Outcome = NA,
                    MeanAUC = NA,  AUC_Q2_5 = NA, AUC_Q50 = NA, AUC_Q97_5 = NA, 
                    MeanBrier =  NA, Brier_Q2_5 = NA, Brier_Q50 = NA, Brier_Q97_5 = NA, 
                    MeanLambda =  NA, Lambda_Q2_5 = NA, Lambda_Q50 = NA, Lambda_Q97_5 = NA)
    cv_batch_df2 <- cv_batch_df1 
  }

  #Do cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
  crossval.train.data <- train.data
  generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified  
  cv_desc = 'Train_CVTrainData_Test_CVTestData_'
  source('LABMARCS_LogisticRegression_CrossValidate_GLM_On_Selected_Data.R')
  
  #save things to our summary data table
  cv_batch_df1[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                           mean(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                           mean(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                           mean(lambda.store), lambda.store_quantile[1],
                           lambda.store_quantile[2],lambda.store_quantile[3])
  
  write.table(cv_batch_df1, file = paste(output_path, model_desc_str, 'Summary_Table.csv',sep = ''),
              row.names = FALSE, sep = ',')
  
  #Do cross validation training on (1 - 1/outsidefolds) but test on excluded dataset
  #for generalisation
  crossval.train.data <- train.data
  crossval.test.data <- test.data
  generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified  
  cv_desc = 'Train_CVTrainData_Test_GeneraliseTestData_'
  source('LABMARCS_LogisticRegression_CrossValidate_GLM_On_Selected_Data.R')

  #save things to our summary data table
  cv_batch_df2[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                            mean(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                            mean(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                            mean(lambda.store), lambda.store_quantile[1],
                            lambda.store_quantile[2],lambda.store_quantile[3])
  
  write.table(cv_batch_df2, file = paste(output_path, model_desc_str, 'Summary_Table.csv',sep = ''),
              row.names = FALSE, sep = ',')
  
} #Matches IF(1)



if (0) {  
  # LOOK AT MINIMUM REPORTING FOR STABILITY ANALYSES FROM STATS RECOMMENDATIONS
  #---------------------------------------------------------------------------
  print("Run Lasso with ALL data")
  # Create the final model over the whole dataset, with the expected performance 
  #from nested CV
  
  #stick with self created dummy vars (model.matrix builds for you)
  x1 <- model.matrix(outcome~., fulldata_origin_filter)[,-1]
  x2 <- model.matrix(outcome~., fulldata)[,2:dim(fulldata)[2]]
  
  #get estimates for setting lambda at 1se
  cv.lasso1 <- cv.glmnet(x1,fulldata_origin_filter$outcome, alpha = 1, data = fulldata,  
                         nfolds = insidefolds, family = "binomial")
  
  cv.lasso2 <- cv.glmnet(x2,fulldata$outcome, alpha = 1, data = fulldata,  
                         nfolds = insidefolds, family = "binomial")
  
  StatModel_FullLasso1 <- glmnet(x1, fulldata_origin_filter$outcome, alpha = 1, family = "binomial",
                                lambda = cv.lasso1$lambda.1se)
  
  StatModel_FullLasso2 <- glmnet(x2, fulldata$outcome, alpha = 1, family = "binomial",
                                lambda = cv.lasso2$lambda.1se)
  
  pdf(paste(save_path, 'Model_LassoAllData_Lambda.pdf',sep = ''))
  plot(cv.lasso)
  dev.off()
  
  sink(paste(save_path, 'Model_LassoAllData_Summary.txt',sep = ''))

  # (ii) Global model standard errors, variables and coefficients
  #The global model including all candidate variables with regression 
  #coefficients and standard errors. (See also REMARK guidelines, 
  #item 17, Altman et al., 2012).
    
  # Display regression coefficients
  print('Coefficients')
  print(coef(StatModel_FullLasso))
  
  # Examine odds ratios
  print('Odds Ratios')
  print(exp(coef(StatModel_FullLasso)))
  
  # Test the best predicted lambda on the remaining data in the outer fold
  x.test <- model.matrix(outcome ~., test.data)[,-1]
  probabilities <- StatModel_FullLasso %>% predict(newx = x.test, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  
  conf_matrix <- table(predicted.classes,test.data$outcome)
  
  tab <- matrix(c(0,0,0,0), ncol = 2, byrow = TRUE)
  colnames(tab) <- c('0','1')
  rownames(tab) <- c('0','1')
  tab <- as.table(tab)
  
  #sometimes the conf matrix is incomplete copy what we do have to tab
  tryCatch(tab[1,1] <- conf_matrix[1,'FALSE'], error = function(e) {tab[1,1] <- 0 } )
  tryCatch(tab[2,1] <- conf_matrix[2,'FALSE'], error = function(e) {tab[2,1] <- 0 } )
  tryCatch(tab[1,2] <- conf_matrix[1,'TRUE'], error = function(e) {tab[1,2] <- 0 } )
  tryCatch(tab[2,2] <- conf_matrix[2,'TRUE'], error = function(e) {tab[2,2] <- 0 } )
  
  print('Sensitivity')
  out_sen <- as.numeric(sensitivity(tab)['.estimate'])
  print(out_sen)
  
  print('Specificity')
  out_spec <- as.numeric(specificity(tab)['.estimate'])
  out_spec <- print(out_spec)
  
  # Model accuracy
  print('Accuracy')
  out_acc <- mean(predicted.classes == test.data$outcome)
  print(out_acc)
  
  # Plot ROC curve and find AUC
  roccurve3 <- roc(outcome ~ c(probabilities), data = test.data)
  out_auc <- auc(roccurve3)
  print('AUC')
  print(out_auc)
  
  ggroc(roccurve3, legacy.axes = T) +
    geom_abline(slope = 1 ,intercept = 0) + # add identity line
    theme(
      panel.background = element_blank(), 
      axis.title.x = element_text(size = 18, face = 'bold'),
      axis.title.y = element_text(size = 18, face = 'bold'),
      panel.border = element_rect(size = 2, fill = NA), 
      axis.text.x = element_text(size = 14, face = 'bold'),
      axis.text.y = element_text(size = 14, face = 'bold')) +
    xlab('1 - Specificity') +
    ylab('Sensitivity') +
    scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) + 
    scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25))
  ggsave(paste(save_path, 'Model_LassoAllData_ROC.pdf', sep = ''),device = 'pdf',
         width = 20, height = 20, units = 'cm', dpi = 300)
  
  # Brier score
  f_t <- probabilities
  o_t <- test.data$outcome
  out_brier <- mean(((f_t) - o_t)^2)
  #out_brier <- BrierScore(StatModel_FullLasso) ?pred missing - GLMnet?
  print('Brier score')
  print(out_brier)
  sink()
  sink()
  
  print("Saving results tables...")
  m_ctr <- m_ctr + 1
  #save things to our summary data table
  batch_df[m_ctr,] <- c('FullLasso', readingwanted_str, dateRange, outcome_str,
                        out_acc,out_auc, out_brier, out_sen, out_spec)
  write.table(batch_df, file = paste(output_path,'Batch_Model_Summary_Table.csv',sep = ''),
              row.names = FALSE)

  # ---------------------------------------------------------------------
  print('-- Done! --')

}
