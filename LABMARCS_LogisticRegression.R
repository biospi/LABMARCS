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
  
# BatchAnalysisOn==1 prevents the dateRange and readingwanted variables from
# being overwritten, but the option to turn this off and run this script 
# without first running MasterAnalysis.R is available by setting 
# BatchAnalysisOn==0 and then manually selecting the dateRange and 
# readingwanted variables.

if (!exists("BatchAnalysisOn")) {
    dateRange <- 3 # 1, 3, 5, 7,14days
    readingwanted <- 0 # 0-worst, 1-first, 2-mean
    # outcomeselection (1) all severe outcomes (2) ICU admission (3) death
    outcomeselection <- 1
    BatchAnalysisOn <- 0
    
    # Cross Validation Parameters
    # Set number of outer fold repeats, outer folds and inner folds
    repeats <- 20 #1000KCH How many times should we shuffle the data
    outsidefolds <- 5 #10KCH How many splits of the shuffled data (5=20%)
    # can't go lower as small test sets may only have example of one class
    insidefolds <- 5 #10KCH (only relevant for LASSO)
}

n_models <- repeats*outsidefolds

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

# Packages which may require installation
if (InstallPackages == 1) {
  install.packages("mice")
  install.packages("DescTools")
  install.packages("moments")
  install.packages("RANN")
  install.packages("VIM")
  install.packages("tidymodels")
  install.packages("tibble")
  install.packages("mbest")
  install.packages("nestfs")
  install.packages("patchwork")
  #need development projpred version https://github.com/stan-dev/projpred
  devtools::install_github('stan-dev/projpred', ref = 'develop', build_vignettes = TRUE)
}

#LOAD REQUIRED LIBRARIES
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
library('patchwork')
#Bayesian packages required  
library(ProbBayes)
library(brms)
library(stringr)
library(projpred) 
library(dplyr)
library(ggplot2)
library(bayesplot)
options(mc.cores = parallel::detectCores())


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

#lets make age categorical too <50, 50-59, 60-69, 70-80, 80+
fulldata$Age_Below50 <- as.numeric(fulldata$Age < 50)
fulldata$Age_50_59 <- as.numeric((fulldata$Age >= 50) & (fulldata$Age < 60))
fulldata$Age_60_69 <- as.numeric((fulldata$Age >= 60) & (fulldata$Age < 70))
fulldata$Age_70_79 <- as.numeric((fulldata$Age >= 70) & (fulldata$Age < 80))
fulldata$Age_80above <- as.numeric((fulldata$Age >= 80))



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

#perform any final tranforms on data n=826


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
train.data_NoDummy <- fulldata_nodummy[logCond, ]
test.data_NoDummy <- fulldata_nodummy[!logCond, ] #this is the generalisation test 



# DATA IMPUTATION
if (ImputationAllowed == 0) {

  #If no imputation just keep complete cases
  train.data <- train.data[complete.cases(train.data), ]
  test.data <- test.data[complete.cases(test.data), ]
  
  } else { #Imputation done in separate file
      source(paste(work_path, "LABMARCS_LogisticRegression_ImputeData.R", sep = ''))
}

#If counting patient numbers keep site data until here
#fulldata <- subset(fulldata, select = -c(Site))
#sink(paste(save_path,'FILTER_Patient_Numbers.txt', sep = ''))
#print(paste('Total Patients:',as.character(dim(fulldata)[1]) ))
#print(paste('Outcome TRUE:', as.character(sum(fulldata$outcome))))
#sink()

print("Finished pre-processing")

#save list of all participants train and test for visualizations



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
# RUN SET OF GLM MODELS WITH NO CROSS VALIDATION

#Run univariate tests to get an idea of each variables prediction power
print('Run Univariate tests...')
source(paste(work_path,'LABMARCS_LogisticRegression_Run_Univariate_Tests.R', sep = ''))

#Run a GLM on all data - our benchmark for best performance
SelectedData <- fulldata
SelectedDataOutcome <- fulldata
SelectedData_str <- paste('TrainFullData_TestFullData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM with full data test/train...')
source(paste(work_path,'LABMARCS_LogisticRegression_GLM_On_Selected_Data.R', sep = ''))
  
#save things to our summary data table
glm_fulldata_batch_df[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                   out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_fulldata_batch_df, file = paste(output_path,'Batch_GLM_TrainFullData_TestFullData_Summary_Table.csv',sep = ''),
            row.names = FALSE, sep = ',')
  
#Internal Validation  
#Run a GLM only on train.data (UHB,NBT) and test on train.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- train.data
SelectedData_str <- paste('Train_TrainData_Test_TrainData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM only on train data for test/train...')
source(paste(work_path,'LABMARCS_LogisticRegression_GLM_On_Selected_Data.R', sep = ''))
  
#save things to our summary data table
glm_traindata_batch_df1[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                    out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df1, file = paste(output_path,'Batch_GLM_Train_TrainData_Test_TrainData_Summary_Table.csv',sep = ''),
            row.names = FALSE, sep = ',')

#External Validation
#Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- test.data
SelectedData_str <- paste('Train_TrainData_Test_GeneraliseData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM, train on UHB/NBT, test on Weston...')
source(paste(work_path,'LABMARCS_LogisticRegression_GLM_On_Selected_Data.R', sep = ''))
  
#save things to our summary data table
glm_traindata_batch_df2[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                    out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df2, file = paste(output_path,'Batch_GLM_Train_TrainData_Test_TestData_Summary_Table.csv',sep = ''),
            row.names = FALSE, sep = ',')
  


#-RUN THE CROSS VALIDATION MODELS -------------------------------------------------
if (!BatchAnalysisOn) {
  cv_batch_df1 <- data.frame(ModelType = NA, Reading = NA, Day = NA, Outcome = NA,
                  MeanAUC = NA,  AUC_Q2_5 = NA, AUC_Q50 = NA, AUC_Q97_5 = NA, 
                  MeanBrier =  NA, Brier_Q2_5 = NA, Brier_Q50 = NA, Brier_Q97_5 = NA, 
                  MeanLambda =  NA, Lambda_Q2_5 = NA, Lambda_Q50 = NA, Lambda_Q97_5 = NA,
                  MeanSensitivity =  NA, Sensitivity_Q2_5 = NA, 
                  Sensitivity_Q50 = NA, Sensitivity_Q97_5 = NA,
                  MeanSpecificity =  NA, Specificity_Q2_5 = NA, 
                  Specificity_Q50 = NA, Specificity_Q97_5 = NA)
    
  cv_batch_df2 <- cv_batch_df1 
  cv_batch_df3 <- cv_batch_df1 
  cv_batch_df4 <- cv_batch_df1 
  cv_batch_df5 <- cv_batch_df1 
  
  varratios_stat_df <- data.frame() 
}
  

#--------------------------------------------------------------------------------------
#Do GLM cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
crossval.train.data <- train.data
#don't need to specify crossval.test.data as it will be a % from crossval.train.data
generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified  
cv_desc = 'Train_CVTrainData_Test_CVTestData_'
p_str <- 'T' #(T)rain, text prefix for roc curve compendium variables
print('Run GLM with CrossVal Train only UHB/NBT 80/20 split...')
source('LABMARCS_LogisticRegression_CrossValidate_GLM_On_Selected_Data.R')

#save things to our summary data table
cv_batch_df1[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                         median(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                         median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                         median(lambda.store), lambda.store_quantile[1],
                         lambda.store_quantile[2],lambda.store_quantile[3],
                         median(sensitivity_store),sensitivity_quantile[1],
                         sensitivity_quantile[2],sensitivity_quantile[3], 
                         median(specificity_store),specificity_quantile[1],
                         specificity_quantile[2],specificity_quantile[3]) 

write.table(cv_batch_df1, file = paste(output_path, 'GLM_Train_Summary_Compendium.csv',sep = ''),
            row.names = FALSE, sep = ',')

#--------------------------------------------------------------------------------------
#Do GLM cross validation training on (1 - 1/outsidefolds) but test on excluded dataset
#for generalisation
crossval.train.data <- train.data
crossval.test.data <- test.data
generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified 
cv_desc = 'Train_CVTrainData_Test_GeneraliseData_'
p_str <- 'G' #(G)eneralise
print('Run GLM with CrossVal Train UHB/NBT 80/20 split, but test generalisation Weston')
source('LABMARCS_LogisticRegression_CrossValidate_GLM_On_Selected_Data.R')

#save things to our summary data table
cv_batch_df2[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3]) 
  write.table(cv_batch_df2, file = paste(output_path, 'GLM_Generalise_Summary_Compendium.csv',sep = ''),
            row.names = FALSE, sep = ',')


#--------------------------------------------------------------------------------------
#Do Lasso cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
crossval.train.data <- train.data
#don't need to specify crossval.test.data as it will be a % from crossval.train.data
generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified  
cv_desc = 'Train_CVTrainData_Test_CVTestData_'
p_str <- 'TL' #(T)rain, text prefix for roc curve compendium variables
print('Run LASSO with CrossVal Train only UHB/NBT 80/20 split...')
source('LABMARCS_LogisticRegression_CrossValidate_LASSO_On_Selected_Data.R')




#save things to our summary data table
cv_batch_df3[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3]) 
  
write.table(cv_batch_df3, file = paste(output_path, 'LASSO_Train_Summary_Compendium.csv',sep = ''),
            row.names = FALSE, sep = ',')

if (m_ctr == 1) { #first time create table
  cv_batch_varlist_df1 <- data.frame(matrix(data = NA, nrow = 1, ncol = 4 + length(varnames[[1]])))
  colnames(cv_batch_varlist_df1)[1:4] <- c('ModelType', 'Reading', 'Day', 'Outcome')
  colnames(cv_batch_varlist_df1)[5:dim(cv_batch_varlist_df1)[2]] <- varnames[[1]]
} 
  
#save variable frequency selection 
cv_batch_varlist_df1[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                                  varcount/(n_models)*100)
  
write.table(cv_batch_varlist_df1, file = paste(output_path, 'LASSO_Variable_Selection_Compendium.csv',
                                       sep = ''), row.names = FALSE, sep = ',')
  
#--------------------------------------------------------------------------------------
  
  
#--------------------------------------------------------------------------------------
#Do Lasso cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
crossval.train.data <- train.data
crossval.test.data <- test.data
generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified 
cv_desc = 'Train_CVTrainData_Test_GeneraliseData_'
p_str <- 'GL' #(T)rain, text prefix for roc curve compendium variables
print('Run LASSO with CrossVal Train UHB/NBT 80/20 split, but test generalisation Weston')
source('LABMARCS_LogisticRegression_CrossValidate_LASSO_On_Selected_Data.R')
  
#save things to our summary data table
cv_batch_df4[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3]) 
  
write.table(cv_batch_df4, file = paste(output_path, 'LASSO_Generalise_Summary_Compendium.csv',sep = ''),
            row.names = FALSE, sep = ',')
#--------------------------------------------------------------------------------------
  

#-----Test Bayesian Logistic Regression-------------------

#get cIS for model on training dataset
crossval.train.data <- train.data
crossval.test.data <- train.data
generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified 
cv_desc = 'Train_CVTrainData_Test_TrainData_'
p_str <- 'BY' #(T)rain, text prefix for roc curve compendium variables
print('Run BAYES with CrossVal Train UHB/NBT 80/20 split, but test generalisation Weston')
source('LABMARCS_LogisticRegression_CrossValidate_BAYES_On_Selected_Data.R')

#save things to our summary data table
cv_batch_df5[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3]) 

write.table(cv_batch_df5, file = paste(output_path, 'BAYES_Train_Summary_Compendium.csv',sep = ''),
            row.names = FALSE, sep = ',')

#k-fold variable selection can't deal with 1/0 needs to be T/F
#for (j in seq(1,dim(train.data)[2])){
#  train.data[,j] = as.logical(train.data[,j])
#  test.data[,j] = as.logical(test.data[,j])
#}
#above doesn't fix... error is Error: New factor levels are not allowed.
#Levels allowed: 'FALSE', 'TRUE' , Levels found: '0', '1'

train.data
crossval.test.data <- test.data
names(train.data) = str_replace_all(names(train.data),"_","")
names(test.data) = str_replace_all(names(test.data),"_","")

# Horseshoe prior from BRMS vignette
#n <- nrow(train.data) # 100
#D <- ncol(train.data[, -1]) # 20
#p0 <- 10 # prior guess for the number of relevant variables
#tau0 <- p0/(D-p0) * 1/sqrt(n) # scale for tau (notice that stan_glm will automatically scale this by sigma)
#prior=prior(horseshoe(scale_global = tau0, scale_slab = 1), class=b),

# In practice, at least 4 chains should be 
# used and 2000 iterations might be required for reliable inference.
# seed=1, chains=4, iter=2000) #try larger value than 2000 to get r_hat close to <1.05

#convert data into logical format
train.data_bool <- train.data
for (i in seq(1,dim(train.data)[2])){
  train.data_bool[,i] <- as.logical(train.data[,i])
}

brmfit <- brm(outcome ~ .,
            data = train.data_bool,
            family = bernoulli(),
            prior = prior(horseshoe(), class = b),
            refresh = 0)

summary(brmfit)
# plot(fit2) <run when exmaining manually else ruins script due to interactivity
mcmc_plot(brmfit, pars = "b")
refmodel <- get_refmodel(brmfit)

#posterior predictive check
pp_check(refmodel$fit, ndraws = 500, alpha = 0.1)

loo_reference_fit <- loo(refmodel$fit)
loo_reference_fit
plot(loo_reference_fit)

pred <- predict(object = refmodel, train.data, type = "response")
roccurve1 <- roc(outcome ~ pred, data = train.data)
auc1 <- auc(roccurve1)

ggroc(roccurve1, legacy.axes = T) +
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
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  geom_text(x = 0.1, y = 1, colour = "black", size = 6,
            label = paste('AUC: ', sprintf("%0.2f",auc1), sep = '') )

SelectedData_str ='Train_FullTrainData_Test_TrainData'
ggsave(paste(save_path, 'Model_BAYES_', SelectedData_str,'_ROC.pdf', sep = ''),device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)



pred <- predict(object = refmodel, test.data, type = "response")
roccurve2 <- roc(outcome ~ pred, data = test.data)
auc2 <- auc(roccurve2)

ggroc(roccurve2, legacy.axes = T) +
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
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  geom_text(x = 0.1, y = 1, colour = "black", size = 6,
            label = paste('AUC: ', sprintf("%0.2f",auc2), sep = '') )

SelectedData_str = 'Train_FullTrainData_Test_TestData'
ggsave(paste(save_path, 'Model_BAYES_', SelectedData_str,'_ROC.pdf', sep = ''),device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)


#-------------------------------------
#evaluate reduced model

#NOTE!! cv_varsel is computationally intense can take 8+ hours on ~500 patients data
#standard is LOO
vs <- cv_varsel(refmodel)
#keeps giving error about new factors not allowed? needs boolean not integer...
vs_k <- cv_varsel(refmodel, cv_method = 'kfold') #,K=20

nv <- suggest_size(vs,alpha = 0.2) #= 80%

# Visualise the most relevant variables in the full model -->
mcmc_areas(as.matrix(refmodel$fit),
           pars = c("b_Intercept", paste0("b_", solution_terms(vs)[1:nv]))) +
  coord_cartesian(xlim = c(-3, 3)) 

#examine variable selection results
solution_terms(vs)
plot(vs, stats = c('auc', 'elpd'))
summary(vs, stats = c('auc', 'elpd'))
ggsave(paste(save_path,  'Bayes-varSelect-AUC-ELPD.pdf',sep = ''), device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

proj_summary = summary(vs, stats = c('auc', 'elpd'))

#EVALUATE REDUCED VARIABLE MODEL
#number of terms to use #default alpha is 0.32 (68%) so variables in 1SD
proj <- project(vs, nv = nv ,seed = 123456,ns = 2000)

# Visualise the projected most relevant variables
posterior_summary(proj)
mcmc_areas(as.matrix(proj), , prob = 0.95, prob_outer = 1) + coord_cartesian(xlim = c(-3, 3))

#We make predictions with the projected submodels. For point estimates we can use method 
#proj_predict. Test inputs can be provided using the keyword newdata. 
#It also the test targets ynew are provided in newdata, then the function evaluates
#the log predictive density at these points. For instance, the following computes 
#the mean of the predictive distribution and evaluates the log density at the training 
#points using the most relevant variables.

#eval internal training data performance
pred <- proj_predict(vs, newdata = train.data, nterms = nv)
y = train.data$outcome
ggplot() + geom_point(aes(x = colMeans(pred), y = y)) +
  labs(x = "train prediction", y = "y")
pr <- as.integer(colMeans(pred) >= 0.5)
# posterior classification accuracy
bayes_train_acc <- mean(xor(pr,as.integer(y == 0)))
# posterior balanced classification accuracy
bayes_train_acc <- (mean(xor(pr[y == 0] > 0.5,as.integer(y[y == 0]))) +
                       mean(xor(pr[y == 1] < 0.5,as.integer(y[y == 1]))))/2
# save ROC curve
roccurve1 <- roc(outcome ~ c( colMeans(pred)), data = train.data)
auc1 <- auc(roccurve1)

ggroc(roccurve2, legacy.axes = T) +
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
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  geom_text(x = 0.1, y = 1, colour = "black", size = 6,
            label = paste('AUC: ', sprintf("%0.2f",auc1), sep = '') )

SelectedData_str ='Train_FullTrainData_Test_TrainData'
ggsave(paste(save_path, 'Model_BAYES_REDUCED', SelectedData_str,'_ROC.pdf', sep = ''),device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#how to get CIs without running variable selection each time? This runs reduced
#var model on 80% slices of data but this isn't right - underestimates variance
#auc_smp=vector()
#for (i in 1:100) {
#  sidx=sample(1:round(nrow(train.data)*0.8))
#  pred <- proj_predict(vs, newdata = train.data[sidx, ], nterms = nv)
#  roccurve1 <- roc(outcome ~ c( colMeans(pred)), data =  train.data[sidx, ])
#  auc1 <- auc(roccurve1)
#  auc_smp[i] = auc1
#}

#eval external validation data performance
pred <- proj_predict(vs, newdata = test.data, nterms = nv)
y = test.data$outcome
ggplot() + geom_point(aes(x = colMeans(pred), y = y)) +
  labs(x = "test prediction", y = "y")
pr <- as.integer(colMeans(pred) >= 0.5)
# posterior classification accuracy
bayes_test_acc <- mean(xor(pr,as.integer(y == 0)))
# posterior balanced classification accuracy
bayes_test_acc <- (mean(xor(pr[y == 0] > 0.5,as.integer(y[y == 0]))) +
                       mean(xor(pr[y == 1] < 0.5,as.integer(y[y == 1]))))/2

# save ROC curve
roccurve2 <- roc(outcome ~ c( colMeans(pred)), data = test.data)
auc2 <- auc(roccurve2)

ggroc(roccurve2, legacy.axes = T) +
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
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  geom_text(x = 0.1, y = 1, colour = "black", size = 6,
            label = paste('AUC: ', sprintf("%0.2f",auc2), sep = '') )

SelectedData_str ='Train_FullTrainData_Test_TestData'
ggsave(paste(save_path, 'Model_BAYES_REDUCED', SelectedData_str,'_ROC.pdf', sep = ''),device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#-----Test Bayesian Logistic Regression-------------------

