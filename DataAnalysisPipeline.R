# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 31/01/2021
# Author: Louis MacGregor & Brian Sullivan

# Location of Rscripts & save intermediate processed files
if (!exists('work_path')) {
  work_path <- 'C:/Users/bs16044/OneDrive - University of Bristol/HDR-UK-AMR/LABMARCS/source/'
}

#Location of graphs and tables outputted
if (!exists('output_path')) {
  output_path <- paste(work_path, 'output/', sep = '')
}

# Input variables (if transformed into a function)
# Set LookForUpdates to 1 if you want to search for and install newest 
#R version
LookForUpdates <- 0

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
    readingwanted <- 1 # 0-worst, 1-first, 2-mean
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
  install.packages(installr)
  library(installr)
  updateR()
}


#LOAD REQUIRED LIBRARIES
library(mice)
library(nestfs)
library(dplyr)
library(MASS)
library(VIM)
library(RANN)
library(moments)
library(DescTools)
library(caret)
library(corrplot)
library(glmnet)
library(pROC)
library(tidymodels)
library(mbest)
library(ggplot2)
library(pROC)
library(md)
library(fastDummies)
library(data.table)
library(patchwork)
#Bayesian packages required  
#Note to use BRMS & ProjPred you'll also need Rtools, Cmnstan, and Cmnstanr:
#https://mc-stan.org/cmdstanr/articles/cmdstanr.html
#https://mc-stan.org/docs/2_29/cmdstan-guide/cmdstan-installation.html
#https://cran.r-project.org/bin/windows/Rtools/
#Prior to ProjPred 2.1.2 needed development projpred version https://github.com/stan-dev/projpred
#devtools::install_github('stan-dev/projpred', ref = 'develop', build_vignettes = TRUE)
library(brms)
library(projpred) 
library(ProbBayes)
library(stringr)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(ROCR)
library(rstan)

library(foreach)
library(doSNOW)
library(parallel)

#should help speed up BRM
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=native -j16' )
#Sys.setenv(MAKEFLAGS = '-j16' )
rstan_options(auto_write = TRUE)



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

# Remove rows without positive test result 
fulldata <- fulldata[complete.cases(fulldata[,"positiveDate"]),]

if (Comorbidities == 1) {
  # Only look at sites with comorbidities
  fulldata <- fulldata[(fulldata$Site == "NBT"),]
}

#exclude cases that started in hospital #removes about 27%
#fulldata <- fulldata[fulldata$OnAdmission == TRUE,]

#print some general patient stats 
sink(paste(save_path,'ALL_Patient_Numbers.txt',sep = ''))
site_ls = c('NBT','Weston','UHB1','UHB2')

print('All O2 ICU Died Both')
for (site in site_ls) {
  print(paste(site, as.character(sum(fulldata$Site == site)),
              as.character(sum(fulldata$Site == site & (fulldata$O2_val != 'Test not taken') )),
              as.character(sum(fulldata$Site == site & fulldata$went_to_icu)),
              as.character(sum(fulldata$Site == site & fulldata$died)),
              as.character(sum(fulldata$Site == site & fulldata$died & fulldata$went_to_icu))))
}

#now by biomarker
bio_ls = c("BE_val","BNP_val","CRP_val","DDM_val","eGFR_val","FER_val", "fib_val",
         "Glucose_val","HB_val","HBA1c_val","LDH_val","PCT_val","PLT_val","trig_val",
         "trop_val","Lymphocytes","Neutrophils","WCC","NLR_val","APTT_val","PT_val",
         "poctLAC_val","O2_val","CO2_val","poctpH_val","viral_coinfection",
         "bc_coinfection","resp_coinfection","urine_coinfection","Urea_val")

print('All O2 ICU Died Both')
for (bio in bio_ls) {
  tmp = fulldata[, names(fulldata) == bio]
  print(paste(bio, as.character(sum( !((tmp == 'Test not taken') | (tmp == 'NA')) )),
              as.character(sum( !((tmp == 'Test not taken') | (tmp == 'NA')) & (fulldata$O2_val != 'Test not taken'))),
              as.character(sum( !((tmp == 'Test not taken') | (tmp == 'NA')) & fulldata$went_to_icu)),
              as.character(sum( !((tmp == 'Test not taken') | (tmp == 'NA')) & fulldata$died)),
              as.character(sum( !((tmp == 'Test not taken') | (tmp == 'NA')) & fulldata$died & fulldata$went_to_icu))) )
}
sink()


#save for filtering test/train data
siteData <- fulldata$Site

# Remove unneeded data columns 
if (1) { #omit if column info needed after data pruning for counting patient numbers
  fulldata <- subset(fulldata, select = -c(died,went_to_icu,severeOutcome,coinfection, #N (absent?)
                                           admissionDate,dischargeDate,ITU_Start,
                                           ITU_End,deathDate,ID,Site,positiveDate,
                                           CriticalDate,CriticalDateType)) 
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
pdf(paste(save_path, 'Age_Histogram.pdf',sep = ''))
nbreaks <- pretty(range(fulldata$Age),n = 20)
xlab = paste('Age, bin width=', as.character(nbreaks[2] - nbreaks[1]))
hist(fulldata$Age,main = "Distribution of Patient Ages",xlab = xlab, breaks = nbreaks)
dev.off()

# Drop if age is missing First site then fulldata
siteData <- siteData[!is.na(fulldata$Age)]
fulldata <- fulldata[!is.na(fulldata$Age), ]

#convert fulldata to factors else some stats libraries won't work
if (1) { #omit if counting participants nums (factors won't allow logical indexing)
  for (i in 3:(length(fulldata) - 1)) {
    fulldata[,i] = as.factor(fulldata[,i])
  }
}


# Display summary of the data in this format
summary(fulldata)

# ----------------------------------------------------------------------



# ASSIGN TRAINING AND TEST DATA SETS ----------------------------------

#----------------------------------------------------------------------
#create dummy variables (glm can do for you but better to manually specify)
fulldata_origin_filter <- fulldata


#Dummy columns should be created manually instead of online via GLM as sometimes
#low data presence means some columns aren't created in cross-validation
#Further, some variables are not worth keeping due to low data coverage
#We have several ways to exclude see create_dummy_var_list.r

#Currently using: Only vars that are significant in Univariate GLM
fulldata_nodummy <- fulldata

#Use only select hospitals for train/test
logCond = siteData == 'NBT' | siteData == "UHB1" | siteData == "UHB2"
train.data_NoDummy <- fulldata_nodummy[logCond, ]
test.data_NoDummy <- fulldata_nodummy[!logCond, ] #this is the generalisation test 

#Save histograms of frequencies of different data values recorded per biomarker
sink(paste(save_path,'filtered_variable_histogram_counts.txt', sep = ''))
print('-----TRAIN DATA------')
for (i in 1:dim(train.data_NoDummy)[2]) {
  print(names(train.data_NoDummy)[i])
  print(table(train.data_NoDummy[,i]))
}
print('-----TEST DATA------')
for (i in 1:dim(test.data_NoDummy)[2]) {
  print(names(test.data_NoDummy)[i])
  print(table(test.data_NoDummy[,i]))
}
sink()

#The function below will remove variable columns that have 'NA/Test Not Taken'
#that exceeds some threshold (using 55% default). We opt to instead inlcude missing
#data as test not taken
fulldata_sv <- train.data_NoDummy #assign which dataset to look for missing %
source('RemoveLowDataColumns.R')
#note don't remove here because it disrupts dummy variable creation (which has
#hard coded names)

#fulldata_sv <- test.data_NoDummy #assign which dataset to look for missing %
#source('RemoveLowDataColumns.R') #we should remove based on train not test (but can check)

source(paste(work_path, "CreateDummyVarList.R", sep = ''))
tmp <- dummy_cols(fulldata)
fulldata <- tmp[,dum_var_ls]

#bayesian analyis needs boolean representation and no spaces in names
fulldata_nobool <- fulldata

for (i in c(1,3:dim(fulldata)[2])) { #skip #2 age
  fulldata[,i] <- as.logical(fulldata[,i])
}

#Bayesian analyis needs no spaces in names
names(fulldata) = str_replace_all(names(fulldata),"val","")
names(fulldata) = str_replace_all(names(fulldata),"__","_")
names(fulldata) = str_replace_all(names(fulldata),"Test not taken","NA")

#manually removing variables that have issues PCT (ICU) or too small sample so can't 
#obtain crossvalidated AUC


#manually removing vairbales is almost the same as using 70% missing
#Remove variables that have significant NaNs or have other problems
if (0) {
  fulldata <- subset(fulldata, select = -c(Nosocomial_1, #only needed to check for sig
                                           
                                           #blood gasses likely for ICU or people with oxygen
                                           O2_Abnormal,O2_NA, #79% NA
                                           CO2_Abnormal,CO2_NA, #79% NA
                                           #poctpH_Abnormal,poctpH_NA, #37% NA, BE is good proxy for pH
                                           
                                           #glucose part of bedside finger test not always in digital record
                                           Glucose_Abnormal,Glucose_NA, #not always recorded digitally
                                           
                                           #tests below are highly likely to only be done in ITU
                                           PCT_Abnormal, #missing normal reading values
                                           poctLAC_Abnormal, poctLAC_NA, #79% NA
                                           trig_NA,trig_Abnormal, #97% NA
                                           HBA1c_Abnormal,HBA1c_NA, #97% NA
                                           LDH_Mild,LDH_Moderate,LDH_Severe,LDH_NA, #94% NA
                                           FER_Mild,FER_Moderate,FER_Severe,FER_NA, #86% NA
                                           fib_Mild,fib_Severe,fib_NA, #95% NA
                                           DDM_Abnormal, DDM_NA, #88% NA
                                           BNP_Abnormal,BNP_NA, #93% NA
                                           trop_Abnormal,trop_NA #76% NA      
  ))
}


if (0) {  #testing reduced model - old MAR 2022
  print('Using reduced biomarker set selected from ProjPred')
  #using results from projpred create a reduced variable model and eval
  #keep: 1. urea,2. nlr 3,3. aptt 2,4. be,5. age,6. egfr,7. pt
  fulldata <- subset(fulldata, select = -c(CRP_Abnormal,CRP_NA,
                                           HB_Mild,HB_Moderate,HB_Severe,HB_NA,  
                                           PLT_Mild,PLT_Moderate,PLT_Severe,PLT_NA,
                                           Lymphocytes_Mild,Lymphocytes_Moderate,Lymphocytes_Severe,Lymphocytes_NA,        
                                           Neutrophils_Mild,Neutrophils_Moderate,Neutrophils_Severe,Neutrophils_NA,
                                           WCC_Mild,WCC_Moderate,WCC_Severe,WCC_NA,
                                           viral_coinfection_TRUE,bc_coinfection_TRUE,   
                                           resp_coinfection_TRUE,urine_coinfection_TRUE
                                           )) #only needed to check for sig
}                     
                                
         
#create boolean versions of test/train with dummy variable encoding
train.data <- fulldata[logCond, ]
test.data <- fulldata[!logCond, ] #this is the generalisation test 

# DATA IMPUTATION
if (ImputationAllowed == 0) {

  #If no imputation just keep complete cases
  train.data <- train.data[complete.cases(train.data), ]
  test.data <- test.data[complete.cases(test.data), ]
  
  } else { #Imputation done in separate file
      source(paste(work_path, "ImputeData.R", sep = ''))
}

print("Finished pre-processing")

#Save list of all participants train and test for visualizations
#quick demographics summary
#mean age & std
mean(fulldata$Age) # 71.34994
sd(fulldata$Age) # 17.01069
#num female/male
sum(fulldata$Gender_F) #377 female
sum(fulldata$Gender_F)/dim(fulldata)[1] # 0.4472123
#num of severe outcomes
sum(fulldata$outcome)/dim(fulldata)[1] # 0.3214709

#TRAIN #mean age & std
mean(train.data$Age) # 69.90339
sd(train.data$Age) # 17.57634
#num female/male
sum(train.data$Gender_F) #257 female
sum(train.data$Gender_F)/dim(train.data)[1] # 0.4355932
#num of severe outcomes
sum(train.data$outcome)/dim(train.data)[1] # 0.2949153


#VALIDATION ##mean age & std
mean(test.data$Age) # 74.72332
sd(test.data$Age) # 17.01069
#num female/male
sum(test.data$Gender_F) #377 female
sum(test.data$Gender_F)/dim(test.data)[1] # 0.4472123
#num of severe outcomes
sum(test.data$outcome)/dim(test.data)[1] # 0.3214709

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






## Projective Prediction ##.
#source('ProjectivePrediction.R')




##----------------------------------------------------------------------
# RUN GLM MODELS WITH NO CROSS VALIDATION (For Now)

#Run single biomarker tests to get an idea of each variables prediction power within the
#training dataset - Due to cross validation of each biomarker for Standard Bayes-Flate + Bayes-Horsehoe
#this will take a long time 2-3 days!! on my current laptop (needs to be setup for cluster)
print('Run Single Biomarker tests...')
if (1) {
  #binary_flag = 1
  #bio_fn = 'SingleBiomarker_AgeGen_AllCases_BinaryLevels.csv'
  #source(paste(work_path,'Run_Single_Biomarker_Tests.R', sep = ''))
  
  binary_flag = 0
  bio_fn = 'SingleBiomarker_AgeGen_AllCases_AllLevels_CV'
  source(paste(work_path,'Run_Single_Biomarker_Tests_CrossValidate_with_AgeGenderCV_Control_PARALLEL.R', sep = ''))
}


#Given the single tests now remove any invalid/unwanted variables
if (1) {
  fulldata <- subset(fulldata, select = -c(Nosocomial_1, #only needed to check for sig
                                           PCT_Abnormal, PCT_NA, #missing normal reading values
                                           trig_NA,trig_Abnormal, #97% NA
                                           HBA1c_Abnormal,HBA1c_NA #97% NA
  ))
}


#Run a GLM on all data - our benchmark for best performance
SelectedData <- fulldata
SelectedDataOutcome <- fulldata
SelectedData_str <- paste('TrainFullData_TestFullData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM with full data test/train...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))
  
#save things to our summary data table
glm_fulldata_batch_df[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                   out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_fulldata_batch_df, file = paste(output_path,
                                                'Batch_GLM_TrainFullData_TestFullData_Summary_Table.csv',
                                                sep = ''),
            row.names = FALSE, sep = ',')
  
#Internal Validation  
#Run a GLM only on train.data (UHB,NBT) and test on train.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- train.data
SelectedData_str <- paste('Train_TrainData_Test_TrainData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM only on train data for test/train...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))
  
#save things to our summary data table
glm_traindata_batch_df1[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                    out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df1, file = paste(output_path,
                                                  'Batch_GLM_Train_TrainData_Test_TrainData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')

#External Validation
#Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- test.data
SelectedData_str <- paste('Train_TrainData_Test_GeneraliseData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM, train on UHB/NBT, test on Weston...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))
  
#save things to our summary data table
glm_traindata_batch_df2[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                    out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df2, file = paste(output_path,
                                                  'Batch_GLM_Train_TrainData_Test_GeneraliseData_Summary_Table.csv',
                                                  sep = ''),
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
  cv_batch_df6 <- cv_batch_df1 
  
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
source('CrossValidate_GLM_On_Selected_Data.R')

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

write.table(cv_batch_df1, file = paste(output_path, 
                                       'GLM_CV_Train_Summary_Compendium.csv',
                                       sep = ''),
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
source('CrossValidate_GLM_On_Selected_Data.R')

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
  write.table(cv_batch_df2, file = paste(output_path, 
                                         'GLM_CV_Generalise_Summary_Compendium.csv'
                                         ,sep = ''),
            row.names = FALSE, sep = ',')

  
  

#--------------------------------------------------------------------------------------
#Do Lasso cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
crossval.train.data <- train.data
#don't need to specify crossval.test.data as it will be a % from crossval.train.data
generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified  
cv_desc = 'Train_CVTrainData_Test_CVTestData_'
p_str <- 'TL' #(T)rain, text prefix for roc curve compendium variables
print('Run LASSO with CrossVal Train only UHB/NBT 80/20 split...')
source('CrossValidate_LASSO_On_Selected_Data.R')

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
  cv_batch_varlist_df1 <- data.frame(matrix(data = NA, nrow = 1, 
                                            ncol = 4 + length(varnames[[1]])))
  colnames(cv_batch_varlist_df1)[1:4] <- c('ModelType', 'Reading', 'Day', 'Outcome')
  colnames(cv_batch_varlist_df1)[5:dim(cv_batch_varlist_df1)[2]] <- varnames[[1]]
} 
  
#save variable frequency selection 
cv_batch_varlist_df1[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                                  varcount/(n_models)*100)
  
write.table(cv_batch_varlist_df1, file = paste(output_path, 
                                               'LASSO_Variable_Selection_Compendium.csv',
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
source('CrossValidate_LASSO_On_Selected_Data.R')
  
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
  
write.table(cv_batch_df4, file = paste(output_path, 
                                       'LASSO_Generalise_Summary_Compendium.csv',
                                       sep = ''),
            row.names = FALSE, sep = ',')
#--------------------------------------------------------------------------------------
  

#-----Test Bayesian Logistic Regression-------------------

#Training Data performance
crossval.train.data <- train.data
crossval.test.data <- train.data
generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified 
cv_desc = 'Train_CVTrainData_Test_TrainData_'
p_str <- 'BY' #(T)rain, text prefix for roc curve compendium variables
print('Run BAYES with CrossVal Train UHB/NBT 80/20 split, test on Train')
source('CrossValidate_BAYES_On_Selected_Data.R')

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

write.table(cv_batch_df5, file = paste(output_path, 
                                       'BAYES_Train_Summary_Compendium.csv',
                                       sep = ''),
            row.names = FALSE, sep = ',')


#-----Generalise Performance
crossval.train.data <- train.data
crossval.test.data <- test.data
generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified 
cv_desc = 'Train_CVTrainData_Test_GeneraliseData_'
p_str <- 'BY' #(T)rain, text prefix for roc curve compendium variables
print('Run BAYES with CrossVal Train UHB/NBT 80/20 split, test on Generalise')
source('CrossValidate_BAYES_On_Selected_Data.R')

#save things to our summary data table
cv_batch_df6[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3]) 

write.table(cv_batch_df6, file = paste(output_path, 
                                       'BAYES_Generalise_Summary_Compendium.csv',
                                       sep = ''),
            row.names = FALSE, sep = ',')



## Projective Prediction ##.
source('ProjectivePrediction.R')


