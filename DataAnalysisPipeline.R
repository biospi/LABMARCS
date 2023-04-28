# Code Authors: Brian Sullivan, Louis MacGregor, Ed Barker 

#this script halts Rstudio on errors, instead of blowing past them, note it
#occasionally on an error it sometimes hangs and won't allow return to prompt in the console
#in this case you may need to restart kernel and then comment this portion out until you
#resolve what the error is coming from and fix the bug
#source(paste(work_path, 'StopScriptOnError.R', sep = ''))

# Location of Rscripts & save intermediate processed files
if (!exists('work_path')) {
  work_path <- 'C:/Users/bs16044/OneDrive - University of Bristol/HDR-UK-AMR/LABMARCS/source/'
  setwd(work_path)
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
    dateRange = 3 # 1, 3, 5, 7,14days
    readingwanted = 1 # 0-worst, 1-first, 2-mean
    # outcomeselection (1) all severe outcomes (2) ICU admission (3) death
    outcomeselection = 1
    BatchAnalysisOn = 0
    
    # Cross Validation Parameters
    # Set number of outer fold repeats, outer folds and inner folds
    repeats = 20 # How many times should we shuffle the data
    outsidefolds = 5 #5 #How many splits of the shuffled data (5=80/20%)
    #repeats * outsidefold = number of CV trials, hence 20*5 = 100 
    # can't go lower as small test sets may only have example of one class
    insidefolds = 5 #10KCH (only relevant for LASSO)
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
#library(mice)
#library(nestfs)
#library(MASS)
#library(VIM)
#library(RANN)
#library(moments)
#library(mbest) unused?
#library(md) unused?

#LOAD REQUIRED LIBRARIES
library(dplyr)
library(DescTools)
library(caret)
library(corrplot)
library(glmnet)
library(pROC)
library(tidymodels) # includes yardstick
library(ggplot2)
library(pROC)
library(fastDummies)
library(data.table)
library(patchwork)

#Bayesian packages required  
library(brms)
#Note to use BRMS & ProjPred you'll also need Rtools, Cmnstan, and Cmnstanr:
#https://mc-stan.org/cmdstanr/articles/cmdstanr.html
#https://mc-stan.org/docs/2_29/cmdstan-guide/cmdstan-installation.html
#https://cran.r-project.org/bin/windows/Rtools/
#Prior to ProjPred 2.1.2 needed development projpred version https://github.com/stan-dev/projpred
#devtools::install_github('stan-dev/projpred', ref = 'develop', build_vignettes = TRUE)
library(projpred) 
library(ProbBayes)
library(stringr)
library(bayesplot)
library(ROCR)
library(rstan)
#parallel packages
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
source(paste(work_path, "RemoveLowDataColumns.R", sep = ''))
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



##------------------------------------------------------------------------------
### Create Cross-validation Data

# Precompute a data set to be shared across all models for cross validation
# Use stratification to first sort outcomes and then guarantee each split
# has similar TRUE examples for severe outcomes ~30%
print('Create CV data set shuffled inidices using Repeat/Folds...')
source('Create_CV_DataSet.R')
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
# RUN Single Biomarker models 
##------------------------------------------------------------------------------

#Run single biomarker tests to get an idea of each variables prediction power within the
#training dataset - Due to cross validation of each biomarker for Standard Bayes-Flate + Bayes-Horsehoe
#this will take a long time 20 hours 
if (1) {
  print('Run Single Biomarker tests...')
  #binary_flag = 1
  #bio_fn = 'SingleBiomarker_AgeGen_AllCases_BinaryLevels.csv'
  #Single threaded version, now deprecated
  #source(paste(work_path,'Single_Biomarker_Tests.R', sep = ''))
  
  binary_flag = 0
  bio_fn = 'SingleBiomarker_AgeGen_AllCases_AllLevels_CV'
  source(paste(work_path,'Single_Biomarker_Tests_CrossValidate_with_AgeGenderCV_Control_PARALLEL.R', sep = ''))
  #note bug atm, the parallell results have repetition so need to be manually edited
}



#Given the single tests now remove any invalid/unwanted variables
if (1) {
  print('Removing Invalid Variables...')
  fulldata <- subset(fulldata, select = -c(Nosocomial_1, #only needed to check for sig
                                           PCT_Abnormal, PCT_NA, #missing normal reading values
                                           trig_NA,trig_Abnormal, #97% NA
                                           HBA1c_Abnormal,HBA1c_NA)) #97% NA
  
  train.data <- subset(train.data, select = -c(Nosocomial_1, #only needed to check for sig
                                          PCT_Abnormal, PCT_NA, #missing normal reading values
                                          trig_NA,trig_Abnormal, #97% NA
                                          HBA1c_Abnormal,HBA1c_NA)) #97% NA
                                                                                    
  test.data <- subset(test.data, select = -c(Nosocomial_1, #only needed to check for sig
                                         PCT_Abnormal, PCT_NA, #missing normal reading values
                                         trig_NA,trig_Abnormal, #97% NA
                                         HBA1c_Abnormal,HBA1c_NA)) #97% NA
}




#-------------------------------------------------------------------------------
#--------- RUN Models WITHOUT CROSS VALIDATION ---------------------------------

#prepare data structures for saving across batch loop
if (!BatchAnalysisOn) {
  m_ctr <- 1
  batch_df <- data.frame(ModelType = NA, Reading = NA, Day = NA, Outcome = NA,
                         Accuracy = NA, AUC = NA, Brier = NA, Sensitivity =  NA, 
                         Specificity = NA)
  glm_fulldata_batch_df <- batch_df
  glm_traindata_batch_df1 <- batch_df
  glm_traindata_batch_df2 <- batch_df
  glm_traindata_batch_df3 <- batch_df
  glm_traindata_batch_df4 <- batch_df
  glm_traindata_batch_df5 <- batch_df
  glm_traindata_batch_df6 <- batch_df
  glm_traindata_batch_df7 <- batch_df
  glm_traindata_batch_df8 <- batch_df
  glm_traindata_batch_df9 <- batch_df
  glm_traindata_batch_df10 <- batch_df
  glm_traindata_batch_df11 <- batch_df
  glm_traindata_batch_df12 <- batch_df
  
} else {
  m_ctr <- m_ctr + 1
}


# 3 models, Full data, Internal validation and External
source(paste(work_path,'GLM_Models_without_CV.R', sep = ''))

  
# Note lasso inspired is run later as we need the CV to generate the list of variables to choose
# this method (although used by others) commits a multiple comparison/selective inference error
# as we run lasso and then rerun smaller set of vars on same data, see for a better
# alternative https://cran.r-project.org/web/packages/selectiveInference/index.html
source(paste(work_path,'LASSO_Models_without_CV.R', sep = ''))

#BAYESIAN 
#The bayesian model can take ~30 minutes to converge, set flag to zero if not needed
if (1) {
  #--Bayesian Models with flat and horseshoe priors Internal validation and External---
  #note convergence is slow for flat prior ~30 minutes + ~5 for horseshoe
  source(paste(work_path,'Bayes_Models_without_CV.R', sep = ''))
}


#-------------------------------------------------------------------------------
#-----RUN CROSS VALIDATION MODELS ----------------------------------------------
#-------------------------------------------------------------------------------

#First create data structures to store our results
if (!BatchAnalysisOn) {
  cv_batch_df1 <- data.frame(ModelType = NA, Reading = NA, Day = NA, Outcome = NA,
                  MeanAUC = NA,  AUC_Q2_5 = NA, AUC_Q50 = NA, AUC_Q97_5 = NA, 
                  MeanBrier =  NA, Brier_Q2_5 = NA, Brier_Q50 = NA, Brier_Q97_5 = NA, 
                  MeanLambda =  NA, Lambda_Q2_5 = NA, Lambda_Q50 = NA, Lambda_Q97_5 = NA,
                  MeanSensitivity =  NA, Sensitivity_Q2_5 = NA, 
                  Sensitivity_Q50 = NA, Sensitivity_Q97_5 = NA,
                  MeanSpecificity =  NA, Specificity_Q2_5 = NA, 
                  Specificity_Q50 = NA, Specificity_Q97_5 = NA,
                  Specificity90_Q2_5 = NA, Specificity90_Q50 = NA, Specificity90_Q97_5 = NA,
                  Specificity95_Q2_5 = NA, Specificity95_Q50 = NA, Specificity95_Q97_5 = NA)

  cv_batch_df2 <- cv_batch_df1 
  cv_batch_df3 <- cv_batch_df1 
  cv_batch_df4 <- cv_batch_df1 
  cv_batch_df5 <- cv_batch_df1 
  cv_batch_df6 <- cv_batch_df1 
  cv_batch_df7 <- cv_batch_df1 
  cv_batch_df8 <- cv_batch_df1 
  cv_batch_df9 <- cv_batch_df1 
  cv_batch_df10 <- cv_batch_df1 
  cv_batch_df11 <- cv_batch_df1 
  cv_batch_df12 <- cv_batch_df1 
  cv_batch_df13 <- cv_batch_df1 
  cv_batch_df14 <- cv_batch_df1 
  
  varratios_stat_df <- data.frame() 
}


print('Begin Cross validation...')
#takes about 6 hours
if (1) { # CV takes a while, especially for the Bayesian models turn to 0 to skip
  #----------- RUN CROSS VALIDATION MODELS ---------------------
  # Note this portion takes about 6 hours to run (4 model fits & 4 generalisation) with 5 fold x 20 repeat
  source(paste(work_path,'Run_Models_CrossValidation.R', sep = ''))
  
  #----------- LASSO Models  Internal validation and External---------------------
  # Lasso inspited needs the CV
  # run GLM using LASSO suggested variables  - LASSO differs on each slice of cross validation use >50%
  source(paste(work_path,'LASSO_Inspired_GLM_Models_without_CV.R', sep = ''))
  
}


#-------------------------------------------------------------------------------
##---------------- Projective Prediction---------------------- ##
# To run projective prediction with variable selection
# Not variable selection (if LOO) takes about 1-2 days to run on 8 core i7
#once run this can be commented out and precomputed variables can be loaded late
source(paste(work_path, "ProjectivePredictionVariableSelect.R", sep = ''))



# If use_precomputed==1 then Using pre-computed results for variable selection via BioSpi, 
# else compute again (which will take a long time if using LOO with cv_varsel)
use_precomputed = 0
cv_style = 'KFold' #'LOO' or 'KFold

vs_flag = FALSE   #(vs) validate_search = FALSE variable selection only once on full data set
#TRUE is default and does var selection every time (but order presented in out put is
#for all data)

nt_max = 70

#Version that ran all Variables (70 total + outcome)
#fn = 'ProjPred_CrossVal_Sep16_N472_VARS70.RData' # Sep 16 2022 - n=472 Data, 70 variables (simple_vs - naive)

#version that removed any variables in above analysis that had NA as the top predictor     
#fn = 'ProjPred_SelectedVar_Model_v2_N590_VARS51.RData' #n=590, 51 variables, August 26 2022, cv_vs

#version with crossvalidation performance to compare against distribution from 100 GLM models 
fn = 'ProjPred_CrossVal_3biomarker_model_Sep16.RData' # Sep 16 2022

tr_backup = train.data
test_backup = test.data
source(paste(work_path, 'ProjectivePrediction_Compute.R', sep = ''))

#Note using pre-saved variable selection results means that some dataframe may need to be reinstated
#these get overwritten if loading from compute above
tr_backup = train.data
test_backup = test.data
names(tr_backup) = str_replace_all(names(tr_backup),"_","")
names(test_backup) = str_replace_all(names(test_backup),"_","")
#convert some booleans to integers as needed to comply with prior variable selection

for (cnm in colnames(tr_backup)) {
  tr_backup[cnm] = as.numeric(tr_backup[cnm])
}

#recast as numeric?
for (ii in c(1:dim(tr_backup)[2])) { #skip #2 age
  tr_backup[,ii] <- as.numeric(tr_backup[,ii])
}

for (ii in c(1:dim(test_backup)[2])) { #skip #2 age
  test_backup[,ii] <- as.numeric(test_backup[,ii])
}

#now evaluate, plot and visualize
source(paste(work_path, 'ProjectivePrediction_Eval.R', sep = ''))

#run one last round of cross validation with our reduced variable model




