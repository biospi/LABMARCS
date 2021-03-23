# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 31/01/2021
# Author: Louis MacGregor

# Pre-set switches in the code (change to modify desired behaviours)
  # Set LookForUpdates to 1 if you want to search for and install newest R version
  LookForUpdates <- 0
  # Set InstallPackAges to 1 if you want to install the packAges listed below all at once
  InstallPackages <- 0
  # Load libraries
  LoadLibraries <- 1
  # outcomeselection (1) all severe outcomes (2) ICU admission (3) death
  outcomeselection <- 1
  # Chose to include Firth's bias in the models
  IncludeFirthBias <- 0
  # 0 - No imputation, 1- K nearest neighbour imputation, 2- MICE imputation
  ImputationAllowed <- 2
  # Model exclusively looking at comordbities (looks only at 1 hospital)
  Comorbidities <- 0
  # MasterAnalysisOn==1 prevents the dateRange and readingwanted variables from
  # being overwritten, but the option to turn this off and run this script without
  # first running MasterAnalysis.R is available by setting MasterAnalysisOn==0 and then
  # manually selecting the dateRange and readingwanted variables.
  MasterAnalysisOn <- 0
  if (MasterAnalysisOn==0) {
    dateRange <- 5 # 1, 3 or 5 days
    readingwanted <- 1 # 0-worst, 1-first, 2-mean
  }

if (LookForUpdates==1) {
  # Update R
  install.packAges("installr")
  library(installr)
  updateR()
}


if (InstallPackages==1) {
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
  install.packages("coxph")
  install.packages("survcomp")
}

if (LoadLibraries==1) {
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
  library("MASS")
  library("glmnet")
  library("pROC")
  library("tidymodels")
  library("mbest")
  library("ggplot2")
  library("pROC")
  library("coxphf")
  library("survival")
  library("survcomp")
  library("data.table")
}
  
# DATA PROCESSING ------------------------------------------------------
# Read in data depending on day window & best/worst/mean measurement compression
setwd("C:/Users/lm13381/OneDrive - University of Bristol/Documents/AMR Project/LABMARCS/Code/LABMARCS-main/LABMARCS-main")
  if (dateRange==1) {
    if (readingwanted==0) {
      fulldata <- read.csv(file="totalBinary1worst.csv",na.strings=c(""))
    }
    if (readingwanted==1) {
      fulldata <- read.csv(file="totalBinary1first.csv",na.strings=c(""))
    }
    if (readingwanted==2) {
      fulldata <- read.csv(file="totalBinary1mean.csv",na.strings=c(""))
    }
  }
  if (dateRange==3) {
    if (readingwanted==0) {
      fulldata <- read.csv(file="totalBinary3worst.csv",na.strings=c(""))
    }
    if (readingwanted==1) {
      fulldata <- read.csv(file="totalBinary3first.csv",na.strings=c(""))
    }
    if (readingwanted==2) {
      fulldata <- read.csv(file="totalBinary3mean.csv",na.strings=c(""))
    }
  }
  if (dateRange==5) {
    if (readingwanted==0) {
      fulldata <- read.csv(file="totalBinary5worst.csv",na.strings=c(""))
    }
    if (readingwanted==1) {
      fulldata <- read.csv(file="totalBinary5first.csv",na.strings=c(""))
    }
    if (readingwanted==2) {
      fulldata <- read.csv(file="totalBinary5mean.csv",na.strings=c(""))
    }
  }
  
# Get current working directory so can revert at the end of the code
mainDir <- getwd()
# Name of folder for saving results from this model
subDir <- "CoxOutputs"
# Create directory for saving outputs (works fine if it already exists)
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
# Set current working directory to this folder
setwd(file.path(mainDir, subDir))

  # remove superfluous column
fulldata <- subset(fulldata, select = -c(X))

# change dates to their appropriate class (character to date)
fulldata$deathDate <- as.Date(fulldata$deathDate, format =  "%Y-%m-%d")
fulldata$ITU_Start <- as.Date(fulldata$ITU_Start, format =  "%Y-%m-%d")
fulldata$positiveDate <- as.Date(fulldata$positiveDate, format =  "%Y-%m-%d")
fulldata$dischargeDate <- as.Date(fulldata$dischargeDate, format =  "%Y-%m-%d")
fulldata$admissionDate <- as.Date(fulldata$admissionDate, format =  "%Y-%m-%d")

# for those who are classed as +ve on admission, set +ve date to admision date
fulldata$positiveDate <- fifelse(fulldata$OnAdmission==TRUE,fulldata$admissionDate,
                                fulldata$positiveDate)

# drop people who do not have a positive date (NEED TO FOLLOW UP WHY)
fulldata <- subset(fulldata, !is.na(positiveDate))

# calculate days until outcomes, 9999 reprents absence of outcome
fulldata$daystodeath <- as.numeric(fulldata$deathDate) - as.numeric(fulldata$positiveDate)
fulldata$daystoICU <- as.numeric(fulldata$ITU_Start) - as.numeric(fulldata$positiveDate)
fulldata$daystodeath[is.na(fulldata$daystodeath)] <- 9999
fulldata$daystoICU[is.na(fulldata$daystoICU)] <- 9999
fulldata$daystosevereOutcome <- pmin(fulldata$daystodeath,fulldata$daystoICU)
fulldata$daystodischarge <- as.numeric(fulldata$dischargeDate)- as.numeric(fulldata$positiveDate)

# drop people who have -ve discharge dates (NEED TO FOLLOW UP WHY)
fulldata <- subset(fulldata, daystodischarge>-1)

# Select outcome variable (1) all severe outcomes (2) ICU admission (3) death
if (outcomeselection == 1) {
fulldata$status <- fulldata$severeOutcome
fulldata$time <- fifelse(fulldata$severeOutcome==TRUE,fulldata$daystosevereOutcome,
                        fulldata$daystodischarge)
                      

} else if (outcomeselection == 2) {
  fulldata$status <- fulldata$went_to_icu
  fulldata$time <- fifelse(fulldata$went_to_icu==TRUE,,fulldata$daystoICU,
                          fulldata$daystodischarge)
  
} else {
  fulldata$status <- fulldata$died
  fulldata$time <- fifelse(fulldata$died==TRUE,fulldata$daystodeath,
                          fulldata$daystodischarge)
}
fulldata$time <- as.numeric(fulldata$time)
fulldata$status <- as.numeric(fulldata$status)

# drop any rows which have a negative time column (can happen for example if
# COVID is caught in ICU and outcome is 'sent to ICU') although a thorough investigation is
# perhaps needed to make sure this is the only scenario
# drop people who have -ve discharge dates (NEED TO FOLLOW UP WHY)
fulldata <- subset(fulldata, time>-1)

# WARNING: THIS SECTION NEEDS WORK
# there are times with values of 0, which needs addressing. This occurs for exampple
# when someone is sent to ICU the same day of their admission/postive test result
# for the time being we drop these troublesome values, but they need addressing
fulldata <- subset(fulldata, time >0)
min(fulldata$time)
# there are also days with values of 9999 which indicates missing data at some point the
# dates of interest, so we again ignore these for now
fulldata <- subset(fulldata, time <9999)
max(fulldata$time)

if (Comorbidities==1) {
# Only look at sites with comorbidities
fulldata<-fulldata[(fulldata$Site=="NBT"),]
}

# Remove superfluous data columns 
fulldata <- subset(fulldata, select = -c(died,went_to_icu,severeOutcome,N,coinfection,
                                         admissionDate,dischargeDate,ITU_Start,
                                         ITU_End,deathDate,ID,Site,positiveDate,
                                         daystodeath,daystoICU,daystodischarge,
                                         daystosevereOutcome))

if (Comorbidities==0) {
# Remove comorbidities as variables if we aren't considering them
fulldata <- subset(fulldata, select = -c(PSI,NYHA_Heart_failure,CRB65_Score,
                                      NEWS2_Score,COPD,Asthma,Bronchiectasis,
                                      Respiratory_Disease_other, Hypertension,
                                      Chronic_Kidney_Disease,Liver_disease,
                                      Diabetes,CVA_Stroke,TIA_mini_stroke,
                                      Immunodeficiency, HIV_positive))
}

# number of events
eventsnumber<-sum(fulldata$status,na.rm = "TRUE")

# Replace 'test not taken' with NA. Comment out to remove this step if wanted HOWEVER:
# NOTE THAT THE MODEL HAS 'ALIASING ERRORS' WHEN NOT INCLUDING THIS LINE. NOT SURE WHY AT
# CURRENT. MICE RUNS EITHER WAY (I.E. NO VARIABLES HAVE NA'S INCLUDED AFTER IMPUTATION)
# BUT THIS MIGHT BE AS WE DROP VARIABLES WITH HIGH LEVELS OF MISSINGNESS WITH THIS LINE
# SO MICE MAY NEED TO EXPAND ITS REACH IF WE WANT TO INCLUDE IMPUTATION ON FURTHER VARIABLES
fulldata[fulldata == "Test not taken"] <- NA

# WHEN WE HAVE OTHER CONTINUOUS VARIABLES ADD THEM HERE
# Gender (Gender)
fulldata$Gender = factor(fulldata$Gender,
                       levels=c("F", "M"))
# Age (Age)
# Age grouped, make into average
fulldata$Age[fulldata$Age == "20-21"] <- "20.5"
fulldata$Age[fulldata$Age == "99-102"] <- "100.5"
fulldata$Age <- as.numeric(fulldata$Age)
# Drop if age is missing
fulldata <- fulldata[!is.na(fulldata$Age), ]


# All others ()
for (i in 3:(length(fulldata)-2)) {
  fulldata[,i] = as.factor(fulldata[,i])
}

# BETTER PRACTICE IS TO SET CATAGORY LEVELS SO THAT ORs ARE >1
# I.E. REFERENCE CATAGORY IS THE LOWEST RISK. THIS SHOULD BE DONE WHEN
# THE FINAL MODEL IS DECIDED AND THE CATAGORY OF LOWEST RISK IS
# EASILY IDENTIFIED.
# EXAMPLE CODE IS SHOWN BELOW (COMMENTED OUT)
#for (i in X:Y) {
#fulldata[,i] = factor(fulldata[,i], levels=c("Normal","Abnormal"))
#}
#for (i in X:Y) {
#  fulldata[,i] = factor(fulldata[,i], levels=c("FALSE","TRUE"))
#}

# Display summary of the data in this format
head(fulldata)
summary(fulldata)

# Examine variables to be dropped
summary(fulldata[, colMeans(is.na(fulldata)) > .30])
# Drop variables where 70% or more of the values are NA
fulldata <- fulldata[, colMeans(is.na(fulldata)) <= .70]

# Check the updated data
summary(fulldata)

# ----------------------------------------------------------------------

# INPUT EXAMINATION ----------------------------------------------------

# Plot histograms of continuous variables to examine distributions
  par(mfrow=c(1,1))
  hist(fulldata$Age,main="Age",xlab="Years")

# Calculate the skewness of the continuous variables
# Guide: If skewness is less than -1 or greater than 1 - highly skewed
#        If skewness is between -1 and -0.5 or 0.5 and 1 - moderately skewed
#        If skewness is between -0.5 and 0.5 - approximately symmetric
  skewness(fulldata$Age, na.rm = TRUE) # moderate negative skew

# Transform variables which have skew
# Guide: square-root for moderate skew:
#        sqrt(x) for positively skewed data,
#        sqrt(max(x+1) - x) for negatively skewed data
#        log for greater skew:
#        log10(x) for positively skewed data,
#        log10(max(x+1) - x) for negatively skewed data
#        inverse for severe skew:
#        1/x for positively skewed data
#        1/(max(x+1) - x) for negatively skewed data
  fulldata$Age <- log10(max(fulldata$Age+1) - fulldata$Age)
  
# Check new skewness values
  skewness(fulldata$Age, na.rm = TRUE) # nearly approximately symmetric
  
# Plot transformed variables
  # Plot histograms of continuous variables to examine distributions
  par(mfrow=c(1,1))
  hist(fulldata$Age,main="Age",xlab="Years")
  
# Scale continuous variables to have mean 0 and sd of 1
  fulldata$Age <- scale(fulldata$Age)[, 1]
  
# Check the scaling has worked as expected
  mean(fulldata$Age)
  sd(fulldata$Age)
  
# Plot regularized variables
# Plot histograms of continuous variables to examine distributions
  par(mfrow=c(1,1))
  hist(fulldata$Age,main="Age",xlab="Years")
  
# Plot boxplots to look for outliers
  par(mfrow=c(1,1))
  boxplot(fulldata$Age,main="Age", ylab="Years")
  
# Examine continuous variables before windsorisation
  summary(fulldata$Age)

# Windzorise the data at the 1st and 99% percentile (replace extreme values)
  fulldata$Age <- Winsorize(fulldata$Age, minval = NULL, maxval = NULL,
                             probs = c(0.01, 0.99), na.rm = FALSE, type = 7)
  
# Examine continuous variables after windsorisation
  summary(fulldata$Age)

# CURRENTLY NOT RELEVANT AS ONLY ONE CONTINUOUS VARIABLE
# Summary/visualisation of pairwise correlation between numerical variables
# Guide: High degree: between ± 0.50 and ± 1 - strong correlation
#        Moderate degree: between ± 0.30 and ± 0.49 - medium correlation
#        Low degree: below ± 0.29 - small correlation.
#  par(mfrow=c(1,1))
#  cor(fulldata[,c(1,4,5,8)],method = "spearman") #index the continuous vars
#  cor(fulldata[,c(1,4,5,8)],method = "kendall" )
#  cor(fulldata[,c(1,4,5,8)],method = "pearson" )
#  corrplot(cor(fulldata[,c(1,4,5,8)]), method="circle")
  
# ASSIGN TRAINING AND TEST DATA SETS ----------------------------------

  # Ensure that factors are specified for values to be imputed (i.e. NA is not a factor)
  factor_levels = c("Normal","Abnormal")
  
  fulldata <- fulldata %>%
    mutate(
      eGFR_val = factor(eGFR_val,levels=factor_levels, exclude=NA),
      WCC = factor(WCC,levels=factor_levels, exclude=NA),
      Neutrophils = factor(Neutrophils,levels=factor_levels, exclude=NA),
      Lymphocytes = factor(Lymphocytes,levels=factor_levels, exclude=NA),
      NLR_val = factor(NLR_val,levels=factor_levels, exclude=NA),
      HB_val = factor(HB_val,levels=factor_levels, exclude=NA),
      PLT_val = factor(PLT_val,levels=factor_levels, exclude=NA),
      CRP_val = factor(CRP_val,levels=factor_levels, exclude=NA)
    )
  
  # Split the data into training and test set
  set.seed(1)
  training.samples <- createDataPartition(fulldata$status, p = 0.8, list = FALSE)
  train.data  <- fulldata[training.samples, ]
  test.data <- fulldata[-training.samples, ]
  summary(train.data)
  summary(test.data)
  
  # Should imputation go before transforming variables?
  # MAYBE WE NEED TO ADD MULTIPLE IMPUTATION
  # MISSING AT RANDOM NEEDS ADDRESSING
  
  if (ImputationAllowed==0) {
    # Keep only complete cases
    train.data <- train.data[complete.cases(train.data), ]
    test.data <- train.data[complete.cases(test.data), ]
  } 
  if (ImputationAllowed==1) {
    # Impute missing values using k-nearest neighbour
    train.data <- kNN(train.data,k=5)
    test.data <- kNN(test.data,k=5)
    fulldata <- kNN(fulldata,k=5)
    # Remove the TRUE/FALSE variables the imputation function adds to the datasets
    train.data <- subset(train.data, select = -c(((ncol(train.data)/2)+1):ncol(train.data)))
    test.data <- subset(test.data, select = -c(((ncol(test.data)/2)+1):ncol(test.data)))
    fulldata <- subset(fulldata, select = -c(((ncol(fulldata)/2)+1):ncol(fulldata)))
  }
  if (ImputationAllowed==2) {
    # Convert string "NA" to actual NA
    is.na(fulldata)
    # Impute missing values using MICE (Multiple Imputation by Chained Equations)
    # Only looking at eGFR,WCC,Neutrophils,Lymphocytes,NLR,Hb,Platelets (PLT),CRP for time being
    mice_plot <- aggr(fulldata, col=c("navyblue","yellow"),
                      numbers=TRUE, sortVars=TRUE,
                      labels=names(fulldata), cex.axis=.7,
                      gap=3, ylab=c("Missing data","Pattern"))
    
    # NOTES
    # 1) At the moment default imputation method is fine, as we only have 2 factors
    #    specified for our variables of interest above, the default method of 
    #    logistic regression is fine. In future, will need to specify method matrix
    #    so that we can have predictive mean matching for continuous
    # 2) Is there anything we would want to exclude from our predictor matrix that we
    #    haven't excluded already? 
    
    #meth = init$method
    #predM = init$predictorMatrix
    #meth <- vector()
    
    #meth[c("eGFR_val")] = "logreg"
    #meth[c("WCC")] = "logreg"
    #meth[c("Neutrophils")] = "logreg"
    #meth[c("Lymphocytes")] = "logreg"
    #meth[c("NLR_val")] = "logreg"
    #meth[c("HB_val")] = "logreg"
    #meth[c("PLT_val")] = "logreg"
    #meth[c("CRP_val")] = "logreg"
    
    imputed_train <- mice(train.data, m=5, seed=107)
    imputed_test <- mice(test.data, m=5, seed=107)
    imputed_full <- mice(fulldata, m=5, seed=107)
    
    # Just use one of the imputed datasets (3rd out of 5)
    # IDEALLY we should use all 5 and pool the output. Uncertain on how to do this!
    train.data <- complete(imputed_train,3)
    test.data <- complete(imputed_test,3)
    fulldata <- complete(imputed_full,3)
  }
  
  summary(fulldata)

#----------------------------------------------------------------------

# RUN SIMPLE PRELIMINARY MODELS

# Initialise model outputs into lists
modelunivar <- vector(mode = "list", length = (length(fulldata)-2))
modeldemo <- vector(mode = "list", length = (length(fulldata)-4))
if(IncludeFirthBias==0) {
  for (i in 1:(length(fulldata)-2)) {
    modelunivar[[i]] <- coxph(Surv(time,status) ~ fulldata[,i]
                            , data = fulldata)
    summary(modelunivar[[i]])
    exp(modelunivar[[i]]$coefficients[1])
    exp(confint(modelunivar[[i]]))
  }
  for (i in 3:(length(fulldata)-2)) {
    modeldemo[[i]] <- coxph(Surv(time,status) ~ fulldata[,i] + fulldata$Age + fulldata$Gender
                              , data = fulldata)
    summary(modelunivar[[i]])
    exp(modelunivar[[i]]$coefficients[1])
    exp(confint(modelunivar[[i]]))
  }
    #PLACE HOLDER CODE TO BE ADAPTED IF WE ADD CO-MORBIDITIES
    #modelcomorb[i] <- glm(fulldata$status ~ chol + Age + sex, thal,
    #                      family = "binomial")
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  for (i in 1:(length(fulldata)-2)) {
    modelunivar[[i]] <- coxphf(Surv(time,status) ~ fulldata[,i]
                              , data = fulldata)
    summary(modelunivar[[i]])
    exp(modelunivar[[i]]$coefficients[1])
    exp(confint(modelunivar[[i]]))
  }
  for (i in 3:(length(fulldata)-2)) {
    modeldemo[[i]] <- coxphf(Surv(time,status) ~ fulldata[,i] + fulldata$Age + fulldata$Gender
                            , data = fulldata)
    summary(modelunivar[[i]])
    exp(modelunivar[[i]]$coefficients[1])
    exp(confint(modelunivar[[i]]))
  }
  #PLACE HOLDER CODE TO BE ADAPTED IF WE ADD CO-MORBIDITIES. ADD LOOP.
  #modelcomorb[i] <- glm(fulldata$outcome ~ chol + Age + sex, thal,
  #                      family = "binomial", method="firthglm.fit")
  #summary(modelcomorb[[i]])
  #exp(modelcomorb[[i]]$coefficients[1])
  #exp(confint(modelcomorb[[i]]))
}

#----------------------------------------------------------------------

# RUN THE MAIN MODELS -------------------------------------------------

# Model 1

# Run model with/without Firth's bias
if(IncludeFirthBias==0) {
  # Initial model using all input parameters without Firth's bias
  globalmodel <- coxph(Surv(time,status) ~ .
                   , data = fulldata)
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  globalmodel <- coxphf(Surv(time,status) ~ .
                   , data = fulldata)
}

# Summarize the final selected model
summary(globalmodel)

# Return p-values (unadjusted)
coef(summary(globalmodel))[,5]
# Adjust the p-values using benjamini hochberg method
adjustedp1 <- p.adjust(coef(summary(globalmodel))[,5], method = "fdr",
                       n = length(coef(summary(globalmodel))[,5]))
# Return p-values (adjusted)
adjustedp1

# Examine hazard ratios and 95% CIs
exp(globalmodel$coefficients)
exp(confint(globalmodel))
concordance1 <- globalmodel[["concordance"]][["concordance"]]

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(globalmodel)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 1 predictions on the test set
# Linear predictor, risk and expected no. events on globalmodel
expectedglobalmodel <- predict(globalmodel,newdata=test.data,type="expected")
expectedglobalmodel
lpglobalmodel <- predict(globalmodel,newdata=test.data,type="lp")
riskscoreglobalmodel <- predict(globalmodel,newdata=test.data,type="risk") # same as exp(lpglobalmodel)
riskscoreglobalmodel
# Survival probability
survprobglobalmodel <- exp(-expectedglobalmodel)
survprobglobalmodel
# Concordance index
pred_validation1 <- predict (globalmodel, newdata = test.data,type="risk")
pred_validation1
cindex_validation1 <- concordance.index(pred_validation1, surv.time = test.data$time,
                                         surv.event=test.data$status, method = "noether")
cindex1 <- cindex_validation1[["c.index"]]

# Model 2

# Run model with/without Firth's bias
if(IncludeFirthBias==0) {
  # Initial model using all input parameters without Firth's bias
  # Using automated stepwise variable selection
  stepmodel <- coxph(Surv(time,status) ~ .
                  , data = fulldata)
  stepAIC(stepmodel,trace = TRUE)
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  # Using automated stepwise variable selection
  stepmodel <- coxphf(Surv(time,status) ~ .
                  , data = fulldata)
    stepAIC(stepmodel,trace = TRUE)
}

# Summarize the final selected model
summary(stepmodel)

# Return p-values (unadjusted)
summary(globalmodel)$coefficients[,5]
# Adjust the p-values using benjamini hochberg method
adjustedp2 <- p.adjust(summary(globalmodel)$coefficients[,5], method = "BH",
                       n = length(summary(globalmodel)$coefficients[,5]))
# Return p-values (adjusted)
adjustedp2

# Examine hazard ratios and 95% CIs
exp(stepmodel$coefficients)
exp(confint(stepmodel))

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(stepmodel)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 2 predictions on test set
# Linear predictor, risk and expected no. events on stepmodel
expectedstepmodel <- predict(stepmodel,newdata=test.data,type="expected")
expectedstepmodel
lpstepmodel <- predict(stepmodel,newdata=test.data,type="lp")
riskscorestepmodel <- predict(stepmodel,newdata=test.data,type="risk") # same as exp(lpstepmodel)
riskscorestepmodel
# Survival probability
survprobstepmodel <- exp(-expectedstepmodel)
survprobstepmodel
# Concordance index
pred_validation2 <- predict (stepmodel, newdata = test.data,type="risk")
pred_validation2
cindex_validation2 <- concordance.index (pred_validation2, surv.time = test.data$time,
                                         surv.event=test.data$status, method = "noether")
cindex2 <- cindex_validation2[["c.index"]]
cindex2

# Model 3
# First we perform nested cross-validation to report on the generalised performance
# Set number of outer fold repeats, outer folds and inner folds
repeats <- 2
outsidefolds <- 5
insidefolds <- 5

# Allow randomness to resume
set.seed(NULL)

# Initialise outcome measures
lambda.store <- vector("numeric", length=repeats*outsidefolds)
cindex <- vector("numeric", length=repeats*outsidefolds)
varnames <- vector(mode = "list", length=repeats*outsidefolds)
varratios <- vector(mode = "list", length=repeats*outsidefolds)

for(j in 1:repeats) {
  # Randomly shuffle the data
  fulldata<-fulldata[sample(nrow(fulldata)),]
  # Create equally sized outer folds
  data.outerfolds <- cut(seq(1,nrow(fulldata)),breaks=outsidefolds,labels=FALSE)
  for(i in 1:outsidefolds){
    #Segement your data by fold using the which() function 
    testIndexes <- which(data.outerfolds==i,arr.ind=TRUE)
    test.data <- fulldata[testIndexes, ]
    train.data <- fulldata[-testIndexes, ]
    
    # Make input (x)
    x <- model.matrix(Surv(time, status) ~ ., data = train.data)
    # Make input (y)
    y <- train.data[,(length(test.data)-1):(length(test.data))]
    y <- y[,c(2,1)]
    y <- data.matrix(y)
    # Determine lambda.1se parameter over inner folds for the training data
    cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C", nfolds=insidefolds)
    LASSORNCVmodel <- glmnet(x, y, alpha = 1, family = "cox",
                     lambda = cvfit$lambda.1se)
    lambda.store[outsidefolds*(j-1)+i] <- cvfit$lambda.1se
    
    # Linear predictor, risk and expected no. events on LASSOmodel
    xtest <- model.matrix(Surv(time, status) ~ ., data = test.data)
    lpLASSORNCVmodel <- predict(LASSORNCVmodel,newx=xtest,type="link")
    riskscoreLASSORNCVmodel <- predict(LASSORNCVmodel,newx=xtest,type="response") # same as exp(lpLASSOmodel)
    
    # Concordance index
    ytest <- test.data[,(length(test.data)-1):(length(test.data))]
    ytest <- ytest[,c(2,1)]
    ytest <- data.matrix(ytest)
    pred = predict(LASSORNCVmodel, newx = xtest)
    cindex[outsidefolds*(j-1)+i]<-apply(pred, 2, Cindex, y=ytest)
    cv.glmnet(x, y, family = "cox", type.measure = "C")
    
    modelcoefs <- exp(coef(LASSORNCVmodel))
    varnames[[outsidefolds*(j-1)+i]] <- modelcoefs@Dimnames[[1]]
    varratios[[outsidefolds*(j-1)+i]] <- modelcoefs@x
  }
}

mean(cindex)
quantile(cindex, c(.025, .50, .975)) 
mean(lambda.store)
quantile(lambda.store, c(.025, .50, .975)) 
hist(cindex,plot=TRUE)
hist(lambda.store,plot=TRUE)

# The stability analysis code below runs, but I think its using HRs not ORs as intended
# plus its for a cox model ouput so may have mistakes anyway.
if (Comorbidities==0) {
  # Stability analysis--------------------------------------------------------
  # Initialize vectors and arrays
  # Count of the number of of times each variable is used in a model
  varcount <- vector("numeric", length=length(varratios[[1]]))
  # List of the total number of variables in each model
  variablesinmodel <- vector("numeric", length=repeats*outsidefolds)
  # A matrix with model index versus inclusion of each variable in that model
  modelvarmatrix <- array(0L, c(repeats*outsidefolds, length(varratios[[1]])))
  # List of the value of the 'events per variable' in each model
  EPV <- vector("numeric", length=repeats*outsidefolds)
  # List of the frequency of pairwise inclusion of each variable over all models
  freqpairs <- array(0L, c(length(varratios[[1]]), length(varratios[[1]])))
  
  # cycle through each model and each variable counting when variable is used
  # i.e. variable value from LASSO =\= 1.
  # Also count the number of variables used (=\= 1) in each model in total
  for (i in 1:repeats*outsidefolds) {
    varratios[[i]] <- round(varratios[[i]], digits=3)
  }
  
  for (i in 1:(repeats*outsidefolds)) {
    for (j in 1:length(varratios[[1]])) {
      if (varratios[[i]][j] <0.999 | varratios[[i]][j] >1.001) {
        varcount[j] <- varcount[j]+1
        variablesinmodel[i] <- variablesinmodel[i] + 1
        modelvarmatrix[i,j] <- 1
      }
    }
    EPV[i] <- eventsnumber/variablesinmodel[i]
  }
  
  # (i) Range and mean of the events per variable (EPV)
  min(EPV) # If <25 then need a quick warning and may need to state the EPV
  max(EPV)
  EPVmean <- mean(EPV)
  
  # (ii) Global model standard errors, variables and coefficients
  # Summary
  summary(globalmodel)
  # Examine odds ratios and 95% CIs
  globalcoefs <- exp(globalmodel$coefficients)
  globalconfints <- exp(confint.default(globalmodel))
  
  # (iii) Inclusion frequencies for each variable
  # Make histogram showing the number of variables included in each model
  png("stabnovariablesCox.png")
  hist(variablesinmodel, main = "Number of variables selected over all models",
       xlab = "Number of variables", breaks=(0:length(varcount)))
  dev.off()
  # Make a bar plot of the included variables
  par(mar= c(8,4,4,4))
  png(file="stabfreqvariablesCox.png")
  barplotstab <- barplot(varcount[2:length(varcount)]/(repeats*outsidefolds)*100,
                         main = 'Model Stability', ylab = "Frequency (%)",
                         names.arg = varnames[[1]][2:length(varcount)], las=2, cex.names=0.75)
  abline(h=50)
  abline(h=20, col="red")
  dev.off()
  
  # (iv) Root mean squared deviation for the co-efficients (global compared to bootstrap)
  beta_global <- exp(globalmodel$coefficients)
  beta_boot <- varratios
  n_boot <- repeats*outsidefolds
  
  # for variables (i) and bootstraps (j) calculate the sum over all bootstraps of the
  # squared value of the bootstrap variable co-efficient minus the global model coefficient
  beta_global_minus_boot_2_sum <- vector("numeric", length=(length(varcount)))
  RMSD <- vector("numeric", length=(length(varcount)))
  for (i in 1:length(varcount)) {
    for (j in 1:repeats*outsidefolds) {
      beta_global_minus_boot_2 <- (beta_boot[[j]][i]-beta_global[i])^2
      beta_global_minus_boot_2_sum[i] <- beta_global_minus_boot_2_sum[i] +
        beta_global_minus_boot_2 
    }
    RMSD[i] <- sqrt(beta_global_minus_boot_2_sum[i]/n_boot)
  }
  RMSD[2:length(varcount)]
  
  # (v) Relative bias conditional on selection
  beta_boot_total <- vector("numeric", length=(length(varcount)))
  beta_boot_average <- vector("numeric", length=(length(varcount)))
  for (i in 1:length(varcount)) {
    for (j in 1:repeats*outsidefolds) {
      beta_boot_total[i] <- beta_boot_total[i] +beta_boot[[j]][i]
    }
    beta_boot_average[i] <- beta_boot_total[i]/n_boot
  }
  
  varfreq=varcount/(repeats*outsidefolds) # Also called BIF (bootstrap inclusion frequency)
  
  relative_bias <- vector("numeric", length=(length(varcount)))
  for (i in 1:length(varcount)) {
    relative_bias[i] <- ((beta_boot_average[i]/(beta_global[i]*varfreq[i]))-1)*100
  }
  
  # (vi) Most commonly chosen models
  # keep only unique rows (models) in a new matrix and make a copy for manipulting
  modelvarmatrixunique <- unique.matrix(modelvarmatrix)
  tempmatrix <- unique.matrix(modelvarmatrix)
  
  # Count of the frequency of each model used
  modelcount <- vector("numeric", length=nrow(modelvarmatrixunique))
  for (i in 1:nrow(modelvarmatrixunique)) {
    for (j in 1:nrow(modelvarmatrix)){
      if (all(modelvarmatrixunique[i,]==modelvarmatrix[j,])) {
        modelcount[i] <- modelcount[i]+1
      }
    }
  }
  
  # Work out the proportion of times each model is chosen overall
  modelprop <- (modelcount/(repeats*outsidefolds))*100
  # sort and keep the index value for the sorting procedure
  modelprop <- sort(modelprop,decreasing=TRUE, index.return=TRUE)
  
  # apply sorting procedure to the matrix to match up the proportions
  for (i in 1:length(modelprop)) {
    tempmatrix[i,]=modelvarmatrixunique[modelprop$ix[i],]
  }
  modelvarmatrixunique <- tempmatrix
  
  sum(modelprop$x[min(20,nrow(modelvarmatrixunique))]) # determine if need first 20 or 80% cumulative total
  # as first 20 results does not comprise 80% of the cummulative models, we should
  # present only the first 20 models
  
  # add in cummulative totals
  for (i in min(20,nrow(modelvarmatrixunique))) {
    modelprop$cummulative[i] <- sum(modelprop$x[1:i])
  }
  
  # List of the models of interest, with proportion and cummulative proportions
  ModelList <- array(0L, c(20,(ncol(modelvarmatrixunique)+2)))
  ModelList[min(20,nrow(modelvarmatrixunique)),1:ncol(modelvarmatrixunique)] <- modelvarmatrixunique[min(20,nrow(modelvarmatrixunique)),]
  ModelList[min(20,nrow(modelvarmatrixunique)),(ncol(modelvarmatrixunique)+1)] <- modelprop$x[min(20,nrow(modelvarmatrixunique))]
  ModelList[min(20,nrow(modelvarmatrixunique)),(ncol(modelvarmatrixunique)+2)] <- modelprop$cummulative[min(20,nrow(modelvarmatrixunique))]
  colnames(ModelList) <- c(varnames[[1]],"Prop", "Cummulative") 
  
  # (vii) Pairwise variable frequencies
  
  independentprop <- array(0L, c(length(varratios[[1]]), length(varratios[[1]])))
  for (i in 1:(repeats*outsidefolds)) {
    for (j in 1:length(varratios[[1]])) {
      for (k in 1:length(varratios[[1]])) {
        # calculate the independent proportions expected from each pair of variables
        independentprop[j,k] <- (varfreq[j]*varfreq[k])
        # start a count for the number of times two variables appear in the same model
        if (modelvarmatrix[i,j]==1 & modelvarmatrix[i,k]==1) {
          # not this is a count, not a propotion of all models including these to variables
          freqpairs[j,k] <- freqpairs[j,k]+1
        }
      }
    }
  }
  
  # name the columns so you can compare by eye etc
  colnames(freqpairs) <- c(varnames[[1]]) 
  rownames(freqpairs) <- c(varnames[[1]])
  colnames(independentprop) <- c(varnames[[1]]) 
  rownames(independentprop) <- c(varnames[[1]])
  
  # use chi-squared tests on each actual versus expected proportion to see if some pairs are
  # over or under represented. Output is p-value. Any p-values below 0.01 significance are
  # considered as flags
  
  chisquaredpvalue <- array(0L, c(length(varratios[[1]]), length(varratios[[1]])))
  flaggedpairs <- array(0L, c(length(varratios[[1]]), length(varratios[[1]])))
  for (j in 1:length(varratios[[1]])) {
    for (k in 1:length(varratios[[1]])) {
      if (independentprop[j,k]>0 & independentprop[j,k]<1) {
        # currenty using proportional chi^2 test, but if cell sizes are low and warnings
        # continue to occur, then consider using fisher's exact test
        chisquaredtest <- prop.test(x=freqpairs[j,k],n=n_boot, p=independentprop[j,k],correct=FALSE)
        chisquaredpvalue[j,k] <- chisquaredtest[["p.value"]]
        # we also use a p-value of 0.01, but we'd expect 1/100 to be incorrect and we're doing
        # roughly 50*50 = 2500 tests, so consider adjusted p-values or using tighter threshold
        if (chisquaredpvalue[j,k]<=0.01) {
          flaggedpairs[j,k] <- 1
        } else {
          flaggedpairs[j,k] <- 0 
        }
      }
      if (independentprop[j,k]==0 | independentprop[j,k]==1) {
        chisquaredpvalue[j,k] <- NA
        flaggedpairs[j,k] <- NA
      }
    }
  }
  
}

#---------------------------------------------------------------------------

# Create the final model over the whole dataset, with the expected performance from nested CV
x <- model.matrix(Surv(time, status) ~ ., data = fulldata)
# Make input (y)
y <- fulldata[,(length(test.data)-1):(length(test.data))]
y <- y[,c(2,1)]
y <- data.matrix(y)
# Determine lambda parameter (we want to safely maximise lambda to reduce
# the number of features) using 10-fold cross-validation
cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C", nfolds=insidefolds)
plot(cvfit)
LASSOmodel <- glmnet(x, y, alpha = 1, family = "cox",
                     lambda = cvfit$lambda.1se)
lambda.store[outsidefolds*(j-1)+i] <- cvfit$lambda.1se

# Linear predictor, risk and expected no. events on LASSOmodel
xtest <- model.matrix(Surv(time, status) ~ ., data = test.data)
lpLASSOmodel <- predict(LASSOmodel,newx=xtest,type="link")
riskscoreLASSOmodel <- predict(LASSOmodel,newx=xtest,type="response") # same as exp(lpLASSOmodel)

# Concordance index
ytest <- test.data[,(length(test.data)-1):(length(test.data))]
ytest <- ytest[,c(2,1)]
ytest <- data.matrix(ytest)
pred = predict(LASSOmodel, newx = xtest)
cindex[outsidefolds*(j-1)+i]<-apply(pred, 2, Cindex, y=ytest)
cv.glmnet(x, y, family = "cox", type.measure = "C")
# ---------------------------------------------------------------------

# SAVE ALL KEY DATA FROM THE MODELs IN THE subDir-------------

# Save all variables of interest (LOTS OF POTENTIAL EXTRA DETAILS COULD BE SAVED
# BUT I HAVE POPULATED WITH THE MINIMUM NEEDED TO CROSS COMPARE MODELS)

# MODELS
# Global model (applied to singular 80% testset)
saveRDS(globalmodel,"globalmodel.rds")
# Stepwise model (applied to singular 80% testset)
saveRDS(stepmodel,"stepmodel.rds")
# LASSO model (applied to entire dataset)
saveRDS(LASSOmodel,"LASSOmodel.rds")

# UNCERAINTY IN LASSO MODEL PERFORMANCE (REPEATED NESTED CROSS-VALIDATION)
# AUC
saveRDS(AUCRNCV,"AUCRNCV.rds")
# Brier
saveRDS(brierRNCV,"brierRNCV.rds")

# STABILITY ANALYSIS ON THE LASSO MODELS
# (i)
saveRDS(EPVmean, "EPVmean.rds")
# (ii)
saveRDS(globalcoefs,"globalcoefs.rds")
saveRDS(globalconfints,"globalconfints.rds")
# (iii)
saveRDS(varcount,"varcount.rds")
# (iv)
saveRDS(RMSD,"RMSD.rds")
# (v)
saveRDS(relative_bias,"relative_bias.rds")
# (vi)
saveRDS(ModelList,"ModelList.rds")
# (vii)
saveRDS(chisquaredpvalue,"chisquaredpvalue.rds")
saveRDS(flaggedpairs,"flaggedpairs.rds")

# Return working diretory to the parent folder
setwd <- mainDir