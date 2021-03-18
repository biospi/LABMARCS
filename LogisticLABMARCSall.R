# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 31/01/2021
# Author: Louis MacGregor

# Input variables (if transformed into a function)
  # Set LookForUpdates to 1 if you want to search for and install newest R version
  LookForUpdates <- 0
  # Set InstallPackAges to 1 if you want to install the packAges listed below all at once
  InstallPackages <- 0
  # Load libraries
  LoadLibraries <- 1
  # outcomeselection (1) all severe outcomes (2) ICU admission (3) death
  outcomeselection <- 3
  # Chose to include Firth's bias in the models
  IncludeFirthBias <- 1
  # Use knn imputation? If not, complete case analysis is used
  ImputationAllowed <- 1
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
}
  
# DATA PROCESSING ------------------------------------------------------
# Read in data depending on day window & best/worst/mean measurement compression
setwd("C:/Users/gq19765/OneDrive - University of Bristol/Documents/ICU PhD/LABMARCS/NewData")
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

fulldata <- subset(fulldata, select = -c(X))

# Initial inspection of the data
head(fulldata)
summary(fulldata)

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

if (Comorbidities==1) {
# Only look at sites with comorbidities
fulldata<-fulldata[(fulldata$Site=="NBT"),]
}

# Remove superfluous data columns 
fulldata <- subset(fulldata, select = -c(died,went_to_icu,severeOutcome,N,coinfection,
                                         admissionDate,dischargeDate,ITU_Start,
                                         ITU_End,deathDate,ID,Site,positiveDate))

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
eventsnumber<-sum(fulldata$outcome,na.rm = "TRUE")

# WHEN WE HAVE OTHER CONTINUOUS VARIABLES ADD THEM HERE
# Gender (Gender)
fulldata$Gender = factor(fulldata$Gender,
                       levels=c("F", "M"))
# Age (Age)
# Age grouped, make into average # NEED TO GET WORKING FOR NEW AGE DATA
fulldata$Age[fulldata$Age == "20-21"] <- "20.5"
fulldata$Age[fulldata$Age == "99-102"] <- "100.5"
fulldata$Age <- as.numeric(fulldata$Age)
# Drop if age is missing
fulldata <- fulldata[!is.na(fulldata$Age), ]


# All others ()
# EXCLUDE CONTINUOUS VARIABLES
for (i in 3:(length(fulldata)-1)) {
  fulldata[,i] = as.factor(fulldata[,i])
}

# BETTER PRACTICE IS TO SET CATAGORY LEVELS SO THAT ORs ARE >1
# I.E. REFERENCE CATAGORY IS THE LOWEST RISK.
#for (i in 3:(length(fulldata)-5)) {
#fulldata[,i] = factor(fulldata[,i], levels=c("Normal","Abnormal"))
#}
#for (i in (length(fulldata)-5):(length(fulldata)-1)) {
#  fulldata[,i] = factor(fulldata[,i], levels=c("FALSE","TRUE"))
#}

# Display summary of the data in this format
head(fulldata)
summary(fulldata)

# DROP IF OUTCOME IS MISSING? DOES THIS OCCUR?

# Drop variables where 70% or more of the values are NA
fulldata <- fulldata[, colMeans(is.na(fulldata)) <= .70]
# Examine dropped variables
summary(fulldata[, colMeans(is.na(fulldata)) > .30])
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
training.samples <- createDataPartition(fulldata$outcome, p = 0.8, list = FALSE)
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
} else {
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
modelunivar <- vector(mode = "list", length = (length(fulldata)-1))
modeldemo <- vector(mode = "list", length = (length(fulldata)-3))
if(IncludeFirthBias==0) {
  for (i in 1:(length(fulldata)-1)) {
    modelunivar[[i]] <- glm(fulldata$outcome ~ fulldata[,i]
                            , family = "binomial")
    summary(modelunivar[[i]])
    exp(modelunivar[[i]]$coefficients[1])
    exp(confint(modelunivar[[i]]))
  }
  for (i in 3:(length(fulldata)-1)) {
    modeldemo[[i]] <- glm(fulldata$outcome ~ fulldata[,i] + fulldata$Age + fulldata$Gender,
                        family = "binomial")
    summary(modelunivar[[i]])
    exp(modelunivar[[i]]$coefficients[1])
    exp(confint(modelunivar[[i]]))
  }
    #PLACE HOLDER CODE TO BE ADAPTED IF WE ADD CO-MORBIDITIES
    #modelcomorb[i] <- glm(fulldata$outcome ~ chol + Age + sex, thal,
    #                      family = "binomial")
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  for (i in 1:(length(fulldata)-1)) {
    modelunivar[[i]] <- glm(fulldata$outcome ~ fulldata[,i]
                            , family = "binomial", method="firthglm.fit")
    summary(modelunivar[[i]])
    exp(modelunivar[[i]]$coefficients[1])
    exp(confint(modelunivar[[i]]))
  }
  for (i in 3:(length(fulldata)-1)) {
    modeldemo[[i]] <- glm(fulldata$outcome ~ fulldata[,i] + fulldata$Age + fulldata$Gender,
                          family = "binomial", method="firthglm.fit")
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
  model1 <- glm(outcome ~.,
                data = train.data, family = "binomial")
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  model1 <- glm(outcome ~ .,
                  data = train.data, family="binomial", method="firthglm.fit")
}

# Summarize the final selected model
summary(model1)

# Return p-values (unadjusted)
coef(summary(model1))[,4]
# Adjust the p-values using benjamini hochberg method
adjustedp1 <- p.adjust(coef(summary(model1))[,4], method = "fdr",
                       n = length(coef(summary(model1))[,4]))
# Return p-values (adjusted)
adjustedp1

# Examine odds ratios and 95% CIs
exp(model1$coefficients)
vcov(model1)
exp(confint.default(model1))

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(model1)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 1 predictions on test set (prob of >0.5 accepted as positive)
probabilities1 <- predict(object = model1, test.data, type = "response")
predicted.classes1 <- ifelse(probabilities1 > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes1==test.data$outcome)

# Sensivity and specificity measures
conf_matrix<-table(predicted.classes1,test.data$outcome)
colnames(conf_matrix)=c(0,1)
sensitivity(conf_matrix)
specificity(conf_matrix)

# find AUC and plot ROC curve
g1 <- roc(outcome ~ probabilities1, data = test.data)
auc(g1)
plot(g1)
BrierScore(model1)

# Model 2

# Run model with/without Firth's bias
if(IncludeFirthBias==0) {
  # Initial model using all input parameters without Firth's bias
  # Using automated stepwise variable selection
  model2 <- glm(outcome ~., data = train.data, family = binomial)
  stepAIC(model2,trace = TRUE)
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  # Using automated stepwise variable selection
  model2 <- glm(outcome ~., data = train.data, family = "binomial",
                     method="firthglm.fit")
    stepAIC(model2,trace = TRUE)
}

# Summarize the final selected model
summary(model2)

# Return p-values (unadjusted)
coef(summary(model2))[,4]
# Adjust the p-values using benjamini hochberg method
adjustedp2 <- p.adjust(coef(summary(model2))[,4], method = "fdr",
                       n = length(coef(summary(model2))[,4]))
# Return p-values (adjusted)
adjustedp2

# Examine odds ratios and 95% CIs
exp(model2$coefficients)
exp(confint.default(model2))

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(model2)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 2 predictions on test set (prob of >0.5 accepted as positive)
probabilities2 <- predict(object = model2, test.data, type = "response")
predicted.classes2 <- ifelse(probabilities2 > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes2==test.data$outcome)

# Sensivity and specificity measures
conf_matrix<-table(predicted.classes2,test.data$outcome)
colnames(conf_matrix)=c(0,1)
sensitivity(conf_matrix)
specificity(conf_matrix)

# Plot ROC curve
g2 <- roc(outcome ~ probabilities2, data = test.data)
auc(g2)
plot(g2)
BrierScore(model2)

# Model 3
# First we perform nested cross-validation to report on the generalised performance
# Set number of outer fold repeats, outer folds and inner folds
repeats <- 10
outsidefolds <- 5
insidefolds <- 5

# Allow randomness to resume
set.seed(NULL)

# Initialise outcome measures
lambda.store <- vector("numeric", length=repeats*outsidefolds)
auc <- vector("numeric", length=repeats*outsidefolds)
brier <- vector("numeric", length=repeats*outsidefolds)
includedvars <- list(length=repeats*outsidefolds)
varnames <- vector(mode = "list", length=repeats*outsidefolds)
varratios <- vector(mode = "list", length=repeats*outsidefolds)
roccurve <- vector(mode = "list", length=repeats*outsidefolds)

for(j in 1:repeats) {
  # Randomly shuffle the data
  fulldata<-fulldata[sample(nrow(fulldata)),]
  # Create 10 equally sized outer folds
  data.outerfolds <- cut(seq(1,nrow(fulldata)),breaks=outsidefolds,labels=FALSE)
  for(i in 1:outsidefolds){
    #Segement your data by fold using the which() function 
    testIndexes <- which(data.outerfolds==i,arr.ind=TRUE)
    test.data <- fulldata[testIndexes, ]
    train.data <- fulldata[-testIndexes, ]
    x <- model.matrix(outcome~., train.data)[,-1]
    # Determine lambda.1se parameter over inner folds for the training data
    cv.lasso <- cv.glmnet(x,train.data$outcome, alpha = 1, data = train.data,  nfolds = insidefolds,
                          family = "binomial")
    lambda.store[outsidefolds*(j-1)+i] <- cv.lasso$lambda.1se
    model3 <- glmnet(x, train.data$outcome, alpha = 1, family = "binomial",
                     lambda = cv.lasso$lambda.1se)
    # Test the best predicted lambda on the remaining data in the outer fold
    x.test <- model.matrix(outcome ~., test.data)[,-1]
    probabilities <- model3 %>% predict(newx = x.test, type="response")
    predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
    # Model accuracy
    mean(predicted.classes==test.data$outcome)
    
    # Sensivity and specificity measures
    conf_matrix<-table(predicted.classes,test.data$outcome)
    colnames(conf_matrix)=c(0,1)
    #sens<-sensitivity(conf_matrix)
    #spec<-specificity(conf_matrix)
    
    # Plot ROC curve
    roccurve[[outsidefolds*(j-1)+i]] <- roc(outcome ~ c(probabilities), data = test.data)
    auc[outsidefolds*(j-1)+i]<- auc(roccurve[[outsidefolds*(j-1)+i]])
    #plot(roccurve)
    # Brier score
    f_t <- probabilities
    o_t <- test.data$outcome
    brier[outsidefolds*(j-1)+i] <- mean(((f_t) - o_t)^2)
    
    modelcoefs <- exp(coef(model3))
    varnames[[outsidefolds*(j-1)+i]] <- modelcoefs@Dimnames[[1]]
    varratios[[outsidefolds*(j-1)+i]] <- modelcoefs@x
    
  }
}

mean(auc)
quantile(auc, c(.025, .50, .975))
t.test(auc)
t.test(auc)$"conf.int"
mean(brier)
quantile(brier, c(.025, .50, .975))
t.test(brier)
t.test(brier)$"conf.int"
mean(lambda.store)
quantile(lambda.store, c(.025, .50, .975))
t.test(lambda.store)
t.test(lambda.store)$"conf.int"
hist(auc,plot=TRUE)
hist(brier,plot=TRUE)
hist(lambda.store,plot=TRUE)

# plot the ROC curves for all models to show uncertainty
ggroc(roccurve, legacy.axes = T) +
  geom_abline(slope = 1 ,intercept = 0) + # add identity line
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size =18, face = 'bold'),
    axis.title.y = element_text(size =18, face = 'bold'),
    panel.border = element_rect(size = 2, fill = NA), 
    axis.text.x = element_text(size = 14, face ='bold'),
    axis.text.y = element_text(size = 14, face ='bold')) +
  xlab('1 - Specificity') +
  ylab('Sensitivity') +
  scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) + 
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25))

if (Comorbidity==0) {
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
mean(EPV)

# (ii) Global model standard errors, variables and coefficients
# Summary
summary(model1)
# Examine odds ratios and 95% CIs
exp(model1$coefficients)
exp(confint.default(model1))

# (iii) Inclusion frequencies for each variable
# Make histogram showing the number of variables included in each model
hist(variablesinmodel, main = "Number of variables selected over all models",
     xlab = "Number of variables", breaks=(0:length(varcount)))
# Make a bar plot of the included variables
par(mar= c(8,4,4,4))
barplotstab <- barplot(varcount[2:length(varcount)]/(repeats*outsidefolds)*100,
        main = 'Model Stability', ylab = "Frequency (%)",
        names.arg = varnames[[1]][2:length(varcount)], las=2, cex.names=0.75)
abline(h=50)
abline(h=20, col="red")
dev.off()

# (iv)

# (v)

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

sum(modelprop$x[1:20]) # determine if need first 20 or 80% cumulative total
# as first 20 results does not comprise 80% of the cummulative models, we should
# present only the first 20 models

# add in cummulative totals
for (i in 1:20) {
modelprop$cummulative[i] <- sum(modelprop$x[1:i])
}

# List of the models of interest, with proportion and cummulative proportions
ModelList <- array(0L, c(20,(ncol(modelvarmatrixunique)+2)))
ModelList[1:20,1:ncol(modelvarmatrixunique)] <- modelvarmatrixunique[1:20,]
ModelList[1:20,(ncol(modelvarmatrixunique)+1)] <- modelprop$x[1:20]
ModelList[1:20,(ncol(modelvarmatrixunique)+2)] <- modelprop$cummulative[1:20]
colnames(ModelList) <- c(varnames[[1]],"Prop", "Cummulative") 

# (vii) Pairwise variable frequencies
for (i in 1:(repeats*outsidefolds)) {
  for (j in 1:length(varratios[[1]])) {
    for (k in 1:length(varratios[[1]])) {
      if (modelvarmatrix[i,j]==1 & modelvarmatrix[i,k]==1) {
        freqpairs[j,k] <- freqpairs[j,k]+1
      }
    }
  }
}

colnames(freqpairs) <- c(varnames[[1]]) 
rownames(freqpairs) <- c(varnames[[1]])
# correlation plot CURRENTLY MESSY AND MORE ROBUST MEASURES ARE NEEDED
corrplot(cor(freqpairs), method="circle")
}

# LOOK AT MINIMUM REPORTING FOR STABILITY ANALYSES FROM STATS RECOMMENDATIONS
#---------------------------------------------------------------------------

# Create the final model over the whole dataset, with the expected performance from nested CV
x <- model.matrix(outcome~., fulldata)[,-1]
cv.lasso <- cv.glmnet(x,fulldata$outcome, alpha = 1, data= fulldata,  nfolds = insidefolds,
                      family = "binomial")
plot(cv.lasso)
model3 <- glmnet(x, fulldata$outcome, alpha = 1, family = "binomial",
                 lambda = cv.lasso$lambda.1se)

# Display regression coefficients
coef(model3)
# Examine odds ratios
exp(coef(model3))

# Test the best predicted lambda on the remaining data in the outer fold
x.test <- model.matrix(outcome ~., test.data)[,-1]
probabilities <- model3 %>% predict(newx = x.test, type="response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes==test.data$outcome)

# Plot ROC curve and find AUC
roccurve3 <- roc(outcome ~ c(probabilities), data = test.data)
auc<- auc(roccurve3)
ggroc(roccurve3, legacy.axes = T) +
  geom_abline(slope = 1 ,intercept = 0) + # add identity line
  theme(
    panel.background = element_blank(), 
    axis.title.x = element_text(size =18, face = 'bold'),
    axis.title.y = element_text(size =18, face = 'bold'),
    panel.border = element_rect(size = 2, fill = NA), 
    axis.text.x = element_text(size = 14, face ='bold'),
    axis.text.y = element_text(size = 14, face ='bold')) +
  xlab('1 - Specificity') +
  ylab('Sensitivity') +
  scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) + 
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25))

# Brier score
f_t <- probabilities
o_t <- test.data$outcome
brier<- mean(((f_t) - o_t)^2)
# ---------------------------------------------------------------------