# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 08/02/2021
# Author: Louis MacGregor

# Data available here:
# https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/
# from link 'processed.cleveland.data'

# Input variables (if transformed into a function)
# Set LookForUpdates to 1 if you want to search for and install newest R version
LookForUpdates <- 0
# Set InstallPackages to 1 if you want to install the packages listed below all at once
InstallPackages <- 0
# Load libraries
LoadLibraries <- 1
# Chose to include Firth's bias in the models
IncludeFirthBias <- 1
# Use knn imputation? If not, complete case analysis is used
ImputationAllowed <- 1

if (LookForUpdates==1) {
  # Update R
  install.packages("installr")
  library(installr)
  updateR()
}


if (InstallPackages==1) {
  # Packages which may require installation (commented out)
  install.packages("DescTools")
  install.packages("moments")
  install.packages("RANN")
  install.packages("VIM")
  install.packages("tidymodels")
  install.packages("tibble")
  install.packages("nlcv")
  install.packages("mbest")
  install.packages("nestfs")
  install.packages("survminer")
  install.packages("survivalROC")
  install.packages("survcomp")
  install.packages("BiocManager")
  a
  BiocManager::install("MLInterfaces")
  BiocManager::install("limma")
  BiocManager::install("multtest")
  BiocManager::install("survcomp")
}

if (LoadLibraries==1) {
  # Load libraries
  library("survcomp")
  library("survivalROC")
  library("nestfs")
  library("a4core")
  library("MLInterfaces")
  library("limma")
  library("multtest")
  library("nlcv")
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
  library("survival")
  library("survminer")
}

# DATA PROCESSING ------------------------------------------------------
# Read in data and label variables (?'s treated as NA)
heartdata <- read.table("processed.cleveland.data",sep=",",na.strings = "?")
names(heartdata) <- c("age","sex","cp","trestbps","chol","fbs","restecg",
                      "thalach","exang","oldpeak","slope","ca","thal","status")

# Variable descriptions for reference:
# age: age in years
# sex: sex (1 = male; 0 = female)
# cp: chest pain type
# -- Value 1: typical angina
# -- Value 2: atypical angina
# -- Value 3: non-anginal pain
# -- Value 4: asymptomatic
# trestbps: resting blood pressure (in mm Hg on admission to the hospital)
# chol: serum cholestoral in mg/dl
# fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
# restecg: resting electrocardiographic results
# -- Value 0: normal
# -- Value 1: having ST-T wave abnormality (T wave inversions and/or
# ST elevation or depression of > 0.05 mV)
# -- Value 2: showing probable or definite left ventricular
# hypertrophy by Estes' criteria
# thalach: maximum heart rate achieved
# exang: exercise induced angina (1 = yes; 0 = no)
# oldpeak = ST depression induced by exercise relative to rest
# slope: the slope of the peak exercise ST segment
# -- Value 1: upsloping
# -- Value 2: flat
# -- Value 3: downsloping
# ca: number of major vessels (0-3) colored by flourosopy
# thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
# status: diagnosis of heart disease (angiographic disease status)

# BLOCK 1 - Blood: chol fbs ca
# BLOCK 2 - Physiological: cp trestbps restecg thalach exang oldpeak slope
# BLOCK 3 - Demographic: age sex
# BLOCK 4 - Co-morbidity: thal

# Add in FICTIONAL time to death for the data, max is set to 100 days
# We treat outcome of heart disease as FICTIONAL death and
# censor non-heart disease outcomes
heartdata$time <- runif(303,0,100)

# Initital inspection of the data
head(heartdata)
summary(heartdata)
# NOTE: indicates 6 NA values, affecting <2% of the data rows

# Recode outcome variable: presence of heart disease in patient
# compresses all positive heart disease outcomes 1-4 into one 
# binary variable (0 represents absence of heart disease)
heartdata$status <- ifelse(heartdata$status > 0,1,0)

# Recode input variable: oldpeak
# Compress values into clinical catagories
heartdata$oldpeak <- ifelse(heartdata$oldpeak < 1,0,
                            ifelse(heartdata$oldpeak>=2,2,1))


# Assign variables as catagorical where appropriate
# Sex (sex)
heartdata$sex[heartdata$sex == 0] = "Female"
heartdata$sex[heartdata$sex == 1] = "Male"
heartdata$sex = factor(heartdata$sex,
                       levels=c("Female", "Male"))

# Chest pain type (cp)
heartdata$cp[heartdata$cp == 1] = "TypAngina"
heartdata$cp[heartdata$cp == 2] = "AtypAngina"
heartdata$cp[heartdata$cp == 3] = "NonAngina"
heartdata$cp[heartdata$cp == 4] = "Asymptomatic"
heartdata$cp = factor(heartdata$cp,
                      levels=c("TypAngina", "AtypAngina","NonAngina",
                               "Asymptomatic"))

# Fasting blood sugar (fbs)
heartdata$fbs[heartdata$fbs == 0] = "False"
heartdata$fbs[heartdata$fbs == 1] = "True"
heartdata$fbs = factor(heartdata$fbs,
                       levels=c("False", "True"))

# Resting electrocardiographic results (restecg)
heartdata$restecg[heartdata$restecg == 0] = "Normal"
heartdata$restecg[heartdata$restecg == 1] = "Abnormality"
heartdata$restecg[heartdata$restecg == 2] = "VentHypertrophy"
heartdata$restecg = factor(heartdata$restecg,
                           levels=c("Normal", "Abnormality",
                                    "VentHypertrophy"))

# Ventricular induced angina (exang)
heartdata$exang[heartdata$exang == 0] = "False"
heartdata$exang[heartdata$exang == 1] = "True"
heartdata$exang = factor(heartdata$exang,
                         levels=c("False", "True"))

# ST depression induced by exercise relative to rest (oldpeak)
# catagories chosen referenced from:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1123032/
heartdata$oldpeak[heartdata$oldpeak == 0] = "Normal"
heartdata$oldpeak[heartdata$oldpeak == 1] = "IndRevIschaemia"
heartdata$oldpeak[heartdata$oldpeak == 2] = "SigRevIschaemia"
heartdata$oldpeak = factor(heartdata$oldpeak,
                           levels=c("Normal", "IndRevIschaemia",
                                    "SigRevIschaemia"))

# The slope of the peak exercise ST segment (slope)
heartdata$slope[heartdata$slope == 1] = "Upsloping"
heartdata$slope[heartdata$slope == 2] = "Flat"
heartdata$slope[heartdata$slope == 3] = "Downsloping"
heartdata$slope = factor(heartdata$slope,
                         levels=c("Upsloping", "Flat","Downsloping"))

# Number of major vessels (0-3) colored by flourosopy (ca)
heartdata$ca[heartdata$ca == 0] = "Zero"
heartdata$ca[heartdata$ca == 1] = "One"
heartdata$ca[heartdata$ca == 2] = "Two"
heartdata$ca[heartdata$ca == 3] = "Three"
heartdata$ca = factor(heartdata$ca,
                      levels=c("Zero", "One","Two","Three"))

#  Thallium stress test (thal)
heartdata$thal[heartdata$thal == 3] = "Normal"
heartdata$thal[heartdata$thal == 6] = "FixedDefect"
heartdata$thal[heartdata$thal == 7] = "ReversibleDefect"
heartdata$thal = factor(heartdata$thal,
                        levels=c("Normal", "FixedDefect",
                                 "ReversibleDefect"))

# Display summary of the data in this format
head(heartdata)
summary(heartdata)

# Drop variables where 70% or more of the values are NA
heartdata <- heartdata[, colMeans(is.na(heartdata)) <= .70]
# No variables dropped in this dataset
# Check the updated dataset
summary(heartdata)

# ----------------------------------------------------------------------

# INPUT EXAMINATION ----------------------------------------------------

# Plot histograms of continuous variables to examine distributions
par(mfrow=c(1,4))
hist(heartdata$age,main="Age",xlab="Years")
hist(heartdata$trestbps,main="Heart rate (admission)",xlab="Beats/minute")
hist(heartdata$chol,main="Cholesterol", xlab="mg/dl")
hist(heartdata$thalach,main="Maximum heart rate",xlab="Beats/minute")

# Calculate the skewness of the continuos variables
# Guide: If skewness is less than -1 or greater than 1 - highly skewed
#        If skewness is between -1 and -0.5 or 0.5 and 1 - moderately skewed
#        If skewness is between -0.5 and 0.5 - approximately symmetric
skewness(heartdata$age, na.rm = TRUE) # approximately symmetric
skewness(heartdata$trestbps, na.rm = TRUE) # moderate +ve skew
skewness(heartdata$chol, na.rm = TRUE) # high +ve skew
skewness(heartdata$thalach, na.rm = TRUE) # moderate negative skew

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
heartdata$trestbps <- sqrt(heartdata$trestbps)
heartdata$chol <- log10(heartdata$chol)
heartdata$thalach <- sqrt(max(heartdata$thalach+1)-heartdata$thalach)

# Check new skewness values
skewness(heartdata$age, na.rm = TRUE) # approximately symmetric
skewness(heartdata$trestbps, na.rm = TRUE) # approximately symmetric
skewness(heartdata$chol, na.rm = TRUE) # approximately symmetric
skewness(heartdata$thalach, na.rm = TRUE) # approximately symmetric

# Plot transformed variables
# Plot histograms of continuous variables to examine distributions
par(mfrow=c(1,4))
hist(heartdata$age,main="Age",xlab="Years")
hist(heartdata$trestbps,main="Heart rate (admission)",xlab="Beats/minute")
hist(heartdata$chol,main="Cholesterol", xlab="mg/dl")
hist(heartdata$thalach,main="Maximum heart rate",xlab="Beats/minute")

# Scale continuous variables to have mean 0 and sd of 1
heartdata$age <- scale(heartdata$age)[, 1]
heartdata$trestbps <- scale(heartdata$trestbps)[, 1]
heartdata$chol <- scale(heartdata$chol)[, 1]
heartdata$thalach <- scale(heartdata$thalach)[, 1]

# Check the scaling has worked as expected
mean(heartdata$age)
sd(heartdata$age)
mean(heartdata$trestbps)
sd(heartdata$trestbps)
mean(heartdata$chol)
sd(heartdata$chol)
mean(heartdata$thalach)
sd(heartdata$thalach)

# Plot regularised variables
# Plot histograms of continuous variables to examine distributions
par(mfrow=c(1,4))
hist(heartdata$age,main="Age",xlab="Years")
hist(heartdata$trestbps,main="Heart rate (admission)",xlab="Beats/minute")
hist(heartdata$chol,main="Cholesterol", xlab="mg/dl")
hist(heartdata$thalach,main="Maximum heart rate",xlab="Beats/minute")

# Plot boxplots to look for outliers
par(mfrow=c(1,4))
boxplot(heartdata$age,main="Age", ylab="Years")
boxplot(heartdata$trestbps,main="Heart rate (admission)",
        ylab="Beats/minute")
boxplot(heartdata$chol,main="Cholesterol", ylab="mg/dl")
boxplot(heartdata$thalach,main="Maximum heart rate", ylab="Beats/Minute")

# Examine continuous variables before windsorisation
summary(heartdata$age)
summary(heartdata$trestbps)
summary(heartdata$chol)
summary(heartdata$thalach)

# Windzorise the data at the 1st and 99% percentile (replace extreme values)
heartdata$age <- Winsorize(heartdata$age, minval = NULL, maxval = NULL,
                           probs = c(0.01, 0.99), na.rm = FALSE, type = 7)
heartdata$trestbps <- Winsorize(heartdata$trestbps, minval = NULL, maxval = NULL,
                                probs = c(0.01, 0.99), na.rm = FALSE, type = 7)
heartdata$chol <- Winsorize(heartdata$chol, minval = NULL, maxval = NULL,
                            probs = c(0.01, 0.99), na.rm = FALSE, type = 7)
heartdata$thalach <- Winsorize(heartdata$thalach, minval = NULL, maxval = NULL,
                               probs = c(0.01, 0.99), na.rm = FALSE, type = 7)

# Examine continuous variables after windsorisation
summary(heartdata$age)
summary(heartdata$trestbps)
summary(heartdata$chol)
summary(heartdata$thalach)

# Summary/visualisation of pairwise correlation between numerical variables
# Guide: High degree: ± 0.50 and ± 1 - strong correlation
#        Moderate degree: ± 0.30 and ± 0.49 - medium correlation
#        Low degree: below ± 0.29 - small correlation.
par(mfrow=c(1,1))
cor(heartdata[,c(1,4,5,8)],method = "spearman")
cor(heartdata[,c(1,4,5,8)],method = "kendall" )
cor(heartdata[,c(1,4,5,8)],method = "pearson" )
corrplot(cor(heartdata[,c(1,4,5,8)]), method="circle")
# Correlation appears to be <0.29 for all variable pairs except for thalach
# and age, suggesting at most weak correlation between pairwise variables.
# However age and thalach appear to have medium correlation and so we may
# want to apply caution including both variables within the same model

# ASSIGN TRAINING AND TEST DATA SETS ----------------------------------

# Split the data into training and test set
set.seed(1)
training.samples <- createDataPartition(heartdata$status, p = 0.8, list = FALSE)
train.data  <- heartdata[training.samples, ]
test.data <- heartdata[-training.samples, ]
summary(train.data)
summary(test.data)

if (ImputationAllowed==0) {
  # Keep only complete cases
  train.data <- train.data[complete.cases(train.data), ]
  test.data <- train.data[complete.cases(test.data), ]
} else {
  # Impute missing values using k-nearest neighbour
  train.data <- kNN(train.data,k=5)
  test.data <- kNN(test.data,k=5)
  heartdata <- kNN(heartdata,k=5)
  # Remove the TRUE/FALSE variables the imputation function adds to the datasets
  train.data <- subset(train.data, select = -c(((ncol(train.data)/2)+1):ncol(train.data)))
  test.data <- subset(test.data, select = -c(((ncol(test.data)/2)+1):ncol(test.data)))
  heartdata <- subset(heartdata, select = -c(((ncol(heartdata)/2)+1):ncol(heartdata)))
}

#----------------------------------------------------------------------

# RUN SIMPLE PRELIMINARY MODELS

# SHOWN HERE IS AN EXAMPLE OF ONE PARAMETER (DO FOR ALL)
  # Initial model
  modelunichol <- coxph(Surv(time, status) ~ chol, data = heartdata)
  modeldemochol <- coxph(Surv(time, status) ~ chol + age + sex, data = heartdata)
  modelcomorbchol <- coxph(Surv(time, status) ~ chol+ age+ sex+ thal, data = heartdata)

# Summaries of these model outputs
summary(modelunichol)
exp(modelunichol$coefficients)
exp(confint(modelunichol))

summary(modeldemochol)
exp(modeldemochol$coefficients)
exp(confint(modeldemochol))

summary(modelcomorbchol)
exp(modelcomorbchol$coefficients)
exp(confint(modelcomorbchol))

#----------------------------------------------------------------------

# RUN THE MAIN MODELS -------------------------------------------------

# Model 1

# Run model using all input parameters without Firth's bias
model1 <- coxph(Surv(time, status) ~ age + sex + cp + trestbps + chol + fbs + restecg +
                  thalach + exang + oldpeak + slope + ca +thal, data = train.data)

# Summarize the final selected model
summary(model1)
exp(model1$coefficients)
exp(confint(model1))

# Return p-values (unadjusted)
summary(model1)$coefficients[,5]
# Adjust the p-values using benjamini hochberg method
adjustedp1 <- p.adjust(summary(model1)$coefficients[,5], method = "fdr",
                       n = length(summary(model1)$coefficients[,5]))
# Return p-values (adjusted)
adjustedp1

# Examine hazard ratios and 95% CIs
exp(model1$coefficients)
exp(confint(model1))

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(model1)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 1 predictions on the test set
# Linear predictor, risk and expected no. events on model1
expectedmodel1 <- predict(model1,newdata=test.data,type="expected")
expectedmodel1
lpmodel1 <- predict(model1,newdata=test.data,type="lp")
riskscoremodel1 <- predict(model1,newdata=test.data,type="risk") # same as exp(lpmodel1)
riskscoremodel1
# Extra details (were in an example, not sure what they mean)
predict(model1,newdata=test.data,type="risk",se.fit=TRUE)
predict(model1,newdata=test.data,type="terms",se.fit=TRUE)
# Survival probability
survprobmodel1 <- exp(-expectedmodel1)
survprobmodel1
# Concordance index
pred_validation1 <- predict (model1, newdata = test.data,type="risk")
pred_validation1
cindex_validation1 <- concordance.index (pred_validation1, surv.time = test.data$time,
                                         surv.event=test.data$status, method = "noether")
cindexmodel1 <- cindex_validation$c.index
cindexmodel1

# CURRENTLY MODEL 2 IS IDENTICAL TO MODEL 1, DUE TO FAKE DATA, MAKE SURE ITS WORKING WITH
# THE ACTUAL DATA

# Model 2
# Stepwise selection
model2 <- coxph(Surv(time, status) ~ age + sex + cp + trestbps + chol + fbs + restecg +
                    thalach + exang + oldpeak + slope + ca +thal, data = train.data)
stepAIC(model2,trace = TRUE)

# Summarize the final selected model
summary(model2)

# Return p-values (unadjusted)
summary(model1)$coefficients[,5]
# Adjust the p-values using benjamini hochberg method
adjustedp2 <- p.adjust(summary(model1)$coefficients[,5], method = "BH",
                       n = length(summary(model1)$coefficients[,5]))
# Return p-values (adjusted)
adjustedp2

# Examine hazard ratios and 95% CIs
exp(model2$coefficients)
exp(confint(model2))

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(model2)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 2 predictions on test set
# Linear predictor, risk and expected no. events on model2
expectedmodel2 <- predict(model2,newdata=test.data,type="expected")
expectedmodel2
lpmodel2 <- predict(model2,newdata=test.data,type="lp")
riskscoremodel2 <- predict(model2,newdata=test.data,type="risk") # same as exp(lpmodel2)
riskscoremodel2
# Extra details (were in an example, not sure what they mean)
predict(model2,newdata=test.data,type="risk",se.fit=TRUE)
predict(model2,newdata=test.data,type="terms",se.fit=TRUE)
# Survival probability
survprobmodel2 <- exp(-expectedmodel2)
survprobmodel2
# Concordance index
pred_validation2 <- predict (model2, newdata = test.data,type="risk")
pred_validation2
cindex_validation2 <- concordance.index (pred_validation2, surv.time = test.data$time,
                                         surv.event=test.data$status, method = "noether")
cindexmodel2 <- cindex_validation$c.index
cindexmodel2

# Model 3
# First we perform nested cross-validation to report on the generalised performance
# Set number of outer fold repeats, outer folds and inner folds
repeats <- 2
outsidefolds <- 10
insidefolds <- 10

# Allow randomness to resume
set.seed(NULL)

# Initialise outcome measures
lambda.store <- vector("numeric", length=repeats*outsidefolds)
cindex <- vector("numeric", length=repeats*outsidefolds)

for(j in 1:repeats) {
  # Randomly shuffle the data
  heartdata<-heartdata[sample(nrow(heartdata)),]
  # Create 10 equally sized outer folds
  data.outerfolds <- cut(seq(1,nrow(heartdata)),breaks=outsidefolds,labels=FALSE)
  for(i in 1:outsidefolds){
    #Segement your data by fold using the which() function 
    testIndexes <- which(data.outerfolds==i,arr.ind=TRUE)
    test.data <- heartdata[testIndexes, ]
    train.data <- heartdata[-testIndexes, ]
    
    # Make input (x)
    x <- model.matrix(Surv(time, status) ~ age + sex + cp + trestbps + chol + fbs + restecg +
                        thalach + exang + oldpeak + slope + ca +thal, data = train.data)
    # Make input (y)
    y <- train.data[,14:15]
    y <- y[,c(2,1)]
    y <- data.matrix(y)
    # Determine lambda.1se parameter over inner folds for the training data
    cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C", nfolds=insidefolds)
    model3 <- glmnet(x, y, alpha = 1, family = "cox",
                     lambda = cvfit$lambda.1se)
    lambda.store[outsidefolds*(j-1)+i] <- cvfit$lambda.1se
    
  # Linear predictor, risk and expected no. events on model3
  xtest <- model.matrix(Surv(time, status) ~ age + sex + cp + trestbps + chol + fbs + restecg +
                          thalach + exang + oldpeak + slope + ca +thal, data = test.data)
  lpmodel3 <- predict(model3,newx=xtest,type="link")
  lpmodel3
  riskscoremodel3 <- predict(model3,newx=xtest,type="response") # same as exp(lpmodel3)
  riskscoremodel3
  
  # Concordance index
  ytest <- test.data[,14:15]
  ytest <- ytest[,c(2,1)]
  ytest <- data.matrix(ytest)
  pred = predict(model3, newx = xtest)
  cindex[outsidefolds*(j-1)+i]<-apply(pred, 2, Cindex, y=ytest)
  cv.glmnet(x, y, family = "cox", type.measure = "C")
  }
}

mean(cindex)
quantile(cindex, c(.025, .50, .975)) 
mean(lambda.store)
quantile(lambda.store, c(.025, .50, .975)) 
hist(cindex,plot=TRUE)
hist(lambda.store,plot=TRUE)

# Create the final model over the whole dataset, with the expected performance from nested CV
x <- model.matrix(Surv(time, status) ~ age + sex + cp + trestbps + chol + fbs + restecg +
                    thalach + exang + oldpeak + slope + ca +thal, data = heartdata)
# Make input (y)
y <- heartdata[,14:15]
y <- y[,c(2,1)]
y <- data.matrix(y)
# Determine lambda parameter (we want to safely maximise lambda to reduce
# the number of features) using 10-fold cross-validation
cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C", nfolds=insidefolds)
plot(cvfit)

model3 <- glmnet(x, y, alpha = 1, family = "cox",
                 lambda = cvfit$lambda.1se)

# Display regression coefficients
coef(model3)
# Examine odds ratios
exp(coef(model3))

# ---------------------------------------------------------------------
