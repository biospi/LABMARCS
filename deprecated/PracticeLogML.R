# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 31/01/2021
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
  install.packages("BiocManager")
  a
  BiocManager::install("MLInterfaces")
  BiocManager::install("limma")
  BiocManager::install("multtest")
  install.packages("devtools")
  devtools::install_github("AndrewLawrence/dCVnet",
                           dependencies = TRUE, build_vignettes = TRUE)
  devtools::install_github('nathanvan/parallelsugar')
}

if (LoadLibraries==1) {
  # Load libraries
  library("parallelsugar")
  library("dCVnet")
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
}
  
# DATA PROCESSING ------------------------------------------------------
# Read in data and label variables (?'s treated as NA)
heartdata <- read.table("processed.cleveland.data",sep=",",na.strings = "?")
names(heartdata) <- c("age","sex","cp","trestbps","chol","fbs","restecg",
                   "thalach","exang","oldpeak","slope","ca","thal","num")

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
  # num: diagnosis of heart disease (angiographic disease status)

 # BLOCK 1 - Blood: chol fbs ca
 # BLOCK 2 - Physiological: cp trestbps restecg thalach exang oldpeak slope
 # BLOCK 3 - Demographic: age sex
 # BLOCK 4 - Co-morbidity: thal

# Initital inspection of the data
head(heartdata)
summary(heartdata)
# NOTE: indicates 6 NA values, affecting <2% of the data rows

# Recode outcome variable: presence of heart disease in patient
# compresses all positive heart disease outcomes 1-4 into one 
# binary variable (0 represents absence of heart disease)
heartdata$num <- ifelse(heartdata$num > 0,1,0)

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
training.samples <- createDataPartition(heartdata$num, p = 0.8, list = FALSE)
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
if(IncludeFirthBias==0) {
  # Initial model using all input parameters without Firth's bias
  modelunichol <- glm(num ~ chol,
                data = heartdata, family = "binomial")
  modeldemochol <- glm(num ~ chol + age + sex,
                      data = heartdata, family = "binomial")
  modelcomorbchol <- glm(num ~ chol + age + sex, thal,
                      data = heartdata, family = "binomial")
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  modelunichol <- glm(num ~ chol,
                      data = heartdata, family = "binomial", method="firthglm.fit")
  modeldemochol <- glm(num ~ chol + age + sex,
                       data = heartdata, family = "binomial", method="firthglm.fit")
  modelcomorbchol <- glm(num ~ chol + age + sex + thal,
                         data = heartdata, family = "binomial", method="firthglm.fit")
}

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

# Run model with/without Firth's bias
if(IncludeFirthBias==0) {
  # Initial model using all input parameters without Firth's bias
  model1 <- glm(num ~ age + sex + cp + trestbps + chol + fbs + restecg +
                thalach + exang + oldpeak + slope + ca +thal,
                data = train.data, family = "binomial")
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  model1 <- glm(num ~ age + sex + cp + trestbps + chol + fbs + restecg +
                  thalach + exang + oldpeak + slope + ca +thal,
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
exp(confint(model1))

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(model1)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 1 predictions on test set (prob of >0.5 accepted as positive)
probabilities1 <- predict(object = model1, test.data, type = "response")
predicted.classes1 <- ifelse(probabilities1 > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes1==test.data$num)

# Sensivity and specificity measures
conf_matrix<-table(predicted.classes1,test.data$num)
sensitivity(conf_matrix)
specificity(conf_matrix)

# find AUC and plot ROC curve
g1 <- roc(num ~ probabilities1, data = test.data)
auc(g1)
plot(g1)
BrierScore(model1)

# Model 2

# Run model with/without Firth's bias
if(IncludeFirthBias==0) {
  # Initial model using all input parameters without Firth's bias
  # Using automated stepwise variable selection
  model2 <- glm(num ~., data = train.data, family = binomial)
  stepAIC(model2,trace = TRUE)
} else {
  # Method below utilises the Firth's bias approach
  # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
  # Using automated stepwise variable selection
  model2 <- glm(num ~., data = train.data, family = "binomial",
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
exp(confint(model2))

# Check for multicollinearity in the model variables (any >5 are problematic)
car::vif(model2)
# All are <5 and so we can be reasonably sure that we have met the 
# assumption of collinearity

# Model 2 predictions on test set (prob of >0.5 accepted as positive)
probabilities2 <- predict(object = model2, test.data, type = "response")
predicted.classes2 <- ifelse(probabilities2 > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes2==test.data$num)

# Sensivity and specificity measures
conf_matrix<-table(predicted.classes2,test.data$num)
sensitivity(conf_matrix)
specificity(conf_matrix)

# Plot ROC curve
g2 <- roc(num ~ probabilities2, data = test.data)
auc(g2)
plot(g2)
BrierScore(model2)

# Model 3
# First we perform nested cross-validation to report on the generalised performance
# Set number of outer fold repeats, outer folds and inner folds
repeats <- 10
outsidefolds <- 10
insidefolds <- 10

# Allow randomness to resume
set.seed(NULL)

# Initialise outcome measures
lambda.store <- vector("numeric", length=repeats*outsidefolds)
auc <- vector("numeric", length=repeats*outsidefolds)
brier <- vector("numeric", length=repeats*outsidefolds)

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
    x <- model.matrix(num~., train.data)[,-1]
    # Determine lambda.1se parameter over inner folds for the training data
    cv.lasso <- cv.glmnet(x,train.data$num, alpha = 1, data = train.data,  nfolds = insidefolds,
                          family = "binomial")
    lambda.store[outsidefolds*(j-1)+i] <- cv.lasso$lambda.1se
    model3 <- glmnet(x, train.data$num, alpha = 1, family = "binomial",
                          lambda = cv.lasso$lambda.1se)
    # Test the best predicted lambda on the remaining data in the outer fold
    x.test <- model.matrix(num ~., test.data)[,-1]
    probabilities <- model3 %>% predict(newx = x.test, type="response")
    predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
    # Model accuracy
    mean(predicted.classes==test.data$num)
    
    # Sensivity and specificity measures
    conf_matrix<-table(predicted.classes,test.data$num)
    sens<-sensitivity(conf_matrix)
    spec<-specificity(conf_matrix)
    
    # Plot ROC curve
    roccurve <- roc(num ~ c(probabilities), data = test.data)
    auc[outsidefolds*(j-1)+i]<- auc(roccurve)
    #plot(roccurve)
    # Brier score
    f_t <- probabilities
    o_t <- test.data$num
    brier[outsidefolds*(j-1)+i] <- mean(((f_t) - o_t)^2)
  }
}

mean(auc)
quantile(auc, c(.025, .50, .975)) 
mean(brier)
quantile(brier, c(.025, .50, .975)) 
mean(lambda.store)
quantile(lambda.store, c(.025, .50, .975)) 
hist(auc,plot=TRUE)
hist(brier,plot=TRUE)
hist(lambda.store,plot=TRUE)

# Create the final model over the whole dataset, with the expected performance from nested CV
x <- model.matrix(num~., heartdata)[,-1]
cv.lasso <- cv.glmnet(x,heartdata$num, alpha = 1, data= heartdata,  nfolds = insidefolds,
                      family = "binomial")
plot(cv.lasso)
model3 <- glmnet(x, heartdata$num, alpha = 1, family = "binomial",
                 lambda = cv.lasso$lambda.1se)

# Display regression coefficients
coef(model3)
# Examine odds ratios
exp(coef(model3))

# ---------------------------------------------------------------------
