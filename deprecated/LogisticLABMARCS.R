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
  outcomeselection <- 1
  # Chose to include Firth's bias in the models
  IncludeFirthBias <- 1
  # Use knn imputation? If not, complete case analysis is used
  ImputationAllowed <- 1

if (LookForUpdates==1) {
  # Update R
  install.packAges("installr")
  library(installr)
  updateR()
}


if (InstallPackages==1) {
  # PackAges which may require installation (commented out)
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
  library("nestfs")
  library("multtest")
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
# Read in data
setwd("//ubht.nhs.uk/userdata/M/MacgregLou/Documents/LABMARCS-main11_02_21/LABMARCS-main")
fulldata <- read.csv(file="totalBinary.csv")
fulldata <- subset(fulldata, select = -c(X))

# Initital inspection of the data
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
# Remove superfluous data columns 
fulldata <- subset(fulldata, select = -c(died,went_to_icu,severeOutcome,
                                         admissionDate,dischargeDate,ITU_Start,
                                         ITU_End,deathDate,ID))

# FOR NOW ALSO REMOVE eGFR_val as all are same value
fulldata <- subset(fulldata, select = -c(eGFR_val))

# Format input variables for the model as catagorical

# Gender (Gender)
fulldata$Gender = factor(fulldata$Gender,
                       levels=c("F", "M"))

# Age (Age)
fulldata$Age <- as.numeric(fulldata$Age)
# All others ()
for (i in 3:(length(fulldata)-1)) {
fulldata[,i] = factor(fulldata[,i], levels=c("Normal","Abnormal"))
}

# Display summary of the data in this format
head(fulldata)
summary(fulldata)

# Drop variables where 70% or more of the values are NA
fulldata <- fulldata[, colMeans(is.na(fulldata)) <= .70]
# Examine dropped variables
summary(fulldata[, colMeans(is.na(fulldata)) > .30])
# Check the updated dataset
summary(fulldata)

# ----------------------------------------------------------------------

# INPUT EXAMINATION ----------------------------------------------------

# Plot histograms of continuous variables to examine distributions
  par(mfrow=c(1,1))
  hist(fulldata$Age,main="Age",xlab="Years")

# Calculate the skewness of the continuos variables
# Guide: If skewness is less than -1 or greater than 1 - highly skewed
#        If skewness is between -1 and -0.5 or 0.5 and 1 - moderately skewed
#        If skewness is between -0.5 and 0.5 - approximately symmetric
  skewness(fulldata$Age, na.rm = TRUE) # moderatel negative skew

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
  
# Plot regularised variables
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
# Guide: High degree: ± 0.50 and ± 1 - strong correlation
#        Moderate degree: ± 0.30 and ± 0.49 - medium correlation
#        Low degree: below ± 0.29 - small correlation.
#  par(mfrow=c(1,1))
#  cor(fulldata[,c(1,4,5,8)],method = "spearman")
#  cor(fulldata[,c(1,4,5,8)],method = "kendall" )
#  cor(fulldata[,c(1,4,5,8)],method = "pearson" )
#  corrplot(cor(fulldata[,c(1,4,5,8)]), method="circle")
# Correlation appears to be <0.29 for all variable pairs except for thalach
# and Age, suggesting at most weak correlation between pairwise variables.
# However Age and thalach appear to have medium correlation and so we may
# want to apply caution including both variables within the same model
  
# ASSIGN TRAINING AND TEST DATA SETS ----------------------------------

# Split the data into training and test set
set.seed(1)
training.samples <- createDataPartition(fulldata$outcome, p = 0.8, list = FALSE)
train.data  <- fulldata[training.samples, ]
test.data <- fulldata[-training.samples, ]
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
  fulldata <- kNN(fulldata,k=5)
  # Remove the TRUE/FALSE variables the imputation function adds to the datasets
  train.data <- subset(train.data, select = -c(((ncol(train.data)/2)+1):ncol(train.data)))
  test.data <- subset(test.data, select = -c(((ncol(test.data)/2)+1):ncol(test.data)))
  fulldata <- subset(fulldata, select = -c(((ncol(fulldata)/2)+1):ncol(fulldata)))
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

# Stability analysis
# Initialise vectors
varcount <- vector("numeric", length=length(varratios[[1]]))
varnotcount <- vector("numeric", length=length(varratios[[1]]))
variablesinmodel <- vector("numeric", length=repeats*outsidefolds)

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
    }
  }
}

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

# LOOK AT MINIMUM REPORTING FOR STABILITY ANALYSES FROM STATS RECOMMENDATIONS

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
