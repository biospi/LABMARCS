# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 31/01/2021
# Author: Louis MacGregor

# Input variables (if transformed into a function)
# Set LookForUpdates to 1 if you want to search for and install newest 
#R version
LookForUpdates <- 0
# Set InstallPackAges to 1 if you want to install the packages listed 
#below all at once
InstallPackages <- 0
# Load libraries
LoadLibraries <- 1
  
# outcomeselection (1) all severe outcomes (2) ICU admission (3) death
outcomeselection <- 3
  
# Choose to include Firth's bias in the models (constrains parameters 
#from >> numbers) #currently breaks - debug
IncludeFirthBias <- 0
  
# Use knn imputation==1, MICE==2, None==0 If none, complete case analysis 
#is used
ImputationAllowed <- 1
  
# Model exclusively looking at comordbities (looks only at 1 hospital)
# Loses 50% of data but appears to be a better model
Comorbidities <- 0
  
  
# MasterAnalysisOn==1 prevents the dateRange and readingwanted variables from
# being overwritten, but the option to turn this off and run this script 
# without first running MasterAnalysis.R is available by setting 
# MasterAnalysisOn==0 and then manually selecting the dateRange and 
# readingwanted variables.
MasterAnalysisOn <- 0
  if (MasterAnalysisOn == 0) {
    dateRange <- 1 # 1, 3 or 5 days
    readingwanted <- 0 # 0-worst, 1-first, 2-mean
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

setwd("Z:/brian/data/")

readingwanted_ls=c('worst','first','mean')

#dateRange<-1 #1,3,5
#readingwanted<-2 #0,1,2

#read in data processed from one_row_per_id_all.R
fulldata <- read.csv(file = paste('totalBinary',as.character(dateRange),
  as.character(readingwanted_ls[readingwanted+1]), '.csv',sep=''),na.strings= c(""))

#remove X column (not used)
fulldata <- subset(fulldata, select = -c(X))

# Initial inspection of the data
head(fulldata)
summary(fulldata)
write.table(colnames(fulldata),file='fulldata_column_names.txt',row.names = FALSE)

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

# Remove superfluous data columns 
fulldata <- subset(fulldata, select = -c(died,went_to_icu,severeOutcome,N,coinfection,
                                         admissionDate,dischargeDate,ITU_Start,
                                         ITU_End,deathDate,ID,Site,positiveDate))

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
# Drop if age is missing
fulldata <- fulldata[!is.na(fulldata$Age), ]


# All others ()
# EXCLUDE CONTINUOUS VARIABLES
for (i in 3:(length(fulldata) - 1)) {
  fulldata[,i] = as.factor(fulldata[,i])
}

# BETTER PRACTICE IS TO SET CATEGORY LEVELS SO THAT ORs ARE >1
# I.E. REFERENCE CATAGORY IS THE LOWEST RISK.
#for (i in 3:(lehead(fulldata)par(mfrow=c(1,1))
pdf(paste(getwd(),'/output/age_histogram.pdf',sep = ''))
nbreaks <- pretty(range(fulldata$Age),n = 20)
xlab = paste('Age, bin width=', as.character(nbreaks[2] - nbreaks[1]))
hist(fulldata$Age,main = "Distribution of Patient Ages",xlab = xlab, breaks = nbreaks)
dev.off()

# Display summary of the data in this format
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
fulldata$Age <- log10(max(fulldata$Age + 1) - fulldata$Age)
  
# Check new skewness values
skewness(fulldata$Age, na.rm = TRUE) # nearly approximately symmetric
  
# Plot transformed variables
# Plot histograms of continuous variables to examine distributions
nbreaks <- pretty(range(fulldata$Age),n = 20)
par(mfrow = c(1,1))
xlab <- paste('Age, bin width=', as.character(nbreaks[2] - nbreaks[1]))
pdf(paste(getwd(),'/output/age_histogram_transformed.pdf',sep = ''))
hist(fulldata$Age,main = "Distribution of Patient Ages",xlab = xlab, breaks = nbreaks)
dev.off()
  
# Scale continuous variables to have mean 0 and sd of 1
fulldata$Age <- scale(fulldata$Age)[, 1]
  
# Check the scaling has worked as expected
mean(fulldata$Age)
sd(fulldata$Age)
  
# Plot regularized variables
# Plot histograms of continuous variables to examine distributions
nbreaks <- pretty(range(fulldata$Age),n = 20)
par(mfrow = c(1,1))
xlab <- paste('Age, bin width=', as.character(nbreaks[2] - nbreaks[1]))
pdf(paste(getwd(),'/output/age_histogram_regularized.pdf',sep= '' ))
hist(fulldata$Age,main = "Distribution of Patient Ages",xlab = xlab, breaks = nbreaks)
dev.off()
  
# Plot boxplots to look for outliers
par(mfrow = c(1,1))
pdf(paste(getwd(),'/output/age_boxplot_outliers.pdf',sep = ''))
boxplot(fulldata$Age,main = "Age", ylab = "Years")
dev.off()
  
# Examine continuous variables before winsorisation
summary(fulldata$Age)

# Winsorise the data at the 1st and 99% percentile (replace extreme values)
fulldata$Age <- Winsorize(fulldata$Age, minval = NULL, maxval = NULL,
                             probs = c(0.01, 0.99), na.rm = FALSE, type = 7)
  
# Examine continuous variables after winsorisation
summary(fulldata$Age)

# CURRENTLY NOT RELEVANT AS ONLY ONE CONTINUOUS VARIABLE
# Summary/visualisation of pairwise correlation between numerical variables
# Guide: High degree: between ? 0.50 and ? 1 - strong correlation
#        Moderate degree: between ? 0.30 and ? 0.49 - medium correlation
#        Low degree: below ? 0.29 - small correlation.
#  par(mfrow=c(1,1))
#  cor(fulldata[,c(1,4,5,8)],method = "spearman") #index the continuous vars
#  cor(fulldata[,c(1,4,5,8)],method = "kendall" )
#  cor(fulldata[,c(1,4,5,8)],method = "pearson" )
#  corrplot(cor(fulldata[,c(1,4,5,8)]), method="circle")
  
# ASSIGN TRAINING AND TEST DATA SETS ----------------------------------

# Ensure that factors are specified for values to be imputed (i.e. NA is not a factor)
factor_levels = c("Normal","Abnormal")
categorical_levels = c("Normal","Mild","Moderate","Severe","Test not taken")
  
fulldata <- fulldata %>%
    mutate(
      eGFR_val = factor(eGFR_val,levels = factor_levels, exclude = NA),
      WCC = factor(WCC,levels = categorical_levels, exclude = NA),
      Neutrophils = factor(Neutrophils,levels = categorical_levels, exclude = NA),
      Lymphocytes = factor(Lymphocytes,levels = categorical_levels, exclude = NA),
      NLR_val = factor(NLR_val,levels = categorical_levels, exclude = NA),
      PLT_val = factor(PLT_val,levels = categorical_levels, exclude = NA),
      CRP_val = factor(CRP_val,levels = factor_levels, exclude = NA),
      APTT_val = factor(APTT_val, levels = categorical_levels, exclude = NA),
      PT_val = factor(PT_val, levels = categorical_levels, exclude = NA),
      FER_val = factor(FER_val, levels = categorical_levels, exclude = NA),
      fib_val = factor(fib_val, levels = categorical_levels, exclude = NA),
      HB_val = factor(HB_val, levels = categorical_levels, exclude = NA),
      LDH_val = factor(LDH_val, levels = categorical_levels, exclude = NA)
    )

# Ensure that unused levels are dropped
fulldata <- droplevels(fulldata)
  
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

if (ImputationAllowed == 0) {
  # Keep only complete cases
  train.data <- train.data[complete.cases(train.data), ]
  test.data <- train.data[complete.cases(test.data), ]
} else {
  # Convert string "NA" to actual NA
  is.na(fulldata)
  # Impute missing values using MICE (Multiple Imputation by Chained Equations)
  # Only looking at eGFR,WCC,Neutrophils,Lymphocytes,NLR,Hb,Platelets (PLT),CRP for time being
  pdf(paste(getwd(),'/output/mice_plot_imputation.pdf',sep = ''))
  
  mice_plot <- aggr(fulldata, col = c("navyblue","yellow"),
                    numbers = TRUE, sortVars = TRUE,
                    labels = names(fulldata), cex.axis = 0.7,
                    gap = 3, ylab = c("Missing data","Pattern"))
  dev.off()
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
  
  imputed_train <- mice(train.data, m = 5, seed = 107)
  imputed_test <- mice(test.data, m = 5, seed = 107)
  imputed_full <- mice(fulldata, m = 5, seed = 107)
  
  # Just use one of the imputed datasets (3rd out of 5)
  # IDEALLY we should use all 5 and pool the output. Uncertain on how to do this!
  train.data <- complete(imputed_train, 3)
  test.data <- complete(imputed_test, 3)
  fulldata <- complete(imputed_full, 3)
}

summary(fulldata)

#----------------------------------------------------------------------
# RUN SIMPLE PRELIMINARY MODELS

# Initialise model outputs into lists
modelunivar <- vector(mode = "list", length = (length(fulldata) - 1))
modeldemo <- vector(mode = "list", length = (length(fulldata) - 3))

#Original code was omitting one variable by mistake and R console was not printing 
#to screen without explicit print()
family_ls = c('glm.fit','firthglm.fit')

#build individual models for each variable we have recorded
sink(paste(getwd(),'/output/Simple_Preliminary_Models.txt',sep = '')) #open text file to review later
for (i in 1:(dim(fulldata)[2])) {
  modelunivar[[i]] <- glm(fulldata$outcome ~ fulldata[, i],
                          family = "binomial",
                          method = family_ls[IncludeFirthBias + 1])
  print('--------------------------------------------------------------------')
  print(paste('Single Variable:', as.character(names(fulldata)[i]), 'predicting outcome'))
  print('Model Summary')
  print(summary(modelunivar[[i]]))
  print('Odds Ratios')
  print(exp(modelunivar[[i]]$coefficients))
  print('Odds Ratios CIs')
  print(exp(confint(modelunivar[[i]])))
}

#Alternate model always including age & gender
for (i in 1:(dim(fulldata)[2])) {
  modeldemo[[i]] <- glm(fulldata$outcome ~ fulldata[, i] + fulldata$Age + fulldata$Gender,
      family = "binomial",
      method = family_ls[IncludeFirthBias + 1])
  print('--------------------------------------------------------------------')
  print(paste('Age & Gender, plus', as.character(names(fulldata)[i]), 'to predict outcome'))
  print('Model Summary')
  print(summary(modeldemo[[i]]))
  print('Odds Ratios')
  print(exp(modeldemo[[i]]$coefficients))
  print('Odds Ratios CIs')
  print(exp(confint(modeldemo[[i]])))
}
sink()

#PLACE HOLDER CODE TO BE ADAPTED IF WE ADD CO-MORBIDITIES
#modelcomorb[i] <- glm(fulldata$outcome ~ chol + Age + sex, thal,
#                      family = "binomial", method = family_ls[IncludeFirthBias + 1])
#print(paste('Comorbid, plus', as.character(names(fulldata)[i]), 'to predict outcome'))
#print(summary(modelcomorb[[i]]))
#print(exp(modelcomorb[[i]]$coefficients[1]))
#print(exp(confint(modelcomorb[[i]])))

#----------------------------------------------------------------------
# RUN THE MAIN MODELS -------------------------------------------------

# Model 1 GLM & # Model 2 STEP_AIC

for (i in c(1,2)) {
  # Run model with/without Firth's bias
  if (IncludeFirthBias == 0) {
    StatModel_Full <- glm(outcome ~.,data = train.data, family = "binomial")
    # Initial model using all input parameters without Firth's bias
    StatModel <- StatModel_Full #rename to generic for loop
    if (i == 2) {
      StatModel_StepAIC <- stepAIC(StatModel_Full,trace = TRUE)
      StatModel <- StatModel_StepAIC}
  } else {
    # Method below utilises the Firth's bias approach
    # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
    model <- glm(outcome ~ .,
                    data = train.data, family = "binomial", method = "firthglm.fit")
  }
  
  
  sink(paste(getwd(),'Model_',as.character(i),'_summary.txt',sep = '')) #open text file to review later
  # Summarize the final selected model
  print('Model Summary')
  print(summary(StatModel))
  
  # Return p-values (unadjusted)
  print('Unadjusted p-values')
  print(coef(summary(StatModel))[,4])
  
  # Adjust the p-values using benjamini hochberg method
  adjustedp <- p.adjust(coef(summary(StatModel))[,4], method = "fdr",
                         n = length(coef(summary(StatModel))[,4]))
  # Return p-values (adjusted)
  print('B-H corrected p-values')
  print(adjustedp)
  
  # Examine odds ratios and 95% CIs
  print('Var/Covar Matrix')
  print(vcov(StatModel))
  print('Odds Ratios')
  print(exp(StatModel$coefficients))
  print('Odds Ratios CIs')
  print(exp(confint.default(StatModel)))
  
  # Check for multicollinearity in the model variables (any >5 are problematic)
  #car::vif(model1)
  # All are <5 and so we can be reasonably sure that we have met the 
  # assumption of collinearity
  
  # Model 1 predictions on test set (prob of >0.5 accepted as positive)
  probabilities <- predict(object = StatModel, test.data, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  # Model accuracy
  print('Model Accuracy')
  print(mean(predicted.classes == test.data$outcome))
  
  # Sensivity and specificity measures
  conf_matrix <- table(predicted.classes,test.data$outcome)
  colnames(conf_matrix) = c(0,1)

  # find AUC and plot ROC curve
  pdf(paste(getwd(),'/output/Model_',as.character(i),'_roc.pdf',sep = ''))
  g <- roc(outcome ~ probabilities, data = test.data)
  plot(g)
  dev.off()
  
  print('AUC')
  print(auc(g))
  print('Sensitivity')
  print(as.numeric(sensitivity(conf_matrix)['.estimate']))
  print('Specificity')
  print(as.numeric(specificity(conf_matrix)['.estimate']))
  print('Brier Score')
  print(BrierScore(StatModel))
  sink()
  
}

#--Model 3-------------------------------
# First we perform nested cross-validation to report on the generalised performance
# Set number of outer fold repeats, outer folds and inner folds
repeats <- 10 #50
outsidefolds <- 5 #20
insidefolds <- 5
n_models <- n_models

# Allow randomness to resume
set.seed(NULL)

# Initialise outcome measures
lambda.store <- vector("numeric", length = n_models)
auc <- vector("numeric", length = n_models)
brier <- vector("numeric", length = n_models)
includedvars <- list(length = n_models)
varnames <- vector(mode = "list", length = n_models)
varratios <- vector(mode = "list", length = n_models)
roccurve <- vector(mode = "list", length = n_models)

for (j in 1:repeats) {
  # Randomly shuffle the data
  fulldata <- fulldata[sample(nrow(fulldata)),]
  
  # Create n equally sized outer folds
  data.outerfolds <- cut(seq(1,nrow(fulldata)),breaks = outsidefolds,labels = FALSE)
  
  for (i in 1:outsidefolds) {
    
    #Segment data by fold using the which() function 
    testIndexes <- which(data.outerfolds == i,arr.ind = TRUE)
    test.data <- fulldata[testIndexes, ]
    train.data <- fulldata[-testIndexes, ]
    x <- model.matrix(outcome~., train.data)[,-1]
    
    # Determine lambda.1se parameter over inner folds for the training data
    cv.lasso <- cv.glmnet(x,train.data$outcome, alpha = 1, data = train.data,  
                          nfolds = insidefolds,
                          family = "binomial")
    lambda.store[outsidefolds*(j - 1) + i] <- cv.lasso$lambda.1se
    StatModel3 <- glmnet(x, train.data$outcome, alpha = 1, family = "binomial",
                     lambda = cv.lasso$lambda.1se)
    
    # Test the best predicted lambda on the remaining data in the outer fold
    x.test <- model.matrix(outcome ~., test.data)[,-1]
    probabilities <- StatModel3 %>% predict(newx = x.test, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
    
    # Model accuracy
    mean(predicted.classes == test.data$outcome)
    
    # Sensitivity and specificity measures
    conf_matrix <- table(predicted.classes,test.data$outcome)
    colnames(conf_matrix) = c(0,1)
    #sens<-sensitivity(conf_matrix)
    #spec<-specificity(conf_matrix)
    
    # Plot ROC curve
    roccurve[[outsidefolds*(j - 1) + i]] <- roc(outcome ~ c(probabilities), 
                                            data = test.data)
    auc[outsidefolds*(j - 1) + i] <- auc(roccurve[[outsidefolds*(j - 1) + i]])
    #plot(roccurve)
    
    # Brier score
    f_t <- probabilities
    o_t <- test.data$outcome
    brier[outsidefolds*(j - 1) + i] <- mean(((f_t) - o_t)^2)
    
    modelcoefs <- exp(coef(StatModel3))
    varnames[[outsidefolds*(j - 1) + i]] <- modelcoefs@Dimnames[[1]]
    varratios[[outsidefolds*(j - 1) + i]] <- modelcoefs@x
    
  }
}

sink(paste(getwd(),'/output/Model_3_glm_lasso_cross-validation.txt',sep = '')) 
print("Mean AUC")
print(mean(auc))
print('Quantile')
print(quantile(auc, c(.025, .50, .975)))
print('t-test AUC')
print(t.test(auc))
print('t-test CIs')
print(t.test(auc)$"conf.int")
print('Mean Brier')
mean(brier)
print('Quantile Brier')
quantile(brier, c(.025, .50, .975))
print('t-test Brier Score')
t.test(brier)
print('t-test Brier CIs')
t.test(brier)$"conf.int"
print('Mean Lambda Score')
mean(lambda.store)
print('Quantile Lambda Score')
quantile(lambda.store, c(.025, .50, .975))
print('t-test Lambda Score')
t.test(lambda.store)
print('t-test Lambda Score CIs')
t.test(lambda.store)$"conf.int"
sink()

pdf(paste(getwd(),'/output/Model_3_AUC_Histogram.pdf',sep = ''))
hist(auc,plot = TRUE, breaks = 25, xlab = 'Area Under the Curve (AUC)',
     main = 'Distibution of AUC Scores')
#abline(v=mean(auc), col="red")
abline(v = median(auc), col = "blue")
dev.off()


pdf(paste(getwd(),'/output/Model_3_BrierScore_Histogram.pdf',sep = ''))
hist(brier,plot = TRUE)
dev.off()

pdf(paste(getwd(),'/output/Model_3_Lamda_Histogram.pdf',sep = ''))
hist(lambda.store,plot = TRUE)
dev.off()

# plot the ROC curves for all models to show uncertainty
pdf(paste(getwd(),'/output/Model_3_ROCs_for_',as.character(n_models),'_models.pdf',sep = ''))
ggroc(roccurve, legacy.axes = T) +
  geom_abline(slope = 1 ,intercept = 0) + # add identity line
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 18, face = 'bold'),
    axis.title.y = element_text(size = 18, face = 'bold'),
    panel.border = element_rect(size = 2, fill = NA), 
    axis.text.x = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(size = 14, face = 'bold')) +
  xlab('1 - Specificity (FP)') +
  ylab('Sensitivity (TP)') +
  scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) + 
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25))
dev.off()


# Stability analysis--------------------------------------------------------

if (Comorbidities == 0) {
  # Initialize vectors and arrays
  # Count of the number of of times each variable is used in a model
  varcount <- vector("numeric", length = length(varratios[[1]]))
  # List of the total number of variables in each model
  variablesinmodel <- vector("numeric", length = n_models)
  # A matrix with model index versus inclusion of each variable in that model
  modelvarmatrix <- array(0L, c(n_models, length(varratios[[1]])))
  # List of the value of the 'events per variable' in each model
  EPV <- vector("numeric", length = n_models)
  # List of the frequency of pairwise inclusion of each variable over all models
  freqpairs <- array(0L, c(length(varratios[[1]]), length(varratios[[1]])))
  
  # cycle through each model and each variable counting when variable is used
  # i.e. variable value from LASSO =\= 1.
  # Also count the number of variables used (=\= 1) in each model in total
  for (i in 1:n_models) {
    varratios[[i]] <- round(varratios[[i]], digits = 3)
  }

  for (i in 1:(n_models)) {
    for (j in 1:length(varratios[[1]])) {
      if (varratios[[i]][j] < 0.999 | varratios[[i]][j] > 1.001) {
        varcount[j] <- varcount[j] + 1
        variablesinmodel[i] <- variablesinmodel[i] + 1
        modelvarmatrix[i,j] <- 1
        }
    }
    EPV[i] <- eventsnumber/variablesinmodel[i]
  }

  sink(paste(getwd(),'/output/Model3_Stability_Analysis.txt',sep = ''))
  # (i) Range and mean of the events per variable (EPV)
  #The EPV ratio, computed from the number of candidate variables,
  #accompanied by a warning note if a user attempts to invoke variable 
  #selection if EPV is lower than 25
  
  print('Events per variable min')
  print(min(EPV)) # If <25 then need a quick warning and may need to state the EPV
  
  print('Events per variable max')
  print(max(EPV))
  
  print('Events per variable mean')
  print(mean(EPV))
  
  # (ii) Global model standard errors, variables and coefficients
  #The global model including all candidate variables with regression 
  #coefficients and standard errors. (See also REMARK guidelines, 
  #item 17, Altman et al., 2012).
  
  # Summary
  print('Full Model Summary')
  print(summary(StatModel_Full))
  
  # Examine odds ratios and 95% CIs
  print('Full Model Coefficients Log-Odds')
  print(exp(StatModel_Full$coefficients))
  print('Full Model Coefficients CIs')
  print(exp(confint.default(StatModel_Full)))

  # (iii) Inclusion frequencies for each variable
  #Bootstrap inclusion frequencies for each candidate variable 
  #(not only the selected ones)
  
  # Make histogram showing the number of variables included in each model
  pdf(paste(getwd(),'/output/VariablesPerModel_Histogram.pdf',sep = ''))
  hist(variablesinmodel, 
       main = paste('Distribution of Number of variables selected over', 
                    as.character(n_models),'models',sep = ' '),
       xlab = "Number of variables", breaks = (0:length(varcount)))
  dev.off()
  
  # Make a bar plot of the included variables
  pdf(file = paste(getwd(),'/output/ModelStability_VariableFrequency.pdf',sep = ''),
      width = 12, height = 6)
  par(mar = c(10,10,10,10),mai = c(2,1,1,1))
  barplotstab <- barplot(varcount[2:length(varcount)]/(n_models)*100,
          main = 'Distribution of Variables Selected via Lasso', 
          ylab = "Frequency (%)", names.arg = varnames[[1]][2:length(varcount)],
          las = 2, cex.names = 0.6, horiz = FALSE )
  abline(h = 50)
  abline(h = 20, col = "red")
  dev.off()

  # (iv) The RMSD of the bootstrapped regression coefficients compared to the 
  #regression coefficients of the global model is given We propose that 
  #software output should at least contain an "RMSD ratio," which is the 
  #RMSD divided by the standard error of beta
  
  # (v) Relative bias conditional on selection, computed as and BIF denoting 
  #the mean bootstrapped estimate, the global model estimate, and the
  #bootstrap inclusion frequency of an IV,respectively

  # (vi) Most commonly chosen models
  #The bootstrap model selection frequencies for the finally selected model 
  #and the most often selected models, for example the list of models with 
  #cumulative frequency of at least 80%, or the list of 20 top-ranked models, 
  #whichever list is shorter.
  
  # keep only unique rows (models) in a new matrix and make a copy for manipulating
  modelvarmatrixunique <- unique.matrix(modelvarmatrix)
  tempmatrix <- unique.matrix(modelvarmatrix)
  
  # Count of the frequency of each model used
  modelcount <- vector("numeric", length = nrow(modelvarmatrixunique))
  for (i in 1:nrow(modelvarmatrixunique)) {
    for (j in 1:nrow(modelvarmatrix)) {
      if (all(modelvarmatrixunique[i,] == modelvarmatrix[j,])) {
        modelcount[i] <- modelcount[i] + 1
      }
    }
  }

  # Work out the proportion of times each model is chosen overall
  modelprop <- (modelcount/(n_models))*100
  # sort and keep the index value for the sorting procedure
  modelprop <- sort(modelprop,decreasing = TRUE, index.return = TRUE)
  
  # apply sorting procedure to the matrix to match up the proportions
  for (i in 1:length(modelprop)) {
    tempmatrix[i,] = modelvarmatrixunique[modelprop$ix[i],]
  }
  modelvarmatrixunique <- tempmatrix
  
  # determine if need first 20 or 80% cumulative total - 62%
  # as first 20 results does not comprise 80% of the cumulative models, 
  #we should present only the first 20 models
  print('Sum-ModelProp % first 20 most common')
  print(sum(modelprop$x[1:20])) 
  sink()
  
  # add in cumulative totals
  for (i in 1:20) {
    modelprop$cummulative[i] <- sum(modelprop$x[1:i])
  }

  # List of the models of interest, with proportion and cumulative proportions
  ModelList <- array(0L, c(20,(ncol(modelvarmatrixunique) + 2)))
  ModelList[1:20,1:ncol(modelvarmatrixunique)] <- modelvarmatrixunique[1:20,]
  ModelList[1:20,(ncol(modelvarmatrixunique) + 1)] <- modelprop$x[1:20]
  ModelList[1:20,(ncol(modelvarmatrixunique) + 2)] <- modelprop$cummulative[1:20]
  colnames(ModelList) <- c(varnames[[1]],"Prop", "Cummulative") 
  
  # (vii) Pairwise variable frequencies
  #A matrix with pairwise inclusion frequencies, which are suitably summarized,
  #for example as odds ratios obtained by log-linear analysis (see Royston & 
  #Sauerbrei, 2003) or as "significant" pairwise over- or underselection
  
  for (i in 1:(n_models)) {
    for (j in 1:length(varratios[[1]])) {
      for (k in 1:length(varratios[[1]])) {
        if (modelvarmatrix[i,j] == 1 & modelvarmatrix[i,k] == 1) {
          freqpairs[j,k] <- freqpairs[j,k] + 1
        }
      }
    }
  }
  
  colnames(freqpairs) <- c(varnames[[1]]) 
  rownames(freqpairs) <- c(varnames[[1]])
  pdf(paste(getwd(),'/output/variable_selection_correlation_stability.pdf',sep = ''))
  # correlation plot CURRENTLY MESSY AND MORE ROBUST MEASURES ARE NEEDED
  corrplot(cor(freqpairs), method = "number",number.cex = 0.2,tl.cex = 0.35)
  dev.off()
}

STOP
# LOOK AT MINIMUM REPORTING FOR STABILITY ANALYSES FROM STATS RECOMMENDATIONS
#---------------------------------------------------------------------------

# Create the final model over the whole dataset, with the expected performance 
#from nested CV
x <- model.matrix(outcome~., fulldata)[,-1]
cv.lasso <- cv.glmnet(x,fulldata$outcome, alpha = 1, data= fulldata,  
                      nfolds = insidefolds, family = "binomial")

pdf(paste(getwd(),'/output/StatModel_FullData_Lasso.pdf',sep = ''))
plot(cv.lasso)
dev.off()

StatModel_FullLasso <- glmnet(x, fulldata$outcome, alpha = 1, family = "binomial",
                 lambda = cv.lasso$lambda.1se)

sink(paste(getwd(),'/output/StatModel_FullData_Lasso_Summary.txt',sep = ''))

# Display regression coefficients
print('Coefficients')
print(coef(StatModel_FullLasso))

# Examine odds ratios
print('Odds Ratios')
print(exp(coef(StatModel_FullLasso)))

# Test the best predicted lambda on the remaining data in the outer fold
x.test <- model.matrix(outcome ~., test.data)[,-1]
probabilities <- StatModel_FullLasso %>% predict(newx = x.test, type="response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
print('Accuracy')
print(mean(predicted.classes==test.data$outcome))

# Plot ROC curve and find AUC
roccurve3 <- roc(outcome ~ c(probabilities), data = test.data)
auc<- auc(roccurve3)
print('AUC')
print(auc)
sink()

pdf(paste(getwd(),'/output/StatModel_FullData_Lasso_ROC.pdf', sep =''))
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
dev.off()

# Brier score
f_t <- probabilities
o_t <- test.data$outcome
brier<- mean(((f_t) - o_t)^2)
# ---------------------------------------------------------------------

