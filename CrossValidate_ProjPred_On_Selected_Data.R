# perform nested cross-validation to report on the  models


mnum = 'PP'
print("Run proj pred bayes Model Cross Validation...")
model_desc_str = paste(cv_desc,mnum,'_R',as.character(repeats),'_OF', 
                       as.character(outsidefolds), '_IF', 
                       as.character(insidefolds), '_', sep = '')
  
# Initialize outcome measures
lambda.store <- vector("numeric", length = n_models)
auc <- vector("numeric", length = n_models)
brier <- vector("numeric", length = n_models)
sensitivity_store <- vector(mode = "numeric", length = n_models)
specificity_store <- vector(mode = "numeric", length = n_models)

specificity90_store <- list()
specificity95_store <- list()

rocposcases <- vector(mode = "numeric", length = n_models)
includedvars <- list(length = n_models)
roccurve <- vector(mode = "list", length = n_models)
varnames <- vector(mode = "list", length = n_models)
calibration_df = data.frame()
calibration_n_df = data.frame()


#initiate stat model list data structure (don't need to reset when test generalisation)
if (!generalise_flag) {
  StatModel_ls <- vector(mode = "list", length = n_models)
  vs_ls <- vector(mode = "list", length = n_models)
}

#Technically the dimension should be columns -1 since we don't include outcome
#in predictions, but intercept is added in list of coefficients
varratios <- as_tibble(matrix(99,nrow = n_models,ncol = dim(crossval.train.data)[2] ))
colnames(varratios) <- names(crossval.train.data)
varratios <- rename(varratios, '(Intercept)' = 'outcome' )

#store the p-values as glm   
varsig <- varratios

#Loop for cross validation
#for (j in 1:repeats) {
j = 0
repeat_flag = FALSE

while (j <= repeats) {
  
  j = j + 1
  
  print(paste('repeat:',j))
  
  # Randomly shuffle the training data
  crossval.train.data <- crossval.train.data[sample(nrow(crossval.train.data)),]
  
  # Create n equally sized outer folds
  data.outerfolds <- cut(seq(1,nrow(crossval.train.data)),breaks = outsidefolds,
                         labels = FALSE)

  #Make sure each split has pos/neg examples, else resample
  testSmp = TRUE 
  while (testSmp) {
    for (i in 1:outsidefolds) {
      
      #Segment data by fold using the which() function 
      testIndexes <- which(data.outerfolds == i,arr.ind = TRUE)
      #cross validation only on train data for outerfolds
      of_crossval.test.data <- crossval.train.data[testIndexes, ]
      
      #currently only require one true sample, could increase if needed
      if (sum(of_crossval.test.data$outcome) == 0) {
        print('Resample')
        crossval.train.data <- crossval.train.data[sample(nrow(crossval.train.data)),]
        
        # Create n equally sized outer folds
        data.outerfolds <- cut(seq(1,nrow(crossval.train.data)),breaks = outsidefolds,
                               labels = FALSE)
        
        break
      } else if (i == outsidefolds) { #reach the end successfully
        testSmp = FALSE
      }
    }
  }
  
  
  for (i in 1:outsidefolds) {
    
    ij_idx = outsidefolds*(j - 1) + i
    
    print(paste('repeat:',j, 'fold:', i))
    
    #Segment data by fold using the which() function
    #this selects a portion of data as function of number of outerfolds, e.g.
    #4 outer folds means 1/4 or 25% of data is used for test, 75% for train
    testIndexes <- which(data.outerfolds == i,arr.ind = TRUE)
    #cross validation only on train data for outerfolds
    of_crossval.train.data <- crossval.train.data[-testIndexes, ]
    
    #note the generalise data must be run after the initial models have been setup
    #else the StatModel_ls will be empty and generalisaton will cause an error
    if(generalise_flag){
      #test CV on dataset to generalise to (assigned in LABMARCS_LogisticRegression.m)
      of_crossval.test.data <- crossval.test.data
      test_backup = of_crossval.test.data
      #ProjectivePrediction_Compuite.R does some data type correction as BRMS/ProjPred are picky
      names(test_backup) = str_replace_all(names(test_backup),"_","")
      
      #recast as numeric?
      for (ii in c(1:dim(test_backup)[2])) { #skip #2 age
        test_backup[,ii] <- as.numeric(test_backup[,ii])
      }
      
      StatModel <- StatModel_ls[[ij_idx]]
      vs <- vs_ls[[ij_idx]]
      refmodel <- get_refmodel(StatModel)
      
    } else{ #test on the portion left out for CV
      of_crossval.test.data <- crossval.train.data[testIndexes, ]  
      
      #x <- model.matrix(outcome~., of_crossval.train.data)[,-1]
      #idx <- 1:(dim(of_crossval.train.data)[2] - 1) #omit outcome column
      cv_style = 'Naive' # 'KFold' or 'LOO'
      vs_flag = FALSE
      use_precomputed = 0
      
      #note projpred needs numeric data so tr_backup and test_backup
      #are transformed via ProjectivePrediction_Compute.R, any further tests
      #need to use _backup data and not the of_crossval.
      tr_backup = of_crossval.train.data
      test_backup = of_crossval.test.data
      
      #this runs brmfit and also variable selection 
      result = tryCatch({
        source('ProjectivePrediction_Compute.R')
      }, error = function(error_condition) {
        return(FALSE)
      })
      
      if (is.logical(result)) {
        repeat_flag = TRUE
        print('Error, Reset j')
      }
      

      if (repeat_flag) {
        print('1.Repeat j as projpred didnot converge!')
        j = j - 1
        repeat_flag = FALSE
        break #shoold break > for (i in 1:outsidefolds) {        
      }

      #save models for test with generalisation set
      StatModel_ls[[ij_idx]] <- brmfit
      vs_ls[[ij_idx]] <- vs
    }
    
    
    #We make predictions with the projected submodels. For point estimates we can use method 
    #proj_predict. Test inputs can be provided using the keyword newdata. 
    #It also the test targets ynew are provided in newdata, then the function evaluates
    #the log predictive density at these points. For instance, the following computes 
    #the mean of the predictive distribution and evaluates the log density at the training 
    #points using the most relevant variables.
    nv <- length(solterms_ls)
    pp_str = paste('ProjPred_V',nv, '_', sep = '')
    proj <- project(vs, solution_terms = solterms_ls,ns = 2000)
    

    print('pre-projection')
    proj <- project(vs, nterms = nt_max,ns = 2000)
    
    pred = proj_predict(proj, newdata = test_backup)
    #projpred does N draws from the model, these are averaged for the overall response
    probabilities_of <- colMeans(pred) 

    # Test the best predicted lambda on the remaining data in the outer fold
    predicted.classes_of <- as.logical(probabilities_of >= 0.5) #prediction from model
    
    #model calibration bin each set of probabilites and then count the percent of our
    #patients that fall in that category
    cal_interval <- 0.1
    zz = 0
    for (kk in seq(0,1 - cal_interval,cal_interval)) {
      
      zz = zz + 1
      #which probabilites are in this interval
      if (kk == 1) { kk = 1.1} #adjust for last bin to include 1
      
      #find which patients are in this bin
      tmp_prob_bin_idx <- (probabilities_of >= kk) & (probabilities_of < kk+cal_interval)
      
      #of the patients in the bin what % had the outcome
      cal_p = sum(of_crossval.test.data$outcome[tmp_prob_bin_idx])/sum(tmp_prob_bin_idx)
      
      #save number of interval and %cases in this bin
      #calibration_df[zz,1] = kk
      #what is the % true outcome for this particular bin
      calibration_df[ij_idx,zz] = cal_p
      calibration_n_df[ij_idx,zz] = sum(tmp_prob_bin_idx)
      
    }
    
    
    # Model accuracy
    out_acc <- mean(predicted.classes_of == of_crossval.test.data$outcome)
    
    # Sensitivity and specificity measures
    conf_matrix_of <- table(predicted.classes_of, of_crossval.test.data$outcome)
    
    tab <- matrix(c(0,0,0,0), ncol = 2, byrow = TRUE)
    colnames(tab) <- c('0','1')
    rownames(tab) <- c('0','1')
    tab <- as.table(tab)
    
    #sometimes the conf matrix is incomplete copy what we do have to tab
    tryCatch(tab[1,1] <- conf_matrix_of[1,'FALSE'], error = function(e) {tab[1,1] <- 0 } )
    tryCatch(tab[2,1] <- conf_matrix_of[2,'FALSE'], error = function(e) {tab[2,1] <- 0 } )
    tryCatch(tab[1,2] <- conf_matrix_of[1,'TRUE'], error = function(e) {tab[1,2] <- 0 } )
    tryCatch(tab[2,2] <- conf_matrix_of[2,'TRUE'], error = function(e) {tab[2,2] <- 0 } )
    
    sensitivity_store[ij_idx] <- as.numeric(sensitivity(tab)['.estimate'])
    specificity_store[ij_idx] <- as.numeric(specificity(tab)['.estimate'])
    
    # save ROC curve
    roccurve[[ij_idx]] <- roc(outcome ~ c(probabilities_of), 
                                                data = of_crossval.test.data)
    rocposcases[[ij_idx]] <- sum(of_crossval.test.data['outcome'])

    #we wnat to save specificity at 90 & 95% sensitivity - the ROC curve stores sensitivities
    #so we need to find the index matching the above, senistivity is stored decreasing from one
    #so if we subtract our threshold sensitivity we can take the first minimum 
    s_thresh = 0.90
    pos_min = roccurve[[ij_idx]]$sensitivities[ (roccurve[[ij_idx]]$sensitivities - s_thresh) > 0 ] - s_thresh
    #we need the first threhold that is at least 90/95% sensitivity (but also need to exclude
    #values just below threshold that might be a smaller min)
    pos_min_rev_idx = seq(length(pos_min),1) #reverse index
    sens_idx_90 = pos_min_rev_idx[which.min(rev(pos_min))]
    
    s_thresh = 0.95
    pos_min = roccurve[[ij_idx]]$sensitivities[ (roccurve[[ij_idx]]$sensitivities - s_thresh) > 0 ] - s_thresh
    pos_min_rev_idx = seq(length(pos_min),1) #reverse index
    sens_idx_95 = pos_min_rev_idx[which.min(rev(pos_min))]
    
    #now we can assign values for specifity at 90 & 95% sensitivity
    #after finishing the loops these lists need to be converted to matrices
    specificity90_store[[ij_idx]] <- c(roccurve[[ij_idx]]$thresholds[sens_idx_90], 
                                       roccurve[[ij_idx]]$sensitivities[sens_idx_90], 
                                       roccurve[[ij_idx]]$specificities[sens_idx_90])
    
    specificity95_store[[ij_idx]] <- c(roccurve[[ij_idx]]$thresholds[sens_idx_95], 
                                       roccurve[[ij_idx]]$sensitivities[sens_idx_95], 
                                       roccurve[[ij_idx]]$specificities[sens_idx_95])
    
    
    auc[ij_idx] <- auc(roccurve[[ij_idx]])
 
    #Brier against train data (BRM?  doesn't have this like GLM?)
    out_brier_train <- NA #BrierScore(StatModel)

    # Brier score against test data
    #f_t <- predicted.classes_of
    #o_t <- of_crossval.test.data$outcome
    #brier[ij_idx] <- mean(((f_t) - o_t)^2)
    brier[ij_idx] <- out_brier_train
    
    #save a record of the odds ratios
    stat_tmp <- summary(StatModel)$coeff
    
    #VarRatios is broken for ProjPred need to debug for now comment out SEP-16-2022
    ##for (kk in 1:dim(stat_tmp)[1]) {
    ##  #the odds ratios for them
    ##  varratios[ij_idx, row.names(stat_tmp)[kk] ] <- exp(stat_tmp[[kk,1]])
    ##}
    
    ##for (kk in 1:dim(stat_tmp)[1]) {
    ##  #the p-values for each
    ##  varsig[ij_idx, row.names(stat_tmp)[kk] ] <- stat_tmp[[kk,4]]
    ##}
    
        
    if (0) { #Verbose Mode
      print('Model Accuracy')
      print(out_acc)
      print('Sensitivity')
      print(sensitivity_store[ij_idx])
      print('Specificity')
      print(specificity_store[ij_idx])
      print('Brier score against test data (fold holdout)')
      print(brier[ij_idx])
      print('Brier Score against Train Data')
      print(out_brier_train)
    }
    
  }
  
  
} #Main Cross-validation loop
  
#add plot for training and for test
calplot_y <- vector()
calplot_x <- vector()
for (kk in 1:dim(calibration_df)[2]) {
  calplot_y[kk] = median(calibration_df[,kk], na.rm = TRUE)
  calplot_x[kk] = kk*cal_interval
}  

plot(calplot_x, calplot_y, type= 'o', xlab = 'Risk Probability Strata', 
     ylab = 'Observed % of Population', panel.first = grid(), 
     main = 'Model Calibration for Test Data')


auc_quantile <- as.numeric(quantile(auc, c(.025, .50, .975)))
brier_quantile <- as.numeric(quantile(brier, c(.025, .50, .975)))
lambda.store_quantile <- as.numeric(quantile(lambda.store, c(.025, .50, .975)))
sensitivity_quantile <- as.numeric(quantile(sensitivity_store, c(.025, .50, .975)))
specificity_quantile <- as.numeric(quantile(specificity_store, c(.025, .50, .975)))


#get quantiles for specificity at particlular sensitiivity
spec90_mat = do.call(cbind,specificity90_store)
specificity90_thresh_quantile <- as.numeric(quantile(spec90_mat[1,], c(.025, .50, .975)))
specificity90_sens_quantile <- as.numeric(quantile(spec90_mat[2,], c(.025, .50, .975)))
specificity90_spec_quantile <- as.numeric(quantile(spec90_mat[3,], c(.025, .50, .975)))

spec95_mat = do.call(cbind,specificity95_store)
specificity95_thresh_quantile <- as.numeric(quantile(spec95_mat[1,], c(.025, .50, .975)))
specificity95_sens_quantile <- as.numeric(quantile(spec95_mat[2,], c(.025, .50, .975)))
specificity95_spec_quantile <- as.numeric(quantile(spec95_mat[3,], c(.025, .50, .975)))


sink(paste(save_path, 'Model_CV_', mnum, '_', model_desc_str, 'summary.txt',sep = '')) 
print("Mean AUC")
print(mean(auc))
print('Quantile')
print(auc_quantile)
print('t-test AUC')
print(t.test(auc))
print('t-test CIs')
print(t.test(auc)$"conf.int")

#This problem doesn't occur with GLM - LASSO sometimes picks similar models
#that aren't very good - so predictions are all false...
if ( sd(brier) != 0 ) {
  print('Mean Brier')
  mean(brier)
  print('Quantile Brier')
  print(brier_quantile)
  print('t-test Brier Score')
  t.test(brier)
  print('t-test Brier CIs')
  t.test(brier)$"conf.int"
  } else { 
    print('Brier score is constant, sd==0')
    mean(brier) }

print('Mean Lambda Score')
mean(lambda.store)
print('Quantile Lambda Score')
print(lambda.store_quantile)
print('t-test Lambda Score')
t.test(lambda.store)
print('t-test Lambda Score CIs')
t.test(lambda.store)$"conf.int"
sink()

pdf(paste(save_path, 'Model_CV_', mnum, '_', model_desc_str, 'AUC_Histogram.pdf',sep = ''))
hist(auc,plot = TRUE, breaks = 25, xlab = 'Area Under the Curve (AUC)',
     main = 'Distibution of AUC Scores')
abline(v = mean(auc), col = "red")
abline(v = median(auc), col = "blue")
dev.off()
  
pdf(paste(save_path, 'Model_CV_', mnum, '_',model_desc_str, 'BrierScore_Histogram.pdf',sep = ''))
hist(brier,plot = TRUE)
dev.off()
  
pdf(paste(save_path,  'Model_CV_', mnum, '_', model_desc_str, 'Lambda_Histogram.pdf',sep = ''))
hist(lambda.store,plot = TRUE)
dev.off()
  
# plot the ROC curves for all models to show uncertainty
myPlot <- ggroc(roccurve, legacy.axes = T) +
  geom_abline(slope = 1 ,intercept = 0) + # add identity line
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 18, face = 'bold'),
    axis.title.y = element_text(size = 18, face = 'bold'),
    panel.border = element_rect(size = 2, fill = NA), 
    axis.text.x = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(size = 14, face = 'bold')) +
  xlab(paste('[Reading:', readingwanted_str, ']  1 - Specificity (FP)' ,sep = '')) +
  ylab(paste('[Day: ', dateRange, ']  Sensitivity (TP)',sep = '')) +
  scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) + 
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  geom_text(x = 0.1, y = 1, colour = "black", size = 6,
            label = paste('Median AUC: ', sprintf("%0.2f",median(auc)), sep = '') ) +
  geom_text(x = 0.11, y = 0.95, colour = "black", size = 5,
          label = paste('Min/Max/Med: ', sprintf("%d,%d,%0.1f",
                                                          min(rocposcases),
                                                          max(rocposcases),
                                                          median(rocposcases)), 
                        sep = '') )

myPlot

ggsave(paste(save_path,  'Model_CV_', mnum, '_', model_desc_str, 'ROCs_for_',
             as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)
  
#this seems to be a hack but we ultimately want to make a subplot with all of the CV results
#across parameters. R does allow subplots but the style is a bit confusing (at least for me atm) 
#if you don't have all of the data ready.
#It's easier to save individually named subplots and then assemble manually - 
#There must be a better way to do this - revisit
assign( paste('roc_cv_p_', p_str, '_', dateRange, '_',outcomeselection, 
              '_',readingwanted,sep = ''), myPlot)
  
#---------------------------
# Stability analysis-using data gathered from above nested folding
#---------------------------
print("Run Stability Analysis...")
  
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
#for (i in 1:n_models) {
#  varratios[[i]] <- round(varratios[[i]], digits = 3)
#}
  
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
sink(paste(save_path,  'Model_CV_', mnum, '_',model_desc_str,
           'Stability_Analysis.txt',sep = ''))
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
  
# (iii) Inclusion frequencies for each variable
#Bootstrap inclusion frequencies for each candidate variable 
#(not only the selected ones)
  
# Make histogram showing the number of variables included in each model
pdf(paste(save_path,  'Model_CV_', mnum, '_', model_desc_str,
          'VariablesPerModel_Histogram.pdf',sep = ''))
hist(variablesinmodel, 
     main = paste('Distribution of Number of variables selected over', 
                  as.character(n_models),'models',sep = ' '),
     xlab = "Number of variables", breaks = (0:length(varcount)))
dev.off()

# Make a bar plot of the included variables
pdf(file = paste(save_path,  'Model_CV_', mnum, '_', model_desc_str, 
                 'Stability_VariableFrequency.pdf',sep = ''), width = 12, 
    height = 6)
par(mar = c(10,10,10,10),mai = c(2,1,1,1))
barplotstab <- barplot(varcount/(n_models)*100,
                       main = 'Distribution of Variables Selected', 
                       ylab = "Frequency (%)", names.arg = varnames[[1]],
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
nmods <- length(modelprop)
  
#Heinze paper says to only conisder top 20 for brevity (if you have that many)
if (nmods > 20) {
  nmods <- 20
}
  
# sort and keep the index value for the sorting procedure
modelprop <- sort(modelprop,decreasing = TRUE, index.return = TRUE)
  
# apply sorting procedure to the matrix to match up the proportions
for (i in 1:length(modelprop$ix)) {
  tempmatrix[i,] = modelvarmatrixunique[modelprop$ix[i],]
}
modelvarmatrixunique <- tempmatrix
  
# determine if need first 20 or 80% cumulative total - 62%
# as first 20 results does not comprise 80% of the cumulative models, 
#we should present only the first 20 models
  
print(paste('Sum-ModelProp % first', nmods,'most common'))
print(sum(modelprop$x[1:nmods])) 
sink()
  
# add in cumulative totals
for (i in 1:nmods) {
  modelprop$cummulative[i] <- sum(modelprop$x[1:i])
}
  
# List of the models of interest, with proportion and cumulative proportions
ModelList <- array(0L, c(nmods,(ncol(modelvarmatrixunique) + 2)))
ModelList[1:nmods,1:ncol(modelvarmatrixunique)] <- modelvarmatrixunique[1:nmods,]
ModelList[1:nmods,(ncol(modelvarmatrixunique) + 1)] <- modelprop$x[1:nmods]
ModelList[1:nmods,(ncol(modelvarmatrixunique) + 2)] <- modelprop$cummulative[1:nmods]
colnames(ModelList) <- c(varnames[[1]],"Prop", "Cummulative") 
  
# (vii) Pairwise variable frequencies
#A matrix with pairwise inclusion frequencies, which are suitably summarized,
#for example as odds ratios obtained by log-linear analysis (see Royston & 
#Sauerbrei, 2003) or as "significant" pairwise over- or under- selection
  
for (i in 1:(n_models)) {
  for (j in 1:length(varratios[[1]])) {
    for (k in 1:length(varratios[[1]])) {
      if (modelvarmatrix[i,j] == 1 & modelvarmatrix[i,k] == 1) {
        freqpairs[j,k] <- freqpairs[j,k] + 1
      }
    }
  }
}
  
#Save varratios to examine -- first convert vector list to tibble
varratios_df <- data.frame() 
for (ii in 1:length(varratios)) {
  tmp <- varratios[[ii]]
  for (jj in 1:length(tmp)) {
    varratios_df[ii,jj] <- tmp[jj]
  }
}

colnames(varratios_df) <- varnames[[1]]
write.csv(varratios_df,paste(save_path,'BAYES_VarRatios.csv', sep = ''))

#add in variables details about model setup
varratios_stat_df[m_ctr,1] <- outcome_str
varratios_stat_df[m_ctr,2] <- readingwanted_str
varratios_stat_df[m_ctr,3] <- dateRange

#prepare summary of var ratios depending on model stability
for (ii in 1:dim(varratios_df)[2]) {
  #if majority of models have this column as significant then get stats
  if ( sum(varratios_df[,ii] > 1.01 | varratios_df[,ii] < 0.99) > 50) {
    #for frequent (>50) coefficients, save the meadian value
    varratios_stat_df[m_ctr, ii + 3] <- summary(varratios_df[,ii])[[3]]
  } else {
    varratios_stat_df[m_ctr, ii + 3] <- NA
  }
}

#note this is a compendium - we don't need to save each loop iteration but do so just in case 
colnames(varratios_stat_df) <- varnames[[1]]
write.csv(varratios_stat_df,paste(output_path,'BAYES_MedianVarRatios_Compendium.csv', sep = ''))


colnames(freqpairs) <- varnames[[1]]
rownames(freqpairs) <- varnames[[1]]
pdf(paste(save_path,  'Model_CV_', mnum, '_', model_desc_str, 
          'variable_selection_correlation_stability.pdf',sep = ''))
# correlation plot CURRENTLY MESSY AND MORE ROBUST MEASURES ARE NEEDED
corrplot(cor(freqpairs), method = "number",number.cex = 0.2,tl.cex = 0.35)
dev.off()