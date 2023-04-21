# perform nested cross-validation to report on the GLM models
mnum = 'GLM'
print("Run GLM Model Cross Validation...")
model_desc_str = paste(cv_desc,mnum,'_R',as.character(repeats),'_OF', 
                       as.character(outsidefolds), '_IF', 
                       as.character(insidefolds), '_', sep = '')
  
# Initialize outcome measures
lambda.store <- vector("numeric", length = n_models)
auc_store <- vector("numeric", length = n_models)
brier <- vector("numeric", length = n_models)
sensitivity_store <- vector(mode = "numeric", length = n_models)
specificity_store <- vector(mode = "numeric", length = n_models)

specificity90_store <- list()
specificity95_store <- list()

rocposcases <- vector(mode = "numeric", length = n_models)
includedvars <- list(length = n_models)
roccurve <- vector(mode = "list", length = n_models)

calibration_df = data.frame()
calibration_n_df = data.frame()

#initiate stat model list data structure (don't need to reset when test generalisation)
if(!generalise_flag) {
  print('Training mode...reset StatModel_ls...')
  StatModel_ls <- vector(mode = "list", length = n_models)
}

#Technically the dimension should be columns -1 since we don't include outcome
#in predictions, but intercept is added in list of coefficients
varratios <- as_tibble(matrix(99,nrow = n_models,ncol = dim(train.data)[2] ))
colnames(varratios) <- names(train.data)
varratios <- rename(varratios, '(Intercept)' = 'outcome' )

#store the p-values as glm   
varsig <- varratios
  
print('Before GLM CV loop')
#Loop for cross validation
for (j in 1:repeats) {
  
  print('Pre outside fold loop...')
  for (i in 1:outsidefolds) {
    
    ij_idx = outsidefolds*(j - 1) + i
    #print(paste('outsidefolds:',i))
    
    #Segment data by fold using the which() function
    #this selects a portion of data as function of number of outerfolds, e.g.
    #4 outer folds means 1/4 or 25% of data is used for test, 75% for train
    testIndexes <- which(shuffle_index_ls[[j]] == i,arr.ind = TRUE)
    #cross validation only on train data for outerfolds
    
    if (lasso_run) { #use reduced variable model
      print('Lasso Run activated...')
      of_crossval.train.data = train.data[-testIndexes, lasso_var_ls]
      of_crossval.test.data = train.data[testIndexes, lasso_var_ls]
    } else { 
      print('Standard GLM') 
      of_crossval.train.data <- train.data[-testIndexes, ]
      of_crossval.test.data <- train.data[testIndexes, ]  
    }

    # Note the generalise data must be run after the initial models have been setup
    # else the StatModel_ls will be empty and generalisaton will cause an error
    if (generalise_flag) {
      # Test CV on dataset to generalise to 
      of_crossval.test.data <- crossval.test.data
      StatModel = StatModel_ls[[ij_idx]]
      
    } else { 
      # Save models for test with generalisation set
      StatModel <- glm(outcome ~.,data = of_crossval.train.data, family = "binomial")
      StatModel_ls[[ij_idx]] <- StatModel

      # Test on the portion of training left out for CV
    }
    
    probabilities_of <- predict(object = StatModel, of_crossval.test.data, type = "response")

    #model calibration bin each set of probabilities and then count the percent of our
    #patients that fall in that category
    cal_interval <- 0.1
    zz = 0
    for (kk in seq(0,1 - cal_interval,cal_interval)) {
      
      zz = zz + 1
      #which probabilites are in this interval
      if (kk == 1) { kk = 1.1} #adjust for last bin to include 1
      
      #find which patients are in this bin
      tmp_prob_bin_idx <- (probabilities_of >= kk) & (probabilities_of < kk + cal_interval)
      
      #of the patients in the bin what % had the outcome
      cal_p = sum(of_crossval.test.data$outcome[tmp_prob_bin_idx])/sum(tmp_prob_bin_idx)
      
      #save number of interval and %cases in this bin
      #calibration_df[zz,1] = kk
      #what is the % true outcome for this particular bin
      calibration_df[ij_idx,zz] = cal_p
      calibration_n_df[ij_idx,zz] = sum(tmp_prob_bin_idx)
    }
    
    # Test the best predicted lambda on the remaining data in the outer fold
    # Note using 0.5 is arbitrary (assumes calibrated model) 
    # you could vary the threshold to achieve differeent sens/spec
    predicted.classes_of <- ifelse(probabilities_of > 0.5, 1, 0)
    
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
    #plot.roc(roccurve[[1]])
    
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
    
    
    auc_store[ij_idx] <- auc(roccurve[[ij_idx]])
    
    #Brier against train data
    out_brier_train <- BrierScore(StatModel)
    
    # Brier score against test data
    f_t <- predicted.classes_of
    o_t <- of_crossval.test.data$outcome
    brier[ij_idx] <- mean(((f_t) - o_t)^2)
    
    #save a record of the odds ratios
    stat_tmp <- summary(StatModel)$coeff
    
    for (kk in 1:dim(stat_tmp)[1]) {
      #the odds ratios for them
      varratios[ij_idx, row.names(stat_tmp)[kk] ] <- exp(stat_tmp[[kk,1]])
    }
    
    for (kk in 1:dim(stat_tmp)[1]) {
      #the p-values for each
      varsig[ij_idx, row.names(stat_tmp)[kk] ] <- stat_tmp[[kk,4]]
    }
    
    #for lasso
    #varnames[[ij_idx]] <- modelcoefs@Dimnames[[1]]
    #varratios[[ij_idx]] <- modelcoefs@x
    
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
  


#plot calibration of model expected vs. observed probabilities
source(paste(work_path,'CalibrationPlot_CV.R', sep = ''))


auc_quantile <- as.numeric(quantile(auc_store, c(.025, .50, .975)))
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


sink(paste(save_path, 'CV_', mnum, '_', model_desc_str, 'summary.txt',sep = '')) 
print("Mean AUC")
print(mean(auc_store))
print('Quantile')
print(auc_quantile)
print('t-test AUC')
print(t.test(auc_store))
print('t-test CIs')
print(t.test(auc_store)$"conf.int")

print('Mean Brier')
mean(brier)
print('Quantile Brier')
print(brier_quantile)
print('t-test Brier Score')
t.test(brier)
print('t-test Brier CIs')
t.test(brier)$"conf.int"

print('Mean Lambda Score')
mean(lambda.store)
print('Quantile Lambda Score')
print(lambda.store_quantile)
print('t-test Lambda Score')
t.test(lambda.store)
print('t-test Lambda Score CIs')
t.test(lambda.store)$"conf.int"
sink()

pdf(paste(save_path, 'CV_', mnum, '_', model_desc_str, 'AUC_Histogram.pdf',sep = ''))
hist(auc_store,plot = TRUE, breaks = 25, xlab = 'Area Under the Curve (AUC)',
     main = 'Distibution of AUC Scores')
abline(v = mean(auc_store), col = "red")
abline(v = median(auc_store), col = "blue")
dev.off()
  
pdf(paste(save_path, 'CV_', mnum, '_',model_desc_str, 'BrierScore_Histogram.pdf',sep = ''))
hist(brier,plot = TRUE)
dev.off()
  
pdf(paste(save_path,  'CV_', mnum, '_', model_desc_str, 'Lambda_Histogram.pdf',sep = ''))
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
            label = paste('Median AUC: ', sprintf("%0.2f",median(auc_store)), sep = '') ) +
  geom_text(x = 0.11, y = 0.95, colour = "black", size = 5,
          label = paste('Min/Max/Med: ', sprintf("%d,%d,%0.1f",
                                                          min(rocposcases),
                                                          max(rocposcases),
                                                          median(rocposcases)), 
                        sep = '') )

myPlot

ggsave(paste(save_path,  'CV_', mnum, '_', model_desc_str, 'ROCs_for_',
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
varcount <- vector("numeric", length =  dim(varratios)[2] )
# List of the total number of variables in each model
variablesinmodel <- vector("numeric", length = n_models)
# A matrix with model index versus inclusion of each variable in that model
modelvarmatrix <- array(0L, c(n_models,  dim(varratios)[2] ))
# List of the value of the 'events per variable' in each model
EPV <- vector("numeric", length = n_models)
# List of the frequency of pairwise inclusion of each variable over all models
freqpairs <- array(0L, c(dim(varratios)[2], dim(varratios)[2] ))
  
# cycle through each model and each variable counting when variable is used
# i.e. variable value from LASSO =\= 1.
# Also count the number of variables used (=\= 1) in each model in total
#for (i in 1:n_models) {
#  varratios[[i]] <- round(varratios[[i]], digits = 3)
#}
  
pval_thresh <- 0.05
#Note GLM does not do variable selection
for (i in 1:n_models) {
  for (j in 1:dim(varratios)[2]) {
    if ( !is.na(varsig[i,j])) { #sometimes model converges on NaN
      if ( varsig[i,j] <= pval_thresh ) { 
        varcount[j] <- varcount[j] + 1
        variablesinmodel[i] <- variablesinmodel[i] + 1
        modelvarmatrix[i,j] <- 1
      }
    }
  }
  EPV[i] <- eventsnumber/variablesinmodel[i]
}
  
sink(paste(save_path,  'CV_', mnum, '_',model_desc_str,
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
pdf(paste(save_path,  'CV_', mnum, '_', model_desc_str,
          'VariablesPerHistogram.pdf',sep = ''))
hist(variablesinmodel, 
     main = paste('Distribution of Number of variables selected over', 
                  as.character(n_models),'models',sep = ' '),
     xlab = "Number of variables", breaks = (0:length(varcount)))
dev.off()

# Make a bar plot of the included variables
tmp = tibble('Variable'=names(varratios),'Count'=varcount,'Percent'=(varcount/(n_models)*100)  )
write.csv(tmp,paste(save_path,  'CV_', mnum, '_', model_desc_str, 
                    'Stability_VariableFrequency.csv',sep = ''))

#bar plot sometimes misses labels? use text file above as final verdict
pdf(file = paste(save_path,  'CV_', mnum, '_', model_desc_str, 
                 'Stability_VariableFrequency.pdf',sep = ''), width = 12, 
    height = 6)
par(mar = c(10,10,10,10),mai = c(2,1,1,1))
barplotstab <- barplot(varcount/(n_models)*100,
                       main = 'Distribution of Variables Selected', 
                       ylab = "Frequency (%)", names.arg = names(varratios),
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
colnames(ModelList) <- c(names(varratios),"Prop", "Cummulative") 
  
# (vii) Pairwise variable frequencies
#A matrix with pairwise inclusion frequencies, which are suitably summarized,
#for example as odds ratios obtained by log-linear analysis (see Royston & 
#Sauerbrei, 2003) or as "significant" pairwise over- or under- selection
  
for (i in 1:(n_models)) {
  for (j in 1:dim(varratios)[2]) {
    for (k in 1:dim(varratios)[2]) {
      if (modelvarmatrix[i,j] == 1 & modelvarmatrix[i,k] == 1) {
        freqpairs[j,k] <- freqpairs[j,k] + 1
      }
    }
  }
}
  
colnames(freqpairs) <- names(varratios)
rownames(freqpairs) <- names(varratios)
pdf(paste(save_path,  'CV_', mnum, '_', model_desc_str, 
          'variable_selection_correlation_stability.pdf',sep = ''))
# correlation plot CURRENTLY MESSY AND MORE ROBUST MEASURES ARE NEEDED
corrplot(cor(freqpairs), method = "number",number.cex = 0.2,tl.cex = 0.35)
dev.off()

