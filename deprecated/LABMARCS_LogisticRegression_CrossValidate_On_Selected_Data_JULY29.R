# perform nested cross-validation to report on the generalised performance
# Model 1 GLM & # Model 2 STEP_AIC
mod_ls <- c('GLM') #,'StepAIC','Lasso')

#Do cross validation for each type of model
for (mnum in mod_ls) { 
  print(paste('model:',mnum))
  
  print("Run Model Cross Validation...")
  model_desc_str = paste(mnum,'_R',as.character(repeats),'_OF', 
                         as.character(outsidefolds), '_IF', 
                         as.character(insidefolds), '_', sep = '')
  
  # Initialize outcome measures
  lambda.store <- vector("numeric", length = n_models)
  auc <- vector("numeric", length = n_models)
  brier <- vector("numeric", length = n_models)
  includedvars <- list(length = n_models)
  varnames <- vector(mode = "list", length = n_models)
  varratios <- vector(mode = "list", length = n_models)
  roccurve <- vector(mode = "list", length = n_models)
  
  #Loop for cross validation
  for (j in 1:repeats) {
    #print(paste('repeat:',j))
    
    # Randomly shuffle the training data
    train.data <- train.data[sample(nrow(train.data)),]
    
    # Create n equally sized outer folds
    data.outerfolds <- cut(seq(1,nrow(train.data)),breaks = outsidefolds,
                           labels = FALSE)
    
    for (i in 1:outsidefolds) {
      #print(paste('outsidefolds:',i))
      
      #Segment data by fold using the which() function 
      testIndexes <- which(data.outerfolds == i,arr.ind = TRUE)
      #cross validation only on train data for outerfolds
      of_test.data <- train.data[testIndexes, ]
      of_train.data <- train.data[-testIndexes, ]
      
      if (mnum == 'GLM') {
        StatModel <- glm(outcome ~.,data = of_train.data, family = "binomial")
        probabilities_of <- predict(object = StatModel, of_test.data, type = "response")
        
      } else if (mnum == 'Lasso') {
        x <- model.matrix(outcome~., of_train.data)[,-1]
        
        # Determine lambda.1se parameter over inner folds for the training data
        cv.lasso <- cv.glmnet(x,of_train.data$outcome, alpha = 1, data = of_train.data,  
                              nfolds = insidefolds,
                              family = "binomial")
        
        lambda.store[outsidefolds*(j - 1) + i] <- cv.lasso$lambda.1se
        
        StatModel <- glmnet(x, of_train.data$outcome, alpha = 1, family = "binomial",
                            lambda = cv.lasso$lambda.1se) 
        
        x.test <- model.matrix(outcome ~., test.data)[,-1]
        probabilities_of <- StatModel %>% predict(newx = x.test, type = "response")
        
      } 
      
      # Test the best predicted lambda on the remaining data in the outer fold
      predicted.classes_of <- ifelse(probabilities_of > 0.5, 1, 0)
      
      # Model accuracy
      out_acc <- mean(predicted.classes_of == of_test.data$outcome)
      
      # Sensitivity and specificity measures
      conf_matrix_of <- table(predicted.classes_of, of_test.data$outcome)
      
      tab <- matrix(c(0,0,0,0), ncol = 2, byrow = TRUE)
      colnames(tab) <- c('0','1')
      rownames(tab) <- c('0','1')
      tab <- as.table(tab)
      
      #sometimes the conf matrix is incomplete copy what we do have to tab
      tryCatch(tab[1,1] <- conf_matrix_of[1,'FALSE'], error = function(e) {tab[1,1] <- 0 } )
      tryCatch(tab[2,1] <- conf_matrix_of[2,'FALSE'], error = function(e) {tab[2,1] <- 0 } )
      tryCatch(tab[1,2] <- conf_matrix_of[1,'TRUE'], error = function(e) {tab[1,2] <- 0 } )
      tryCatch(tab[2,2] <- conf_matrix_of[2,'TRUE'], error = function(e) {tab[2,2] <- 0 } )
      
      out_sen <- as.numeric(sensitivity(tab)['.estimate'])
      out_spec <- as.numeric(specificity(tab)['.estimate'])
      
      # Plot ROC curve
      roccurve[[outsidefolds*(j - 1) + i]] <- roc(outcome ~ c(probabilities_of), 
                                                  data = of_test.data)
      #plot.roc(roccurve[[1]])
      
      auc[outsidefolds*(j - 1) + i] <- auc(roccurve[[outsidefolds*(j - 1) + i]])
      
      #Brier against train data
      out_brier_train <- BrierScore(StatModel)
      
      # Brier score against test data
      f_t <- predicted.classes_of
      o_t <- of_test.data$outcome
      brier[outsidefolds*(j - 1) + i] <- mean(((f_t) - o_t)^2)
      
      #save a record of the odds ratios
      modelcoefs <- exp(coef(StatModel))
      varnames[[outsidefolds*(j - 1) + i]] <- names(modelcoefs)
      varratios[[outsidefolds*(j - 1) + i]] <- modelcoefs
      
      #for lasso
      #varnames[[outsidefolds*(j - 1) + i]] <- modelcoefs@Dimnames[[1]]
      #varratios[[outsidefolds*(j - 1) + i]] <- modelcoefs@x
      
      if (0) { #Verbose Mode
        print('Model Accuracy')
        print(out_acc)
        print('Sensitivity')
        print(out_sen)
        print('Specificity')
        print(out_spec)
        print('Brier score against test data (fold holdout)')
        print(brier[outsidefolds*(j - 1) + i])
        print('Brier Score against Train Data')
        print(out_brier_train)
      }
      
    }
  } #Main Cross-validation loop
  
  auc_quantile <- as.numeric(quantile(auc, c(.025, .50, .975)))
  brier_quantile <- as.numeric(quantile(brier, c(.025, .50, .975)))
  lambda.store_quantile <- as.numeric(quantile(lambda.store, c(.025, .50, .975)))
  
  sink(paste(save_path, 'Model_CV_', mnum, '_', model_desc_str, 'summary.txt',sep = '')) 
  print("Mean AUC")
  print(mean(auc))
  print('Quantile')
  
  print(auc_quantile)
  print('t-test AUC')
  print(t.test(auc))
  print('t-test CIs')
  print(t.test(auc)$"conf.int")
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
  
  #save things to our summary data table
  cv_batch_df[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                           mean(auc), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                           mean(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                           mean(lambda.store), lambda.store_quantile[1],
                           lambda.store_quantile[2],lambda.store_quantile[3])
  
  write.table(cv_batch_df, file = paste(output_path, model_desc_str, 'Summary_Table.csv',sep = ''),
              row.names = FALSE, sep = ',')
  
  
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
  
  ggsave(paste(save_path,  'Model_CV_', mnum, '_', model_desc_str, 'ROCs_for_',
               as.character(n_models), '_models.pdf',sep = ''), device = 'pdf',
         width = 20, height = 20, units = 'cm', dpi = 300)
  
  
  
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
  for (i in 1:n_models) {
    varratios[[i]] <- round(varratios[[i]], digits = 3)
  }
  
  #Note GLM does not do variable selection
  for (i in 1:(n_models)) {
    for (j in 1:length(varratios[[1]])) {
      if (varratios[[i]][j] < 0.999 | varratios[[i]][j] > 1.001 | mnum == 'GLM') {
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
  
  colnames(freqpairs) <- c(varnames[[1]]) 
  rownames(freqpairs) <- c(varnames[[1]])
  pdf(paste(save_path,  'Model_CV_', mnum, '_', model_desc_str, 
            'variable_selection_correlation_stability.pdf',sep = ''))
  # correlation plot CURRENTLY MESSY AND MORE ROBUST MEASURES ARE NEEDED
  corrplot(cor(freqpairs), method = "number",number.cex = 0.2,tl.cex = 0.35)
  dev.off()
  
  #save things to our summary data table
  #batch_df[m_ctr,] <- c('FullLasso', readingwanted_str, dateRange, outcome_str,
  #                      out_acc,out_auc, out_brier, out_sen, out_spec)
  #write.table(batch_df, file = paste(output_path,'Batch_Model_Summary_Table.csv',sep = ''),
  #            row.names = FALSE)
  
} #Loop for Stat model type (GLM, AIC, Lasso)

