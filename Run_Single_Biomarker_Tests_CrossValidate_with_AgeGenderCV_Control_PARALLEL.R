print("Run GLM/BRM model per biomarker (controlling for gender/age)")

#groups of variables, instead of doing test individually group by biomarker
var_groups = list("Age",
                  "Gender_F",                  
                  c("Age","Gender_F"),
                  "Nosocomial_1",
                  c("BE_Abnormal", "BE_NA"),         
                  c("BNP_Abnormal","BNP_NA"),
                  c("CRP_Abnormal", "CRP_NA"),
                  c("DDM_Abnormal", "DDM_NA"),        
                  c("eGFR_Abnormal","eGFR_NA"),
                  c("FER_Mild", "FER_Moderate","FER_Severe","FER_NA"),        
                  c("fib_Mild", "fib_Severe", "fib_NA"),        
                  c("Glucose_Abnormal","Glucose_NA"),
                  c("HB_Mild","HB_Moderate","HB_Severe","HB_NA"),         
                  c("HBA1c_Abnormal","HBA1c_NA"),
                  c("LDH_Mild", "LDH_Moderate", "LDH_Severe","LDH_NA"),
                  c("PCT_Abnormal","PCT_NA"),
                  c("PLT_Mild","PLT_Moderate", "PLT_Severe","PLT_NA"),
                  c( "trig_Abnormal","trig_NA"),
                  c("trop_Abnormal", "trop_NA"),       
                  c( "Lymphocytes_Mild","Lymphocytes_Moderate", "Lymphocytes_Severe", "Lymphocytes_NA"),
                  c("Neutrophils_Mild", "Neutrophils_Moderate","Neutrophils_Severe",  "Neutrophils_NA"),
                  c("WCC_Mild","WCC_Moderate","WCC_Severe","WCC_NA"),        
                  c("NLR_Mild","NLR_Moderate","NLR_Severe", "NLR_NA"),
                  c("APTT_Mild", "APTT_Moderate" ,"APTT_NA"),
                  c("PT_Abnormal","PT_NA"),         
                  c( "poctLAC_Abnormal","poctLAC_NA"),
                  c("O2_Abnormal", "O2_NA"),
                  c("CO2_Abnormal", "CO2_NA"),        
                  c("poctpH_Abnormal" , "poctpH_NA"),
                  c("Urea_Abnormal", "Urea_NA"),
                  "viral_coinfection_TRUE",
                  "bc_coinfection_TRUE",
                  "resp_coinfection_TRUE",
                  "urine_coinfection_TRUE")    

if (!BatchAnalysisOn) {
  #Initialise results tibble, else done in the batch file
  univar_batch_result_tb <- tibble(
    'VarGroup' = character(),
    'Variable' = character(),
    'Samples' = numeric(),
    'Observations' = numeric(),
    'Obs. %' = numeric(),
    'Non-Severe Absent N' = numeric(),
    'Non-Severe Present N' = numeric(),
    'Severe Absent N' = numeric(),
    'Severe Present N' = numeric(),
    'P Value' = numeric(),
    'AUC' = numeric(),
    'AUC CV CI 2.5' = numeric(),
    'AUC CV CI 50' = numeric(),
    'AUC CV CI 97.5' = numeric(),
    'AUC Difference CV CI 2.5' = numeric(),
    'AUC Difference CV CI 50' = numeric(),
    'AUC Difference CV CI 97.5' = numeric(),
    'Odds Ratio' = numeric(),
    'CI2.5' = numeric(),
    'CI97.5' = numeric(),
    'Bayes AUC' = numeric(),
    'Bayes AUC CV CI 2.5' = numeric(),
    'Bayes AUC CV CI 50' = numeric(),
    'Bayes AUC CV CI 97.5' = numeric(),
    'Bayes Odds Ratio' = numeric(),
    'Bayes CI2.5' = numeric(),
    'Bayes CI97.5' = numeric(),
    'Bayes AUC Difference CV CI 2.5' = numeric(),
    'Bayes AUC Difference CV CI 50' = numeric(),
    'Bayes AUC Difference CV CI 97.5' = numeric(),
    'Bayes AUC HS' = numeric(),
    'Bayes HS AUC CV CI 2.5' = numeric(),
    'Bayes HS AUC CV CI 50' = numeric(),
    'Bayes HS AUC CV CI 97.5' = numeric(),
    'Bayes HS AUC Difference CV CI 2.5' = numeric(),
    'Bayes HS AUC Difference CV CI 50' = numeric(),
    'Bayes HS AUC Difference CV CI 97.5' = numeric(),
    'Bayes HS Odds Ratio' = numeric(),
    'Bayes HS CI2.5' = numeric(),
    'Bayes HS CI97.5' = numeric()
  )
}

#make a copy to hold parallel data
univar_batch_result_tb_PAR = univar_batch_result_tb

# Initialise model outputs into lists
model_glm <- vector(mode = "list", length = length(var_groups))
model_brm <- vector(mode = "list", length = length(var_groups))
model_brm_hs <- vector(mode = "list", length = length(var_groups))

family_ls = c('glm.fit','firthglm.fit')

# Initialize outcome measures for cross validation (currently using only AUC but just in case)
auc <- vector("numeric", length = n_models)
auc_brm <- vector("numeric", length = n_models)
auc_brm_hs <- vector("numeric", length = n_models)

auc_ctrl <- vector("numeric", length = n_models)
auc_brm_ctrl <- vector("numeric", length = n_models)
auc_brm_hs_ctrl <- vector("numeric", length = n_models)

brier <- vector("numeric", length = n_models)
sensitivity_store <- vector(mode = "numeric", length = n_models)
specificity_store <- vector(mode = "numeric", length = n_models)
rocposcases <- vector(mode = "numeric", length = n_models)
includedvars <- list(length = n_models)

roccurve <- vector(mode = "list", length = n_models)
roccurve_brm <- vector(mode = "list", length = n_models)
roccurve_brm_hs <- vector(mode = "list", length = n_models)

roccurve_ctrl <- vector(mode = "list", length = n_models)
roccurve_brm_ctrl <- vector(mode = "list", length = n_models)
roccurve_brm_hs_ctrl <- vector(mode = "list", length = n_models)

StatModel_ls <- vector(mode = "list", length = n_models)


NumberClusters <- 8
# how many jobs you want the computer to run at the same time
cl <- makeCluster(NumberClusters) # Make clusters
registerDoSNOW(cl) # use the above cluster


#build individual models for each variable we have recorded factoring in age/gender

#results <- foreach(i = val, .combine = "rbind") %dopar% {
#for (i in 1:(length(var_groups)) ) { 
univar_batch_result_tb_PAR <- foreach(i = 1:(length(var_groups)), .combine = "rbind") %dopar% {

  source(paste(work_path,'ReloadLibraries.R', sep = ''))
  print('Evaluate Biomarker:')
  print(c(i, var_groups[[i]]))
  
  if ( i < 4) { #run age & gender solo
    brm_data = train.data[, c('outcome', var_groups[[i]])]
  } else { #for all other vars beyond age/gender
    #create temp data store with required var groups for glm & brm
    brm_data = train.data[, c('outcome', 'Age', 'Gender_F', var_groups[[i]])]
  }
  
  #comparison using only age/gender
  brm_data_ctrl = train.data[, c('outcome', 'Age', 'Gender_F')]
  
  if(binary_flag){
    #make multilevel markers binary (ignore entries with 5 cols (out,age,gen,TestAbnorm,NA)
    if ( (i > 4) & dim(brm_data)[2] > 5 ) { #need to binarise
      #select rows with levels (and keep NA row in needed)
      tmp = as.logical(rowSums(brm_data[,4:(dim(brm_data)[2] - 1)] == TRUE))
      brm_data_tmp = data.frame('ABNORMAL' = tmp)
      brm_data_tmp = add_column(brm_data,brm_data_tmp)
      brm_data_tmp = brm_data_tmp[,c(1:3,(dim(brm_data_tmp)[2] - 1):dim(brm_data_tmp)[2])]
      brm_data = brm_data_tmp
    }
  }
  
  #check if biomarker has an NA column and exclude NAs
  if (regexpr('_NA',var_groups[[i]][length(var_groups[[i]])])[1] != -1 ) {
    #remove NAs
    brm_data_ctrl = brm_data_ctrl[ brm_data[, dim(brm_data)[2]] != TRUE , ]
    #now overwite the original
    brm_data = brm_data[ brm_data[, dim(brm_data)[2]] != TRUE , ]

    #remove NA column
    brm_data = brm_data[ , 1:(dim(brm_data)[2] - 1) ]
  }
  
  
  #First get stats for non-cross validated full model
  
  #Standard GLM
  model_glm[[i]] <- glm(data = brm_data, formula = outcome ~ .,
                        family = "binomial",
                        method = family_ls[IncludeFirthBias + 1])

  
  
  model_brm[[i]] <- brm(outcome ~ .,
                        data = brm_data,
                        family = bernoulli(), cores = 4,
                        threads = threading(2), #threads per core
                        backend = 'cmdstanr', silent = 2)

  #Bayes with horseshoe prior
  model_brm_hs[[i]] <- brm(outcome ~ .,
                           data = brm_data,
                           prior = prior(horseshoe(), class = b),
                           family = bernoulli(), cores = 4,
                           threads = threading(2), #threads per core
                           backend = 'cmdstanr', silent = 2)
  
  
  #Compute AUC for the 3 types of models
  #Standard GLM
  Prob <- predict(model_glm[[i]], type = "response")
  Pred <- prediction(Prob, brm_data$outcome)
  AUC <- performance(Pred, measure = "auc")
  AUC_glm <- AUC@y.values[[1]]
  
  #Bayes with flat prior
  Prob <- predict(model_brm[[i]], type = "response")
  Pred <- prediction(Prob[,1], brm_data$outcome)
  AUC <- performance(Pred, measure = "auc")
  AUC_brm <- AUC@y.values[[1]]
  
  #Bayes with horseshoe prior
  Prob <- predict(model_brm_hs[[i]], type = "response")
  Pred <- prediction(Prob[,1], brm_data$outcome)
  AUC <- performance(Pred, measure = "auc")
  AUC_brm_hs <- AUC@y.values[[1]]
  
  
  #get coefficients p-values - and print if significant
  tmp <- coef(summary(model_glm[[i]]))
  tmp_ci = confint(model_glm[[i]], level = 0.95)      
  
  #need to extract coefficients but brm saves in different format compared to glm
  tmp_b = summary(model_brm[[i]]$fit)[1] #first entry has summary of mc chains
  #retrieve coefficient estimate and confidence interval
  #intercept is first & lp_ is last so only take inner portion of results
  tmp_b = exp(tmp_b$summary[2:(dim(tmp_b$summary)[1] - 1), c(1,4,8)])
  
  #Same but with horseshoe prior
  #need to extract coefficients but brm saves in different format compared to glm
  tmp_b_hs = summary(model_brm_hs[[i]]$fit)[1] #first entry has summary of mc chains
  #retrieve coefficient estimate and confidence interval
  #intercept is first & lp_ is last so only take inner portion of results
  tmp_b_hs = exp(tmp_b_hs$summary[2:(dim(tmp_b_hs$summary)[1] - 1), c(1,4,8)])
  
  
  #for printout we don't need age/gender for each biomarker
  if ( i < 4) {init_row = 2
  } else { init_row = 4}
  
  for (j in init_row:dim(tmp)[1]) { #skip first entry (intercept)
    
    #create new line for each predictor
    ctr <- dim(univar_batch_result_tb)[1] + 1
    
    #get some numbers on distribution of severity & readings
    #find where NAs are and only consider valid readings
    na_idx = grep('_NA',names(brm_data))
    
    if (length(na_idx) == 0) { #no NAs ignore set all true
      non_na_indices = array(data = TRUE, dim = dim(brm_data)[1])
    } else { #get only valid indices
      non_na_indices = brm_data[,na_idx] == FALSE
    }
    
    n_severe = brm_data[non_na_indices,1] == TRUE
    n_non_severe = brm_data[non_na_indices,1] == FALSE
    n_abnormal = brm_data[non_na_indices,j] == TRUE
    n_normal = brm_data[non_na_indices,j] == FALSE
    
    #n with normal and severe
    n_nonsevere_normal = sum(n_non_severe & n_normal)
    n_nonsevere_abnormal = sum(n_non_severe & n_abnormal)
    n_severe_normal = sum(n_severe & n_normal)
    n_severe_abnormal = sum(n_severe & n_abnormal)
    univar_batch_result_tb[ctr,'Non-Severe Absent N'] = n_nonsevere_normal
    univar_batch_result_tb[ctr,'Non-Severe Present N'] = n_nonsevere_abnormal
    univar_batch_result_tb[ctr,'Severe Absent N'] = n_severe_normal
    univar_batch_result_tb[ctr,'Severe Present N'] = n_severe_abnormal
    
    univar_batch_result_tb[ctr,'VarGroup'] <- var_groups[[i]][1]
    univar_batch_result_tb[ctr,'Variable'] <- row.names(tmp)[j]
    univar_batch_result_tb[ctr,'Samples'] =  dim(brm_data)[1]
    univar_batch_result_tb[ctr,'Observations'] =  sum(brm_data[,names(brm_data)[j]] > 0)
    univar_batch_result_tb[ctr,'Obs. %'] =  sum(brm_data[,names(brm_data)[j]] > 0)/dim(brm_data)[1]
    univar_batch_result_tb[ctr,'P Value'] <- tmp[j,4]
    
    univar_batch_result_tb[ctr,'AUC'] <- AUC_glm
    univar_batch_result_tb[ctr,'Odds Ratio'] <- exp(tmp[j,'Estimate'])
    univar_batch_result_tb[ctr,'CI2.5'] <- exp(tmp_ci[j,'2.5 %'])
    univar_batch_result_tb[ctr,'CI97.5'] <- exp(tmp_ci[j,'97.5 %'])
    
    univar_batch_result_tb[ctr,'Bayes AUC'] <- AUC_brm
    univar_batch_result_tb[ctr,'Bayes Odds Ratio'] <- tmp_b[j - 1,'mean']
    univar_batch_result_tb[ctr,'Bayes CI2.5'] <- tmp_b[j - 1,'2.5%']
    univar_batch_result_tb[ctr,'Bayes CI97.5'] <- tmp_b[j - 1,'97.5%']
    
    univar_batch_result_tb[ctr,'Bayes AUC HS'] <- AUC_brm_hs
    univar_batch_result_tb[ctr,'Bayes HS Odds Ratio'] <- tmp_b_hs[j - 1,'mean']
    univar_batch_result_tb[ctr,'Bayes HS CI2.5'] <- tmp_b_hs[j - 1,'2.5%']
    univar_batch_result_tb[ctr,'Bayes HS CI97.5'] <- tmp_b_hs[j - 1,'97.5%']
    
  }
  

  print('START CROSS-VALIDATION')
  
  #Now run cross validated models 
  for (jj in 1:repeats) {
    print('CV-repeat #')
    print(jj)
    
    # Randomly shuffle the training data
    smp = sample(nrow(brm_data))
    crossval.train.data <- brm_data[smp,]
    #contstruct a comparison control model without NA's using only age+gender
    crossval.train.data_ctrl <- brm_data_ctrl[smp,]
    
    # Create n equally sized outer folds with label 1:N for each break
    data.outerfolds <- cut(seq(1,nrow(crossval.train.data)),breaks = outsidefolds,
                           labels = FALSE)
    data.outerfolds_ctrl <- cut(seq(1,nrow(crossval.train.data_ctrl)),breaks = outsidefolds,
                                labels = FALSE)
    
    #First Make sure each split has pos/neg examples, else resample
    testSmp = TRUE 
    ctr_smp = 0
    ctr_smp_flag = FALSE
    while (testSmp) {
      for (ii in 1:outsidefolds) {
        
        #Get fold segment indices 
        testIndexes <- which(data.outerfolds == ii,arr.ind = TRUE)
        
        #cross validation only on train data for outerfolds
        of_crossval.test.data <- crossval.train.data[testIndexes, ]
        of_crossval.test.data_ctrl <- crossval.train.data_ctrl[testIndexes, ]
        
        #guarantee at least 1 of each outcome
        if ( length(unique(of_crossval.test.data$outcome)) == 1 ) { #
          print('Resample')
          smp=sample(nrow(crossval.train.data))
          crossval.train.data <- crossval.train.data[smp,]
          crossval.train.data_ctrl <- crossval.train.data_ctrl[smp, ]
          
          # Create n equally sized outer folds
          data.outerfolds <- cut(seq(1,nrow(crossval.train.data)),breaks = outsidefolds,
                                 labels = FALSE)

          ctr_smp = ctr_smp + 1
          if (ctr_smp > 100) { #if we can't resample easily just go ahead (models will be bad)
            print('stop resampling - not enough outcome examples')
            ctr_smp_flag = TRUE
            testSmp = FALSE }
          
          break
        } else if (ii == outsidefolds) { #reach the end successfully
          testSmp = FALSE
        } 
      }
    } # while(testSmp)
    
    #Now that we have a valid set of samples proceed to fit models
    if (!ctr_smp_flag) {
      for (ii in 1:outsidefolds) {
        print('Biomarker:')
        print(c(i, var_groups[[i]]))
        print('CV-repeat #')
        print(jj)
        print('CV-outside fold #')
        print(ii)
        print('CV-save counter #')
        print(ctr)
        
        #Segment data by fold using the which() function
        #this selects a portion of data as function of number of outerfolds, e.g.
        #4 outer folds means 1/4 or 25% of data is used for test, 75% for train
        testIndexes <- which(data.outerfolds == ii,arr.ind = TRUE)
        #cross validation only on train data for outerfolds
        of_crossval.train.data <- crossval.train.data[-testIndexes, ]
        of_crossval.train.data_ctrl <- crossval.train.data_ctrl[-testIndexes, ]
        
        #test on the portion left out for CV
        of_crossval.test.data <- crossval.train.data[testIndexes, ]  
        of_crossval.test.data_ctrl <- crossval.train.data_ctrl[testIndexes, ]  
      
        #Standard GLM
        StatModel <- glm(data = of_crossval.train.data, formula = outcome ~ .,
                         family = "binomial")
        
        StatModel_ctrl <- glm(data = of_crossval.train.data_ctrl, formula = outcome ~ .,
                         family = "binomial")
        
        #Bayes with flat prior
        StatModel_brm <- brm(outcome ~ .,
                             data = of_crossval.train.data,
                             family = bernoulli(), cores = 4,
                             threads = threading(2), #threads per core
                             backend = 'cmdstanr', silent = 2)

        StatModel_brm_ctrl <- brm(outcome ~ .,
                             data = of_crossval.train.data_ctrl,
                             family = bernoulli(), cores = 4,
                             threads = threading(2), #threads per core
                             backend = 'cmdstanr', silent = 2)
        
        #Bayes with horseshoe prior
        StatModel_brm_hs <- brm(outcome ~ .,
                                data = of_crossval.train.data,
                                prior = prior(horseshoe(), class = b),
                                family = bernoulli(), cores = 4,
                                threads = threading(2), #threads per core
                                backend = 'cmdstanr', silent = 2)

        StatModel_brm_hs_ctrl <- brm(outcome ~ .,
                                data = of_crossval.train.data_ctrl,
                                prior = prior(horseshoe(), class = b),
                                family = bernoulli(), cores = 4,
                                threads = threading(2), #threads per core
                                backend = 'cmdstanr', silent = 2)
        
        
        #save models for test with generalisation set
        StatModel_ls[[outsidefolds*(jj - 1) + i]] <- StatModel
        
        probabilities_of <- predict(object = StatModel, of_crossval.test.data, type = "response")
        probabilities_of_ctrl <- predict(object = StatModel_ctrl, of_crossval.test.data_ctrl, type = "response")
        
        probabilities_of_brm <- predict(object = StatModel_brm, of_crossval.test.data, type = "response")
        probabilities_of_brm_ctrl <- predict(object = StatModel_brm_ctrl, of_crossval.test.data_ctrl, type = "response")
        
        probabilities_of_brm_hs <- predict(object = StatModel_brm_hs, of_crossval.test.data, type = "response")
        probabilities_of_brm_hs_ctrl <- predict(object = StatModel_brm_hs_ctrl, of_crossval.test.data_ctrl, type = "response")
        
        # Test the best predicted lambda on the remaining data in the outer fold
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
        
        sensitivity_store[outsidefolds*(jj - 1) + ii] <- as.numeric(sensitivity(tab)['.estimate'])
        specificity_store[outsidefolds*(jj - 1) + ii] <- as.numeric(specificity(tab)['.estimate'])
        
        # save ROC curve
        roccurve[[outsidefolds*(jj - 1) + ii]] <- roc(outcome ~ c(probabilities_of), 
                                                      data = of_crossval.test.data)
        roccurve_ctrl[[outsidefolds*(jj - 1) + ii]] <- roc(outcome ~ c(probabilities_of_ctrl), 
                                                      data = of_crossval.test.data_ctrl)
        
        roccurve_brm[[outsidefolds*(jj - 1) + ii]] <- roc(outcome ~ c(probabilities_of_brm[,1]), 
                                                          data = of_crossval.test.data)
        roccurve_brm_ctrl[[outsidefolds*(jj - 1) + ii]] <- roc(outcome ~ c(probabilities_of_brm_ctrl[,1]), 
                                                          data = of_crossval.test.data_ctrl)
        

        roccurve_brm_hs[[outsidefolds*(jj - 1) + ii]] <- roc(outcome ~ c(probabilities_of_brm_hs[,1]), 
                                                             data = of_crossval.test.data)
        roccurve_brm_hs_ctrl[[outsidefolds*(jj - 1) + ii]] <- roc(outcome ~ c(probabilities_of_brm_hs_ctrl[,1]), 
                                                             data = of_crossval.test.data_ctrl)

                
        rocposcases[[outsidefolds*(jj - 1) + ii]] <- sum(of_crossval.test.data['outcome'])
        #plot.roc(roccurve[[1]])
        
        auc[outsidefolds*(jj - 1) + ii] <- auc(roccurve[[outsidefolds*(jj - 1) + ii]])
        auc_ctrl[outsidefolds*(jj - 1) + ii] <- auc(roccurve_ctrl[[outsidefolds*(jj - 1) + ii]])
        
        auc_brm[outsidefolds*(jj - 1) + ii] <- auc(roccurve_brm[[outsidefolds*(jj - 1) + ii]])
        auc_brm_ctrl[outsidefolds*(jj - 1) + ii] <- auc(roccurve_brm_ctrl[[outsidefolds*(jj - 1) + ii]])
        
        auc_brm_hs[outsidefolds*(jj - 1) + ii] <- auc(roccurve_brm_hs[[outsidefolds*(jj - 1) + ii]])
        auc_brm_hs_ctrl[outsidefolds*(jj - 1) + ii] <- auc(roccurve_brm_hs_ctrl[[outsidefolds*(jj - 1) + ii]])
        
        #Brier against train data
        out_brier_train <- BrierScore(StatModel)
        
        # Brier score against test data
        f_t <- predicted.classes_of
        o_t <- of_crossval.test.data$outcome
        brier[outsidefolds*(jj - 1) + ii] <- mean(((f_t) - o_t)^2)
        
        #save a record of the odds ratios
        stat_tmp <- summary(StatModel)$coeff
        
      } # for (ii in 1:outsidefolds) {
    } # if(!ctr_smp_flag){ can't do CV because of small sample diversity
  } #for (jj in 1:repeats) { begin CV

  print('save counter #')
  print(ctr)
  
  if (!ctr_smp_flag) { #normal sample size
    univar_batch_result_tb[ctr,'AUC CV CI 2.5'] <- sort(auc)[round(length(auc)*.025)]
    univar_batch_result_tb[ctr,'AUC CV CI 50'] <- sort(auc)[round(length(auc)*.5)]
    univar_batch_result_tb[ctr,'AUC CV CI 97.5'] <- sort(auc)[round(length(auc)*.975)]

    univar_batch_result_tb[ctr,'AUC CV CI 2.5'] <- sort(auc)[round(length(auc)*.025)]
    univar_batch_result_tb[ctr,'AUC CV CI 50'] <- sort(auc)[round(length(auc)*.5)]
    univar_batch_result_tb[ctr,'AUC CV CI 97.5'] <- sort(auc)[round(length(auc)*.975)]

    auc_difference = auc_ctrl - auc
    univar_batch_result_tb[ctr,'AUC Difference CV CI 2.5'] <- sort(auc_difference)[round(length(auc_difference)*.025)]
    univar_batch_result_tb[ctr,'AUC Difference CV CI 50'] <- sort(auc_difference)[round(length(auc_difference)*.5)]
    univar_batch_result_tb[ctr,'AUC Difference CV CI 97.5'] <- sort(auc_difference)[round(length(auc_difference)*.975)]
    
    univar_batch_result_tb[ctr,'Bayes AUC CV CI 2.5'] <- sort(auc_brm)[round(length(auc_brm)*.025)]
    univar_batch_result_tb[ctr,'Bayes AUC CV CI 50'] <- sort(auc_brm)[round(length(auc_brm)*.5)]
    univar_batch_result_tb[ctr,'Bayes AUC CV CI 97.5'] <- sort(auc_brm)[round(length(auc_brm)*.975)]
    
    auc_brm_difference = auc_brm_ctrl - auc_brm
    univar_batch_result_tb[ctr,'Bayes AUC Difference CV CI 2.5'] <- sort(auc_brm_difference)[round(length(auc_brm_difference)*.025)]
    univar_batch_result_tb[ctr,'Bayes AUC Difference CV CI 50'] <- sort(auc_brm_difference)[round(length(auc_brm_difference)*.5)]
    univar_batch_result_tb[ctr,'Bayes AUC Difference CV CI 97.5'] <- sort(auc_brm_difference)[round(length(auc_brm_difference)*.975)]
    
    univar_batch_result_tb[ctr,'Bayes HS AUC CV CI 2.5'] <- sort(auc_brm_hs)[round(length(auc_brm_hs)*.025)]
    univar_batch_result_tb[ctr,'Bayes HS AUC CV CI 50'] <- sort(auc_brm_hs)[round(length(auc_brm_hs)*.5)]
    univar_batch_result_tb[ctr,'Bayes HS AUC CV CI 97.5'] <- sort(auc_brm_hs)[round(length(auc_brm_hs)*.975)]
    
    auc_brm_hs_difference = auc_brm_hs_ctrl - auc_brm_hs
    univar_batch_result_tb[ctr,'Bayes HS AUC Difference CV CI 2.5'] <- sort(auc_brm_hs_difference)[round(length(auc_brm_hs_difference)*.025)]
    univar_batch_result_tb[ctr,'Bayes HS AUC Difference CV CI 50'] <- sort(auc_brm_hs_difference)[round(length(auc_brm_hs_difference)*.5)]
    univar_batch_result_tb[ctr,'Bayes HS AUC Difference CV CI 97.5'] <- sort(auc_brm_hs_difference)[round(length(auc_brm_hs_difference)*.975)]
    
    
  } else { # not enough samples for CV
    univar_batch_result_tb[ctr,'AUC CV CI 2.5'] <- NA
    univar_batch_result_tb[ctr,'AUC CV CI 50'] <- NA
    univar_batch_result_tb[ctr,'AUC CV CI 97.5'] <- NA
    
    univar_batch_result_tb[ctr,'AUC Difference CV CI 2.5'] <- NA
    univar_batch_result_tb[ctr,'AUC Difference CV CI 50'] <- NA
    univar_batch_result_tb[ctr,'AUC Difference CV CI 97.5'] <- NA
    
    univar_batch_result_tb[ctr,'Bayes AUC CV CI 2.5'] <- NA
    univar_batch_result_tb[ctr,'Bayes AUC CV CI 50'] <- NA
    univar_batch_result_tb[ctr,'Bayes AUC CV CI 97.5'] <- NA
    
    univar_batch_result_tb[ctr,'Bayes AUC Difference CV CI 2.5'] <- NA
    univar_batch_result_tb[ctr,'Bayes AUC Difference CV CI 50'] <- NA
    univar_batch_result_tb[ctr,'Bayes AUC Difference CV CI 97.5'] <- NA
    
    univar_batch_result_tb[ctr,'Bayes HS AUC CV CI 2.5'] <- NA
    univar_batch_result_tb[ctr,'Bayes HS AUC CV CI 50'] <- NA
    univar_batch_result_tb[ctr,'Bayes HS AUC CV CI 97.5'] <- NA
    
    univar_batch_result_tb[ctr,'Bayes HS AUC Difference CV CI 2.5'] <- NA
    univar_batch_result_tb[ctr,'Bayes HS AUC Difference CV CI 50'] <- NA
    univar_batch_result_tb[ctr,'Bayes HS AUC Difference CV CI 97.5'] <- NA
    
  }  

  return(univar_batch_result_tb)

} # univar_batch_result_tb_PAR <- foreach(i = 1:(length(var_groups)), .combine = "rbind") %dopar% {


model_desc_str = paste('_R',as.character(repeats),'_OF', 
                       as.character(outsidefolds), sep = '')

print("Saving results tables...")
write.csv(univar_batch_result_tb_PAR, 
          paste(save_path, model_desc_str,bio_fn,'.csv', sep = ''), 
          row.names = FALSE)

stopCluster(cl) # close clusters
