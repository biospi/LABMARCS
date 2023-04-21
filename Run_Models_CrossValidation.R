# Setup stores for CV performance measures
n_comparisons = 7 #glm, lasso, bayes-flat, bayes-hs, lasso inspired glm, projpred
auc_matrix_train = matrix(data = NA, n_models, n_comparisons)
auc_matrix_gen = matrix(data = NA, n_models, n_comparisons)
auc_matrix_train_diff = matrix(data = NA, n_models, n_comparisons)
auc_matrix_train_diff2_5 = matrix(data = NA, 1, n_comparisons)
auc_matrix_train_diff50 = matrix(data = NA, 1, n_comparisons)
auc_matrix_train_diff97_5 = matrix(data = NA, 1, n_comparisons)

auc_matrix_gen_diff = matrix(data = NA, n_models, n_comparisons)
auc_matrix_gen_diff2_5 = matrix(data = NA, 1, n_comparisons)
auc_matrix_gen_diff50 = matrix(data = NA, 1, n_comparisons)
auc_matrix_gen_diff97_5 = matrix(data = NA, 1, n_comparisons)

sensitivity_matrix_train = matrix(data = NA, n_models, n_comparisons)
sensitivity_matrix_gen = matrix(data = NA, n_models, n_comparisons)
sensitivity_matrix_train_diff = matrix(data = NA, n_models, n_comparisons)
sensitivity_matrix_gen_diff = matrix(data = NA, n_models, n_comparisons)

specificity90_matrix_train = matrix(data = NA, n_models, n_comparisons)
specificity90_matrix_gen = matrix(data = NA, n_models, n_comparisons)
specificity90_matrix_train_diff = matrix(data = NA, n_models, n_comparisons)
specificity90_matrix_gen_diff = matrix(data = NA, n_models, n_comparisons)

specificity95_matrix_train = matrix(data = NA, n_models, n_comparisons)
specificity95_matrix_gen = matrix(data = NA, n_models, n_comparisons)
specificity95_matrix_train_diff = matrix(data = NA, n_models, n_comparisons)
specificity95_matrix_gen_diff = matrix(data = NA, n_models, n_comparisons)



#--------------------------------------------------------------------------------------
##---- Standard GLM Models ------
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
#INTERNAL VALIDATION (Train on training set and test on training set)
#Do GLM cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%

generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified avicw  
cv_desc = ''
p_str <- 'T_GLM_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_CVTestData_', sep = '')
lasso_run = FALSE
print('Run GLM with CrossVal Train only UHB/NBT 80/20 split...')
source(paste(work_path,'CrossValidate_GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df1[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3],
                          specificity90_spec_quantile[1],
                          specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                          specificity95_spec_quantile[1],
                          specificity95_spec_quantile[2],specificity95_spec_quantile[3]) 

write.table(cv_batch_df1, file = paste(output_path, 
                                       'GLM_CV_Train_Summary_Compendium.csv',
                                       sep = ''),
            row.names = FALSE, sep = ',')

#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 1
auc_matrix_train[,cnum] = auc_store
sensitivity_matrix_train[,cnum] = sensitivity_store 
specificity90_matrix_train[,cnum] = spec90_mat[3,]
specificity95_matrix_train[,cnum] = spec95_mat[3,]



#--------------------------------------------------------------------------------------
# GENERALISATION
# Do GLM cross validation training on (1 - 1/outsidefolds) but test on excluded dataset

crossval.test.data <- test.data
generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified above
p_str <- 'G_GLM_' ##(G)eneralise, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_GeneraliseData_', sep = '')
lasso_run = FALSE
print('Run GLM with CrossVal Train UHB/NBT 80/20 split, but test generalisation Weston')
source(paste(work_path,'CrossValidate_GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df2[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3],
                          specificity90_spec_quantile[1],
                          specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                          specificity95_spec_quantile[1],
                          specificity95_spec_quantile[2],specificity95_spec_quantile[3]) 

write.table(cv_batch_df2, file = paste(output_path, 
                                       'GLM_CV_Generalise_Summary_Compendium.csv'
                                       ,sep = ''),
            row.names = FALSE, sep = ',')


#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 1
auc_matrix_gen[,cnum] = auc_store
sensitivity_matrix_gen[,cnum] = sensitivity_store 
specificity90_matrix_gen[,cnum] = spec90_mat[3,]
specificity95_matrix_gen[,cnum] = spec95_mat[3,]






#--------------------------------------------------------------------------------------
# LASSO MODEL
#--------------------------------------------------------------------------------------

#INTERNAL VALIDATION (Train and test on training data with CV split)
#--------------------------------------------------------------------------------------
#Do Lasso cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
#crossval.train.data <- train.data
#don't need to specify crossval.test.data as it will be a % from crossval.train.data
generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified  
p_str <- 'T_LASSO_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_CVTestData_', sep = '')

print('Run LASSO with CrossVal Train only UHB/NBT 80/20 split...')
source(paste(work_path,'CrossValidate_LASSO_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df3[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3],
                          specificity90_spec_quantile[1],
                          specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                          specificity95_spec_quantile[1],
                          specificity95_spec_quantile[2],specificity95_spec_quantile[3])  

write.table(cv_batch_df3, file = paste(output_path, 'LASSO_CV_Train_Summary_Compendium.csv',sep = ''),
            row.names = FALSE, sep = ',')

#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 2
auc_matrix_train[,cnum] = auc_store
sensitivity_matrix_train[,cnum] = sensitivity_store 
specificity90_matrix_train[,cnum] = spec90_mat[3,]
specificity95_matrix_train[,cnum] = spec95_mat[3,]






if (m_ctr == 1) { #first time create table
  cv_batch_varlist_df1 <- data.frame(matrix(data = NA, nrow = 1, 
                                            ncol = 4 + length(varnames[[1]])))
  colnames(cv_batch_varlist_df1)[1:4] <- c('ModelType', 'Reading', 'Day', 'Outcome')
  colnames(cv_batch_varlist_df1)[5:dim(cv_batch_varlist_df1)[2]] <- varnames[[1]]
} 

#save variable frequency selection 
cv_batch_varlist_df1[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                                  varcount/(n_models)*100)

write.table(cv_batch_varlist_df1, file = paste(output_path, 
                                               'LASSO_CV_Variable_Selection_Compendium.csv',
                                               sep = ''), row.names = FALSE, sep = ',')

#Find which variables in LOO are non-zero at least 50% of the time
equal_above_50 = (varcount/(n_models)*100) >= 50
vname = varnames[[1]]

#save lasso names
lasso_var_ls = vname[equal_above_50]

#LASSO output appends superfluous TRUE, refine so we can save the list for later
for (i in seq(1, length(lasso_var_ls))) {
  if (grepl("TRUE", lasso_var_ls[i] , fixed = TRUE)) {
    #remove redundant true
    lasso_var_ls[i] = substr(lasso_var_ls[i], 1, nchar(lasso_var_ls[i])-4)
  }
}

#remove intercept as this is auto created
lasso_var_ls = lasso_var_ls[2:length(lasso_var_ls)]
#add outcome so models can use 
lasso_var_ls[length(lasso_var_ls) + 1] = "outcome"

sink(paste(save_path ,cv_desc, '_LASSO_VarNames.txt',sep = ''))
print(lasso_var_ls)
sink()


#--------------------------------------------------------------------------------------
#EXTERNAL VALIDATION (Train on training data and test with validation set)  
#--------------------------------------------------------------------------------------
#Do Lasso cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
#crossval.train.data <- train.data
crossval.test.data <- test.data
generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified 

p_str <- 'G_LASSO_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_GeneraliseData_', sep = '')

print('Run LASSO with CrossVal Train UHB/NBT 80/20 split, but test generalisation Weston')
source(paste(work_path,'CrossValidate_LASSO_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df4[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3],
                          specificity90_spec_quantile[1],
                          specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                          specificity95_spec_quantile[1],
                          specificity95_spec_quantile[2],specificity95_spec_quantile[3]) 

write.table(cv_batch_df4, file = paste(output_path, 
                                       'LASSO_CV_Generalise_Summary_Compendium.csv',
                                       sep = ''),
            row.names = FALSE, sep = ',')


#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 2
auc_matrix_gen[,cnum] = auc_store
sensitivity_matrix_gen[,cnum] = sensitivity_store 
specificity90_matrix_gen[,cnum] = spec90_mat[3,]
specificity95_matrix_gen[,cnum] = spec95_mat[3,]









#if (TRUE) { #run bayesian cross val - can take several hours due to model compilation}

  #--------------------------------------------------------------------------------------
  #----- Bayesian Logistic Regression-------------------
  #--------------------------------------------------------------------------------------
  
  
  
  #-----------------FLAT PRIOR----------------------------
  #INTERNAL VALIDATION Training Data performance
  generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified 
  
  p_str <- 'T_BAYES_' #(T)rain, text prefix for roc curve compendium variables
  cv_desc = paste(p_str , 'Train_CVTrainData_Test_TrainData_', sep = '')
  
  prior_type = 'Flat'
  print('Run Flat BAYES with CrossVal Train UHB/NBT 80/20 split, test on Train')
  source(paste(work_path,'CrossValidate_BAYES_On_Selected_Data.R', sep = ''))
  
  #save things to our summary data table
  cv_batch_df5[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                            median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                            median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                            median(lambda.store), lambda.store_quantile[1],
                            lambda.store_quantile[2],lambda.store_quantile[3],
                            median(sensitivity_store),sensitivity_quantile[1],
                            sensitivity_quantile[2],sensitivity_quantile[3], 
                            median(specificity_store),specificity_quantile[1],
                            specificity_quantile[2],specificity_quantile[3],
                            specificity90_spec_quantile[1],
                            specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                            specificity95_spec_quantile[1],
                            specificity95_spec_quantile[2],specificity95_spec_quantile[3])  
  
  write.table(cv_batch_df5, file = paste(output_path, 
                                         'Flat_BAYES_CV_Train_Summary_Compendium.csv',
                                         sep = ''),
              row.names = FALSE, sep = ',')
  
  #save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
  cnum = 3
  auc_matrix_train[,cnum] = auc_store
  sensitivity_matrix_train[,cnum] = sensitivity_store 
  specificity90_matrix_train[,cnum] = spec90_mat[3,]
  specificity95_matrix_train[,cnum] = spec95_mat[3,]
  

  #-----Generalise Performance
  #crossval.train.data <- train.data
  crossval.test.data <- test.data
  generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified 
  
  p_str <- 'G_BAYES_' #(T)rain, text prefix for roc curve compendium variables
  cv_desc = paste(p_str , 'Train_CVTrainData_Test_GeneraliseData_', sep = '')
  
  prior_type = 'Flat'
  print('Run Flat BAYES with CrossVal Train UHB/NBT 80/20 split, test on Generalise')
  source(paste(work_path,'CrossValidate_BAYES_On_Selected_Data.R', sep = ''))
  
  #save things to our summary data table
  cv_batch_df6[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                            median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                            median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                            median(lambda.store), lambda.store_quantile[1],
                            lambda.store_quantile[2],lambda.store_quantile[3],
                            median(sensitivity_store),sensitivity_quantile[1],
                            sensitivity_quantile[2],sensitivity_quantile[3], 
                            median(specificity_store),specificity_quantile[1],
                            specificity_quantile[2],specificity_quantile[3],
                            specificity90_spec_quantile[1],
                            specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                            specificity95_spec_quantile[1],
                            specificity95_spec_quantile[2],specificity95_spec_quantile[3])  
  
  write.table(cv_batch_df6, file = paste(output_path, 
                                         'Flat_BAYES_CV_Generalise_Summary_Compendium.csv',
                                         sep = ''),
              row.names = FALSE, sep = ',')
  
  #save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
  cnum = 3
  auc_matrix_gen[,cnum] = auc_store
  sensitivity_matrix_gen[,cnum] = sensitivity_store 
  specificity90_matrix_gen[,cnum] = spec90_mat[3,]
  specificity95_matrix_gen[,cnum] = spec95_mat[3,]
  
  
  
  
  
  
  #----------------------------HORSE SHOE ----------------------------
  
  #INTERNAL VALIDATION Training Data performance
  #crossval.train.data <- train.data
  crossval.test.data <- train.data
  generalise_flag <- 0 # if == 1, do not test CV with 20% held out, instead test on specified 
  
  p_str <- 'T_BAYES_HS' #(T)rain, text prefix for roc curve compendium variables
  cv_desc = paste(p_str , 'Train_CVTrainData_Test_TrainData_', sep = '')
  
  print('Run HS BAYES with CrossVal Train UHB/NBT 80/20 split, test on Train')
  prior_type = 'Horseshoe'
  source(paste(work_path,'CrossValidate_BAYES_On_Selected_Data.R', sep = ''))

  # Projective prediction below relies on an initial bayesian model from BRMS, we can
  # save the models here and apply ProjPred variable selection and projection later
  BAYES_HS_StatModel_ls = StatModel_ls
  
  #save things to our summary data table
  cv_batch_df7[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                            median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                            median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                            median(lambda.store), lambda.store_quantile[1],
                            lambda.store_quantile[2],lambda.store_quantile[3],
                            median(sensitivity_store),sensitivity_quantile[1],
                            sensitivity_quantile[2],sensitivity_quantile[3], 
                            median(specificity_store),specificity_quantile[1],
                            specificity_quantile[2],specificity_quantile[3],
                            specificity90_spec_quantile[1],
                            specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                            specificity95_spec_quantile[1],
                            specificity95_spec_quantile[2],specificity95_spec_quantile[3])  
  
  write.table(cv_batch_df7, file = paste(output_path, 
                                         'HS_BAYES_CV_Train_Summary_Compendium.csv',
                                         sep = ''),
              row.names = FALSE, sep = ',')
  
  #save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
  cnum = 4
  auc_matrix_train[,cnum] = auc_store
  sensitivity_matrix_train[,cnum] = sensitivity_store 
  specificity90_matrix_train[,cnum] = spec90_mat[3,]
  specificity95_matrix_train[,cnum] = spec95_mat[3,]
  

  
  
  
  #-----Generalise Performance
  #crossval.train.data <- train.data
  crossval.test.data <- test.data
  generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified 
  
  p_str <- 'G_BAYES_HS_' #(T)rain, text prefix for roc curve compendium variables
  cv_desc = paste(p_str , 'Train_CVTrainData_Test_GeneraliseData_', sep = '')
  
  prior_type = 'Horseshoe'
  print('Run HS BAYES with CrossVal Train UHB/NBT 80/20 split, test on Generalise')
  source(paste(work_path,'CrossValidate_BAYES_On_Selected_Data.R', sep = ''))

  
  #save things to our summary data table
  cv_batch_df8[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                            median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                            median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                            median(lambda.store), lambda.store_quantile[1],
                            lambda.store_quantile[2],lambda.store_quantile[3],
                            median(sensitivity_store),sensitivity_quantile[1],
                            sensitivity_quantile[2],sensitivity_quantile[3], 
                            median(specificity_store),specificity_quantile[1],
                            specificity_quantile[2],specificity_quantile[3],
                            specificity90_spec_quantile[1],
                            specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                            specificity95_spec_quantile[1],
                            specificity95_spec_quantile[2],specificity95_spec_quantile[3])  
  
  write.table(cv_batch_df8, file = paste(output_path, 
                                         'HS_BAYES_CV_Generalise_Summary_Compendium.csv',
                                         sep = ''),
              row.names = FALSE, sep = ',')
  
  #save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
  cnum = 4
  auc_matrix_gen[,cnum] = auc_store
  sensitivity_matrix_gen[,cnum] = sensitivity_store 
  specificity90_matrix_gen[,cnum] = spec90_mat[3,]
  specificity95_matrix_gen[,cnum] = spec95_mat[3,]
  
  

  #-----------------------------------------------------------------------------------------
#} else { print('Skip Bayesian CV...')}







#-----------------------------------------------------------------------------------------
#REDUCED VARIABLE MODELS
#-----------------------------------------------------------------------------------------

#this seems to be loaded or saved from elsewhere reintialise so it is empty
StatModel_ls <- vector(mode = "list", length = n_models)

#Evaluate LASSO Inspired Model
#--------------------------------------------------------------------------------------
#INTERNAL VALIDATION (Train on training set and test on training set)
#Do GLM cross validation training on (1 - 1/outsidefolds )*100, e.g. train 75% test 25%
#crossval.train.data <- train.data[, lasso_var_ls]
lasso_run = TRUE
#crossval.train.data <- # don't need to specify crossval.test.data as it will be a % from crossval.train.data
generalise_flag = 0 # if == 1, do not test CV with 20% held out, instead test on specified avicw  

p_str <- 'T_LASSO_REDUCE_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_CVTestData_', sep = '')

print('Run GLM (LASSO selected reduced vars) CrossVal. Train/Test only UHB/NBT 80/20 split...')

source(paste(work_path,'CrossValidate_GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df9[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3],
                          specificity90_spec_quantile[1],
                          specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                          specificity95_spec_quantile[1],
                          specificity95_spec_quantile[2],specificity95_spec_quantile[3]) 

write.table(cv_batch_df9, file = paste(output_path, 
                                       'GLM_LASSO_REDUCED_CV_Train_Summary_Compendium.csv',
                                       sep = ''),
            row.names = FALSE, sep = ',')

#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 5
auc_matrix_train[,cnum] = auc_store
sensitivity_matrix_train[,cnum] = sensitivity_store 
specificity90_matrix_train[,cnum] = spec90_mat[3,]
specificity95_matrix_train[,cnum] = spec95_mat[3,]




#--------------------------------------------------------------------------------------


#Do GLM cross validation training on (1 - 1/outsidefolds) but test on excluded dataset
#for generalisation
#crossval.train.data <- train.data[, lasso_var_ls]
lasso_run = TRUE
crossval.test.data <- test.data[, lasso_var_ls]
generalise_flag <- 1 # if == 1, do not test CV with 20% held out, instead test on specified above

p_str <- 'G_LASSO_REDUCE_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_GeneraliseData_', sep = '')

print('Run GLM (LASSO selected reduced vars) with CrossVal Train UHB/NBT 80/20 split, but test generalisation Weston')
source(paste(work_path,'CrossValidate_GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df10[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                          median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                          median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                          median(lambda.store), lambda.store_quantile[1],
                          lambda.store_quantile[2],lambda.store_quantile[3],
                          median(sensitivity_store),sensitivity_quantile[1],
                          sensitivity_quantile[2],sensitivity_quantile[3], 
                          median(specificity_store),specificity_quantile[1],
                          specificity_quantile[2],specificity_quantile[3],
                          specificity90_spec_quantile[1],
                          specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                          specificity95_spec_quantile[1],
                          specificity95_spec_quantile[2],specificity95_spec_quantile[3]) 

write.table(cv_batch_df10, file = paste(output_path, 
                                       'GLM_LASSO_REDUCED_CV_Generalise_Summary_Compendium.csv'
                                       ,sep = ''),
            row.names = FALSE, sep = ',')

#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 5
auc_matrix_gen[,cnum] = auc_store
sensitivity_matrix_gen[,cnum] = sensitivity_store 
specificity90_matrix_gen[,cnum] = spec90_mat[3,]
specificity95_matrix_gen[,cnum] = spec95_mat[3,]



#--------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------

###--------------------- 
# PROJECTIVE PREDICTION
###---------------------

#--------------------------------------------------------------------------------------
# FULL VARIABLE PROJPRED MODEL (HORSE SHOE PRIOR)
# INTERNAL VALIDATION (Training Data performance)
#--------------------------------------------------------------------------------------
# SLOW!!!!!!!
# Using naive variable selection speeds up results (default)
# LOO is much slower

crossval.test.data = train.data
varselect_flag = TRUE
generalise_flag = FALSE # if == 1, do not test CV with 20% held out, instead test on specified 

#we do not need to recompute the BRMS fits (generalise flag is a slight misnomer as
# it really means recompute models (which we don't need in generalisation either)
# we can use StatModel_ls from the ones in BAYES_HS_StatModel_ls

p_str <- 'T_PP_FULL_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_TrainData_', sep = '')

#if testing all variables set solution tems list to null
solterms_ls = NULL
nt_max = 70

print('Run full ProjPred with CrossVal Train UHB/NBT 80/20 split, test on Train')
prior_type = 'Horseshoe'
source(paste(work_path,'CrossValidate_ProjPred_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df11[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                           median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                           median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                           median(lambda.store), lambda.store_quantile[1],
                           lambda.store_quantile[2],lambda.store_quantile[3],
                           median(sensitivity_store),sensitivity_quantile[1],
                           sensitivity_quantile[2],sensitivity_quantile[3], 
                           median(specificity_store),specificity_quantile[1],
                           specificity_quantile[2],specificity_quantile[3],
                           specificity90_spec_quantile[1],
                           specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                           specificity95_spec_quantile[1],
                           specificity95_spec_quantile[2],specificity95_spec_quantile[3])  

write.table(cv_batch_df11, file = paste(output_path, 
                                        'ProjPred_FULL_CV_Train_Summary_Compendium.csv',
                                        sep = ''),
            row.names = FALSE, sep = ',')

#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 6
auc_matrix_train[,cnum] = auc_store
sensitivity_matrix_train[,cnum] = sensitivity_store 
specificity90_matrix_train[,cnum] = spec90_mat[3,]
specificity95_matrix_train[,cnum] = spec95_mat[3,]


#--------------------------------------------------------------------------------------
# GENERALISATION
# FULL VARIABLE PROJPRED MODEL (HORSE SHOE PRIOR)
#--------------------------------------------------------------------------------------

crossval.test.data <- test.data
varselect_flag = FALSE
generalise_flag <- TRUE # if == 1, do not test CV with 20% held out, instead test on specified 

p_str <- 'T_PP_FULL_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_GeneraliseData_', sep = '')

print('Run full ProjPred with CrossVal Train UHB/NBT 80/20 split, test on Train')
prior_type = 'Horseshoe'
source(paste(work_path,'CrossValidate_ProjPred_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df12[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                           median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                           median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                           median(lambda.store), lambda.store_quantile[1],
                           lambda.store_quantile[2],lambda.store_quantile[3],
                           median(sensitivity_store),sensitivity_quantile[1],
                           sensitivity_quantile[2],sensitivity_quantile[3], 
                           median(specificity_store),specificity_quantile[1],
                           specificity_quantile[2],specificity_quantile[3],
                           specificity90_spec_quantile[1],
                           specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                           specificity95_spec_quantile[1],
                           specificity95_spec_quantile[2],specificity95_spec_quantile[3])  

write.table(cv_batch_df12, file = paste(output_path, 
                                        'ProjPred_FULL_CV_Generalise_Summary_Compendium.csv',
                                        sep = ''),
            row.names = FALSE, sep = ',')


#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 6
auc_matrix_gen[,cnum] = auc_store
sensitivity_matrix_gen[,cnum] = sensitivity_store 
specificity90_matrix_gen[,cnum] = spec90_mat[3,]
specificity95_matrix_gen[,cnum] = spec95_mat[3,]


#-------------------------------------------------------
# REDUCED VARIABLE PROJPRED MODEL 
#-------------------------------------------------------
# INTERNAL VALIDATION Training Data performance

crossval.test.data <- train.data
#this will reuse the models from above as we do not need to recompute whole model
#just need to reproject the smaller version using solterms_ls
varselect_flag = FALSE
generalise_flag <- TRUE # if == 1, do not test CV with 20% held out, instead test on specified 

p_str <- 'T_PP_REDUCE_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_TrainData_', sep = '')

#limited set of terms selected from running CV varsel LOO on entire 590 training set
#this is a bit unconventional as each 80/20 split might come up with a different set
#of variables, so this is more of a test of the performance across 80/20 splits than
#mimcking the LASSO varibale frequency

solterms_ls = c( "UreaAbnormal", "UreaNA", ##AUC=0.62
                 "Age", #AUC=0.64
                 "PTAbnormal","PTNA", ##AUC=0.68
                 "NLRMild","NLRModerate","NLRSevere","NLRNA" #AUC =  G 0.70  // I 0.75 **       
)
nt_max = length(solterms_ls)

print('Run small ProjPred with CrossVal Train UHB/NBT 80/20 split, test on Train')
prior_type = 'Horseshoe'
source(paste(work_path,'CrossValidate_ProjPred_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df13[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                           median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                           median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                           median(lambda.store), lambda.store_quantile[1],
                           lambda.store_quantile[2],lambda.store_quantile[3],
                           median(sensitivity_store),sensitivity_quantile[1],
                           sensitivity_quantile[2],sensitivity_quantile[3], 
                           median(specificity_store),specificity_quantile[1],
                           specificity_quantile[2],specificity_quantile[3],
                           specificity90_spec_quantile[1],
                           specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                           specificity95_spec_quantile[1],
                           specificity95_spec_quantile[2],specificity95_spec_quantile[3])  

write.table(cv_batch_df13, file = paste(output_path, 
                                        'ProjPred_REDUCE_CV_Train_Summary_Compendium.csv',
                                        sep = ''),
            row.names = FALSE, sep = ',')

#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 7
auc_matrix_train[,cnum] = auc_store
sensitivity_matrix_train[,cnum] = sensitivity_store 
specificity90_matrix_train[,cnum] = spec90_mat[3,]
specificity95_matrix_train[,cnum] = spec95_mat[3,]



#--------------------------------------------------------------------------------------
# GENERALISE TEST
# REDUCED VARIABLE PROJPRED 

crossval.test.data <- test.data
varselect_flag = FALSE
generalise_flag <- TRUE # if == 1, do not test CV with 20% held out, instead test on specified 

p_str <- 'T_PP_REDUCE_' #(T)rain, text prefix for roc curve compendium variables
cv_desc = paste(p_str , 'Train_CVTrainData_Test_GeneraliseData_', sep = '')

print('Run small ProjPred with CrossVal Train UHB/NBT 80/20 split, test on Train')
prior_type = 'Horseshoe'
source(paste(work_path,'CrossValidate_ProjPred_On_Selected_Data.R', sep = ''))

#save things to our summary data table
cv_batch_df14[m_ctr,] <- c(mnum, readingwanted_str, dateRange, outcome_str,
                           median(auc_store), auc_quantile[1],auc_quantile[2],auc_quantile[3],
                           median(brier), brier_quantile[1],brier_quantile[2],brier_quantile[3],
                           median(lambda.store), lambda.store_quantile[1],
                           lambda.store_quantile[2],lambda.store_quantile[3],
                           median(sensitivity_store),sensitivity_quantile[1],
                           sensitivity_quantile[2],sensitivity_quantile[3], 
                           median(specificity_store),specificity_quantile[1],
                           specificity_quantile[2],specificity_quantile[3],
                           specificity90_spec_quantile[1],
                           specificity90_spec_quantile[2],specificity90_spec_quantile[3],
                           specificity95_spec_quantile[1],
                           specificity95_spec_quantile[2],specificity95_spec_quantile[3])  

write.table(cv_batch_df14, file = paste(output_path, 
                                        'ProjPred_REDUCE_CV_Generalise_Summary_Compendium.csv',
                                        sep = ''),
            row.names = FALSE, sep = ',')

#save AUC, sensitivty, spec (@ 0.9 and 0.95 sensitivity) for delta calculation
cnum = 7
auc_matrix_gen[,cnum] = auc_store
sensitivity_matrix_gen[,cnum] = sensitivity_store 
specificity90_matrix_gen[,cnum] = spec90_mat[3,]
specificity95_matrix_gen[,cnum] = spec95_mat[3,]
#---------------------------------------------------------------------------------






#---------------------------------------------------------------------------------

sink(paste(save_path ,cv_desc, '_AUC_Difference_table.csv',sep = ''))

#prepare table with distribution of differences between models (using GLM as standard)
for (i in seq(1,n_comparisons)) {
  auc_matrix_train_diff[,i] = auc_matrix_train[,i]-auc_matrix_train[,4]
  auc_matrix_train_diff2_5[i] = quantile(auc_matrix_train_diff[,i],0.025, na.rm = TRUE)
  auc_matrix_train_diff50[i] = quantile(auc_matrix_train_diff[,i],0.5, na.rm = TRUE)
  auc_matrix_train_diff97_5[i] = quantile(auc_matrix_train_diff[,i],0.975, na.rm = TRUE)
  s = sprintf('%8.2f [%8.2f,%8.2f]',auc_matrix_train_diff50[i],auc_matrix_train_diff2_5[i],auc_matrix_train_diff97_5[i])
  print(s)
}

for (i in seq(1,n_comparisons)) {
  auc_matrix_gen_diff[,i] = auc_matrix_gen[,i]-auc_matrix_gen[,4]
  auc_matrix_gen_diff2_5[i] = quantile(auc_matrix_gen_diff[,i],0.025, na.rm = TRUE)
  auc_matrix_gen_diff50[i] = quantile(auc_matrix_gen_diff[,i],0.5, na.rm = TRUE)
  auc_matrix_gen_diff97_5[i] = quantile(auc_matrix_gen_diff[,i],0.975, na.rm = TRUE)
  s = sprintf('%8.2f [%8.2f,%8.2f]',auc_matrix_gen_diff50[i],auc_matrix_gen_diff2_5[i],auc_matrix_gen_diff97_5[i])
  print(s)
}

sink()


#create table with quartiles

