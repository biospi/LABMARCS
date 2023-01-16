#-------------------------------------------------------------------------------
#----------- Bayesian Models-------------------------------------------------------
#-------------------------------------------------------------------------------


#prep data
tr_backup = train.data
test_backup = test.data

names(tr_backup) = str_replace_all(names(tr_backup),"_","")
names(test_backup) = str_replace_all(names(test_backup),"_","")

#recast as numeric?
for (i in c(1:dim(tr_backup)[2])) { #skip #2 age
  tr_backup[,i] <- as.numeric(tr_backup[,i])
}

for (i in c(1:dim(test_backup)[2])) { #skip #2 age
  test_backup[,i] <- as.numeric(test_backup[,i])
}


###FLAT PRIOR
# Note the flat prior has difficulty converging takes ~30 minutes with 2000 iterations
# Horseshoe converges on same data in ~5 minutes w/ 10K iterations

#Internal Validation  
# TRAIN ON TRAINING SET AND TEST ON TRAINING SET
#Run a Bayesian GLM only on train.data (UHB,NBT) and test on train.data (Weston)
SelectedData <- tr_backup
SelectedDataOutcome <- tr_backup
SelectedData_str <- paste('BAYES_FLAT_Train_TrainData_Test_TrainData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Fit Flat BAYES to train data and test on train data...')
prior_type = 'Flat'
model_training = 1 # need to train model
source(paste(work_path,'BAYES_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df5[m_ctr,] <- c('BAYES', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df5, file = paste(output_path,
                                                  'Batch_BAYES_Flat_Train_TrainData_Test_TrainData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')


#External Validation
# TRAIN ON TRAINING SET AND TEST ON VALIDATION SET
#Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
SelectedData <- tr_backup
SelectedDataOutcome <- test_backup
SelectedData_str <- paste('BAYES_FLAT_Train_TrainData_Test_GeneraliseData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run Flat BAYES fitted model on validation...')
prior_type = 'Flat'
model_training = 0 # can re-use model from above
source(paste(work_path,'BAYES_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df6[m_ctr,] <- c('BAYES', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df6, file = paste(output_path,
                                                  'Batch_BAYES_Flat_Train_TrainData_Test_GeneraliseData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')




###Horse shoe PRIOR

#Internal Validation  
# TRAIN ON TRAINING SET AND TEST ON TRAINING SET
#Run a Bayesian GLM only on train.data (UHB,NBT) and test on train.data (Weston)
SelectedData <- tr_backup
SelectedDataOutcome <- tr_backup
SelectedData_str <- paste('BAYES_HS_Train_TrainData_Test_TrainData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Fit HS BAYES to train data and test on train data...')
prior_type = 'Horseshoe'
model_training = 1 # need to train model
source(paste(work_path,'BAYES_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df7[m_ctr,] <- c('BAYES', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df7, file = paste(output_path,
                                                  'Batch_BAYES_HS_Train_TrainData_Test_TrainData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')


#External Validation
# TRAIN ON TRAINING SET AND TEST ON VALIDATION SET
#Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
SelectedData <- tr_backup
SelectedDataOutcome <- test_backup
SelectedData_str <- paste('BAYES_HS_Train_TrainData_Test_GeneraliseData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run HS BAYES fitted model on validation...')
prior_type = 'Horseshoe'
model_training = 0 # need to train model
source(paste(work_path,'BAYES_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df8[m_ctr,] <- c('BAYES', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df8, file = paste(output_path,
                                                  'Batch_BAYES_HS_Train_TrainData_Test_GeneraliseData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')

