#--- TRAIN ON FULL DATASET ---
#Train on full data and test on full data (disregard train/test groups)
#Run a GLM on all n=843 data - our benchmark for best performance (but not for generalise)
SelectedData <- fulldata[, lasso_var_ls]
SelectedDataOutcome <- fulldata[, lasso_var_ls]
SelectedData_str <- paste('GLM_via_LASSO_TrainFullData_TestFullData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM via LASSO with full data test/train...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_fulldata_batch_df[m_ctr,] <- c('GLM_via_LASSO', readingwanted_str, dateRange, outcome_str,
                                   out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_fulldata_batch_df, file = paste(output_path,
                                                'Batch_GLM_via_LASSO_TrainFullData_TestFullData_Summary_Table.csv',
                                                sep = ''),
            row.names = FALSE, sep = ',')

#Internal Validation  
# TRAIN ON TRAINING SET AND TEST ON TRAINING SET
#Run a GLM only on train.data (UHB,NBT) and test on train.data (Weston)
SelectedData <- train.data[, lasso_var_ls]
SelectedDataOutcome <- train.data[, lasso_var_ls]
SelectedData_str <- paste('GLM_via_LASSO_Train_TrainData_Test_TrainData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM via LASSO only on train data for test/train...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df1[m_ctr,] <- c('GLM_via_LASSO', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df1, file = paste(output_path,
                                                  'Batch_GLM_via_LASSO_Train_TrainData_Test_TrainData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')


#External Validation
# TRAIN ON TRAINING SET AND TEST ON VALIDATION SET
#Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
SelectedData <- train.data[, lasso_var_ls]
SelectedDataOutcome <- test.data[, lasso_var_ls]
SelectedData_str <- paste('GLM_via_LASSO_Train_TrainData_Test_GeneraliseData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM via LASSO, train on UHB/NBT, test on Weston...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df2[m_ctr,] <- c('GLM_via_LASSO', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df2, file = paste(output_path,
                                                  'Batch_GLM_via_LASSO_Train_TrainData_Test_GeneraliseData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')
