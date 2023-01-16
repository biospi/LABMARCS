#--- TRAIN ON FULL DATASET ---
#Train on full data and test on full data (disregard train/test groups)
#Run a GLM on all n=843 data - our benchmark for best performance (but not for generalise)
SelectedData <- fulldata
SelectedDataOutcome <- fulldata
SelectedData_str <- paste('GLM_TrainFullData_TestFullData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM with full data test/train...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_fulldata_batch_df[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                   out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_fulldata_batch_df, file = paste(output_path,
                                                'Batch_GLM_TrainFullData_TestFullData_Summary_Table.csv',
                                                sep = ''),
            row.names = FALSE, sep = ',')

#Internal Validation  
# TRAIN ON TRAINING SET AND TEST ON TRAINING SET
#Run a GLM only on train.data (UHB,NBT) and test on train.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- train.data
SelectedData_str <- paste('GLM_Train_TrainData_Test_TrainData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM only on train data for test/train...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df1[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df1, file = paste(output_path,
                                                  'Batch_GLM_Train_TrainData_Test_TrainData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')


#External Validation
# TRAIN ON TRAINING SET AND TEST ON VALIDATION SET
#Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- test.data
SelectedData_str <- paste('GLM_Train_TrainData_Test_GeneraliseData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run GLM, train on UHB/NBT, test on Weston...')
source(paste(work_path,'GLM_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df2[m_ctr,] <- c('GLM', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df2, file = paste(output_path,
                                                  'Batch_GLM_Train_TrainData_Test_GeneraliseData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')
