#-------------------------------------------------------------------------------
#----------- LASSO Models-------------------------------------------------------
#-------------------------------------------------------------------------------
#Internal Validation  
# TRAIN ON TRAINING SET AND TEST ON TRAINING SET
#Run a LASSO GLM only on train.data (UHB,NBT) and test on train.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- train.data
SelectedData_str <- paste('LASSO_Train_TrainData_Test_TrainData_N', 
                          as.character(dim(SelectedData)[1]) , sep = '')
print('Run LASSO only on train data for test/train...')
source(paste(work_path,'LASSO_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df3[m_ctr,] <- c('LASSO', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df3, file = paste(output_path,
                                                  'Batch_LASSO_Train_TrainData_Test_TrainData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')


#External Validation
# TRAIN ON TRAINING SET AND TEST ON VALIDATION SET
#Run a GLM only on train.data (UHB,NBT) and test on test.data (Weston)
SelectedData <- train.data
SelectedDataOutcome <- test.data
SelectedData_str <- paste('LASSO_Train_TrainData_Test_GeneraliseData_N', as.character(dim(SelectedData)[1]) , sep = '')
print('Run LASSO, train on UHB/NBT, test on Weston...')
source(paste(work_path,'LASSO_On_Selected_Data.R', sep = ''))

#save things to our summary data table
glm_traindata_batch_df4[m_ctr,] <- c('LASSO', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)
write.table(glm_traindata_batch_df4, file = paste(output_path,
                                                  'Batch_LASSO_Train_TrainData_Test_GeneraliseData_Summary_Table.csv',
                                                  sep = ''),
            row.names = FALSE, sep = ',')
