#ProjectivePrediction_Perf.R
y = pp_data$outcome

pred <- proj_predict(proj, newdata = pp_data, nterms = nv)

# Visualise the projected most relevant variables
mcmc_areas(as.matrix(proj), , prob = 0.95, prob_outer = 1) + coord_cartesian(xlim = c(-3, 3))


ggplot() + geom_point(aes(x = colMeans(pred), y = y)) +
  labs(x = "train prediction", y = "y")
pr <- as.integer(colMeans(pred) >= 0.5) #prediction from model

# posterior classification accuracy
out_acc <- mean(pr == y)

# posterior balanced classification accuracy 1/2*(TP/P + TN/N)
#better to report when data set doesn't have near 50/50 of pos/neg
TP_P = sum( y & pr)/sum(y)
TN_N = sum(!y & !pr)/sum(!y)
bayes_train_acc_2 = (TP_P + TN_N)/2

# save ROC curve
roccurve <- roc(outcome ~ c( colMeans(pred)), data = pp_data)
out_auc <- auc(roccurve)

ggroc(roccurve, legacy.axes = T) +
  geom_abline(slope = 1 ,intercept = 0) + # add identity line
  theme(
    panel.background = element_blank(), 
    axis.title.x = element_text(size = 18, face = 'bold'),
    axis.title.y = element_text(size = 18, face = 'bold'),
    panel.border = element_rect(size = 2, fill = NA), 
    axis.text.x = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(size = 14, face = 'bold')) +
  xlab('1 - Specificity') +
  ylab('Sensitivity') +
  scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) + 
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  geom_text(x = 0.1, y = 1, colour = "black", size = 6,
            label = paste('AUC: ', sprintf("%0.2f",auc1), sep = '') )

ggsave(paste(save_path, pp_str, SelectedData_str,'_ROC.pdf', sep = ''),
       device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#Accuracy	AUC	Brier	Sensitivity	Specificity

sink(paste(save_path, pp_str, SelectedData_str,'_Summary.csv', sep = ''))

print('Odds Ratios')
print(posterior_summary(proj))

#AUC
print('AUC')
print(out_auc)

# Model accuracy
print('Accuracy')
print(out_acc)

conf_matrix <- table(pr,y)
colnames(conf_matrix) <- c(0,1) #rename so sensitivity & specificity functions work

print('Sensitivity')
out_sen <- sensitivity(conf_matrix)
out_sen <- out_sen[,'.estimate'][[1]]
print(out_sen)

print('Specificity')
out_spec <- specificity(conf_matrix)
out_spec <- out_spec[,'.estimate'][[1]]
print(out_spec)

print('Brier Score')
#This function only works form glm objects need to manually calculate
#out_brier <- BrierScore(StatModel) 
out_brier = mean(((pr) - y)^2)
print(out_brier)
sink()

glm_traindata_batch_df9[m_ctr,] <- c('ProjPred', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)

write.table(glm_traindata_batch_df9, file = paste(output_path, pp_str, SelectedData_str, 
                                                  'Batch_ProjPred_Summary_Table.csv',
                                                  sep = ''), 
            row.names = FALSE, sep = ',')



