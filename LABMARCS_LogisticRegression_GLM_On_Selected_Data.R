
#-----------------------------------------------------------------------------
#RUN GLM ON ALL DATA - OUR BENCHMARK FOR BEST PERFORMANCE
StatModel <- glm(outcome ~.,data = SelectedData, family = "binomial")
probabilities <- predict(object = StatModel, SelectedDataOutcome, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
conf_matrix <- table(predicted.classes,SelectedDataOutcome$outcome)
colnames(conf_matrix) <- c(0,1) #rename so sensitivity & specificity functions work

sink(paste(save_path,'GLM_Summary', SelectedData_str,'.txt', sep = ''))

# Summary
print('Full Model Summary')
print(summary(StatModel))

print('Events per Variable')
print(eventsnumber/ (dim(SelectedData)[2] - 1) )

# Examine odds ratios and 95% CIs
print('Full Model Coefficients Log-Odds')
print(exp(StatModel$coefficients))
print('Full Model Coefficients CIs')
print(exp(confint.default(StatModel)))

# Model accuracy
print('Accuracy')
out_acc <- mean(predicted.classes == SelectedDataOutcome$outcome)
print(out_acc)

print('Sensitivity')
out_sen <- sensitivity(conf_matrix)
out_sen <- out_sen[,'.estimate'][[1]]
print(out_sen)

print('Specificity')
out_spec <- specificity(conf_matrix)
out_spec <- out_spec[,'.estimate'][[1]]
print(out_spec)

print('Brier Score')
out_brier <- BrierScore(StatModel)
print(out_brier)

# Plot ROC curve and find AUC
roccurve3 <- roc(outcome ~ c(probabilities), data = SelectedDataOutcome)
out_auc <- auc(roccurve3)
out_auc <- out_auc[1]
print('AUC')
print(out_auc)

sink()

ggroc(roccurve3, legacy.axes = T) +
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
            label = paste('AUC: ', sprintf("%0.2f",out_auc), sep = '') )

ggsave(paste(save_path, 'Model_GLM_', SelectedData_str,'_ROC.pdf', sep = ''),device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#-----------------------------------------------------------------------------
