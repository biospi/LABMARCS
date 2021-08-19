
# Model 1 GLM & # Model 2 STEP_AIC
mod_ls <- c('1_FullGLM','2_FullStepAIC')

for (i in c(1,2)) {
  
  print(paste("Run",mod_ls[i],"model"))
  
  # Run model with/without Firth's bias
  if (IncludeFirthBias == 0) {
    StatModel_Full <- glm(outcome ~.,data = train.data, family = "binomial")
    # Initial model using all input parameters without Firth's bias
    StatModel <- StatModel_Full #rename to generic for loop
    if (i == 2) {
      StatModel_StepAIC <- stepAIC(StatModel_Full,trace = TRUE)
      StatModel <- StatModel_StepAIC}
  } else {
    # Method below utilises the Firth's bias approach
    # Note: modified-scores approach (pl=False) or maximum penalized likelihood (pl=True)
    model <- glm(outcome ~ .,
                 data = train.data, family = "binomial", method = "firthglm.fit")
  }
  
  sink(paste(save_path, 'Model_',mod_ls[i],'_summary.txt',sep = '')) #open text file to review later
  # Summarize the final selected model
  print('Model Summary')
  print(summary(StatModel))
  
  # Return p-values (unadjusted)
  print('Unadjusted p-values')
  print(coef(summary(StatModel))[,4])
  
  # Adjust the p-values using benjamini hochberg method
  adjustedp <- p.adjust(coef(summary(StatModel))[,4], method = "fdr",
                        n = length(coef(summary(StatModel))[,4]))
  # Return p-values (adjusted)
  print('B-H corrected p-values')
  print(adjustedp)
  
  # Examine odds ratios and 95% CIs
  print('Var/Covar Matrix')
  print(vcov(StatModel))
  print('Odds Ratios')
  print(exp(StatModel$coefficients))
  print('Odds Ratios CIs')
  print(exp(confint.default(StatModel)))
  
  # Model predictions on test set (prob of >0.5 accepted as positive)
  probabilities <- predict(object = StatModel, test.data, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  # Model accuracy
  print('Model Accuracy')
  out_acc <- mean(predicted.classes == test.data$outcome)
  print(out_acc)
  
  # Sensitivity and specificity measures
  conf_matrix <- table(predicted.classes,test.data$outcome)
  colnames(conf_matrix) = c(0,1)
  
  # find AUC and plot ROC curve
  pdf(paste(save_path, 'Model_',mod_ls[i],'_ROC.pdf',sep = ''))
  g <- roc(outcome ~ probabilities, data = test.data)
  plot(g)
  dev.off()
  
  print('AUC')
  out_auc <- auc(g)
  print(out_auc)
  
  print('Sensitivity')
  out_sen <- as.numeric(sensitivity(conf_matrix)['.estimate'])
  print(out_sen)
  
  print('Specificity')
  out_spec <- as.numeric(specificity(conf_matrix)['.estimate'])
  out_spec <- print(out_spec)
  
  sink()
  
  m_ctr <- m_ctr + 1
  #save things to our summary datatable
  batch_df[m_ctr,] <- c(mod_ls[i], readingwanted_str, dateRange, outcome_str,
                        out_acc, out_auc, out_brier, out_sen, out_spec)
}
