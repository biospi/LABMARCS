
#-----------------------------------------------------------------------------
#RUN GLM ON ALL DATA - OUR BENCHMARK FOR BEST PERFORMANCE
x <- model.matrix(outcome~., data = SelectedData)[,-1]
idx <- 1:(dim(SelectedData)[2] - 1) #omit outcome column
  
cv.lasso <- cv.glmnet(x,SelectedData$outcome, alpha = 1, 
                      data = SelectedData[,idx],
                      nfolds = insidefolds,
                      family = "binomial")
StatModel <- glmnet(x, SelectedData$outcome, alpha = 1, 
                    family = "binomial",
                    lambda = cv.lasso$lambda.min) #Optimal lambda 

#Setup prediction set on test data
x.test <- model.matrix(outcome ~., SelectedDataOutcome)[,-1]
probabilities <- StatModel %>% predict(newx = x.test, type = "response")

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
print('Model Coefficients Odds')
modelcoefs <- exp(coef(StatModel))
print(exp(modelcoefs))
print('Model Coefficients CIs')
#print(exp(confint.default(StatModel)))
print('N/A')

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
#This function only works form glm objects need to manually calculate
#out_brier <- BrierScore(StatModel) 
f_t <- predicted.classes
o_t <- SelectedDataOutcome$outcome
out_brier = mean(((f_t) - o_t)^2)
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

ggsave(paste(save_path, 'LASSO_', SelectedData_str,'_ROC.pdf', sep = ''),device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#-----------------------------------------------------------------------------
