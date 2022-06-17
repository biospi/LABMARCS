#After getting confidence intervals for cross validated Bayesian models. We
#want to train our final model on all of the training data. Once we have this
#model we can then apply projective prediction to rank variables according to
#their contribution. This final set of variables can then be used to
#run a final GLM/Bayes model using only those variables 

#k-fold variable selection can't deal with 1/0 needs to be T/F
#for (j in seq(1,dim(train.data)[2])){
#  train.data[,j] = as.logical(train.data[,j])
#  test.data[,j] = as.logical(test.data[,j])
#}
#above doesn't fix... error is Error: New factor levels are not allowed.
#Levels allowed: 'FALSE', 'TRUE' , Levels found: '0', '1'

names(train.data) = str_replace_all(names(train.data),"_","")
names(test.data) = str_replace_all(names(test.data),"_","")

# Horseshoe prior from BRMS vignette
#n <- nrow(train.data) # 100
#D <- ncol(train.data[, -1]) # 20
#p0 <- 10 # prior guess for the number of relevant variables
#tau0 <- p0/(D-p0) * 1/sqrt(n) # scale for tau (notice that stan_glm will automatically scale this by sigma)
#prior=prior(horseshoe(scale_global = tau0, scale_slab = 1), class=b),

# In practice, at least 4 chains should be 
# used and 2000 iterations might be required for reliable inference.
# seed=1, chains=4, iter=2000) #try larger value than 2000 to get r_hat close to <1.05

#Train final bayesian model using all of the training data

#recast as numeric?
tr_backup <- train.data
for (i in c(1:dim(tr_backup)[2])) { #skip #2 age
  tr_backup[,i] <- as.numeric(tr_backup[,i])
}

test_backup <- test.data
for (i in c(1:dim(test_backup)[2])) { #skip #2 age
  test_backup[,i] <- as.numeric(test_backup[,i])
}

#using flat prior
# brmfit <- brm(outcome ~ .,
#               data = tr_backup, #train.data,
#               family = bernoulli(),
#               cores = parallel::detectCores(),
#               backend = 'cmdstanr',
#               iter = 10000,
#               control = list(adapt_delta = 0.999, 
#                              step_size = 0.01, 
#                              max_treedepth = 15))

#using cmdstanr backend
brmfit <- brm(outcome ~ .,
              data = tr_backup, #train.data,
              family = bernoulli(),
              prior = prior(horseshoe(), class = b),
              cores = parallel::detectCores(),
              backend = 'cmdstanr',
              iter = 10000,
              control = list(adapt_delta = 0.999, 
                             step_size = 0.01, 
                             max_treedepth = 15))

#print some general summary on coeffients and their CIs
sink(paste(save_path,'BAYES_Train_AllTrainData_FitSummary.txt',sep = ''))
summary(brmfit)
print('Convert to log odds via exp()')
exp(fixef(brmfit))
sink()

#using default backend rstan (?)
#brmfit <- brm(outcome ~ .,
#              data = tr_backup,
#              family = bernoulli(),
#              prior = prior(horseshoe(), class = b),
#              refresh = 0)

# plot(fit2) <run when exmaining manually else ruins script due to interactivity
mcmc_plot(brmfit)
refmodel <- get_refmodel(brmfit)

#posterior predictive check
pp_check(refmodel$fit, ndraws = 500, alpha = 0.1)

loo_reference_fit <- loo(refmodel$fit)
loo_reference_fit
plot(loo_reference_fit)

pred <- predict(object = refmodel, tr_backup, type = "response")
roccurve1 <- roc(outcome ~ pred, data = tr_backup)
auc1 <- auc(roccurve1)

ggroc(roccurve1, legacy.axes = T) +
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

SelectedData_str ='Train_FullTrainData_Test_TrainData'
ggsave(paste(save_path, 'Model_BAYES_', SelectedData_str,'_ROC.pdf', sep = ''),
       device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)



pred <- predict(object = refmodel, test_backup, type = "response")
roccurve2 <- roc(outcome ~ pred, data = test_backup)
auc2 <- auc(roccurve2)

ggroc(roccurve2, legacy.axes = T) +
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
            label = paste('AUC: ', sprintf("%0.2f",auc2), sep = '') )

SelectedData_str = 'Train_FullTrainData_Test_TestData'
ggsave(paste(save_path, 'Model_BAYES_', SelectedData_str,'_ROC.pdf', sep = ''),
       device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)


#-------------------------------------
#PROJECTIVE PREDICTION evaluate reduced model

#NO cross validation - takes 6-7 minutes
if (1) {
  b_style = 'Naive'
  nt_max = 70
  N_k = 0
  vs_simple = varsel(refmodel, nterms_max = nt_max)
  vs = vs_simple
}

if (0) {
  b_style = 'KFold'
  nt_max = 20
  N_k = 15
  #nterms_max uses too much memory if >50? takes 15? minutes
  kf_vs = cv_varsel(refmodel, cv_method = 'kfold', nterms_max = nt_max, K = N_k)
  vs = kf_vs
}

if (0) {#NOTE!! cv_varsel is computationally intense can take 8+ hours on ~500 patients data
  b_style = 'LOO'
  nt_max = 20
  N_k = 0
  cv_vs = cv_varsel(refmodel, cv_method = 'loo', nterms_max = nt_max)
  vs = cv_vs
}

#validate_search = FALSE variable selection only once on full data set
#TRUE is default and does var selection every time (but otder presented in out put is
#for all data)

sink(paste(save_path, b_style, '_MaxTerms', nt_max, '_K', N_k, 'Bayes-varSelect-Summary.txt',sep = ''))
#examine variable selection results
refmodel$fit
exp(fixef(refmodel$fit))
solution_terms(vs)
summary(vs, stats = c('auc', 'elpd'))
sink()

plot(vs, stats = c('auc', 'elpd'))
ggsave(paste(save_path, b_style, '_MaxTerms', nt_max, '_K', N_k, 'Bayes-varSelect-AUC-ELPD.pdf',sep = ''), device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)







proj_summary = summary(vs, stats = c('auc', 'elpd'))

#nv <- suggest_size(vs,alpha = 0.2) #= 80% <-NA?
nv=25
# Visualise the most relevant variables in the full model -->
mcmc_areas(as.matrix(refmodel$fit),
           pars = c("b_Intercept", paste0("b_", solution_terms(vs)[1:nv]))) +
  coord_cartesian(xlim = c(-3, 3)) 

#EVALUATE REDUCED VARIABLE MODEL
#number of terms to use #default alpha is 0.32 (68%) so variables in 1SD
#proj <- project(vs, nterms = nv ,seed = 123456,ns = 2000)

#Instead of taking top N variables cluster biomarker tests together
solterms = c( "Age", "GenderF",
                    "UreaAbnormal", "UreaNA",
                    #"poctLACNA",
                    #"O2NA"     ,
                    #"CO2NA"            [17] "CO2Abnormal"       
                    "PTAbnormal","PTNA",
                    "NLRMild","NLRModerate","NLRSevere","NLRNA",         
                    #"LDHNA"            
                    "poctpHAbnormal","poctpHNA",
                    "LymphocytesMild","LymphocytesModerate","LymphocytesSevere","LymphocytesNA", 
                    "APTTMild", "APTTModerate", "APTTNA"                  
                    "eGFRAbnormal", "eGFRNA"      
                    "NeutrophilsMild",  "NeutrophilsModerate",  "NeutrophilsSevere",  "NeutrophilsNA",  
                    #"FERNA"
                    #"fibNA"            
                    #"CRPAbnormal"       
                    #"DDMAbnormal"      
                    )

proj <- project(vs, solution_terms = solterms,ns = 2000)

# Visualise the projected most relevant variables
posterior_summary(proj)
mcmc_areas(as.matrix(proj), , prob = 0.95, prob_outer = 1) + coord_cartesian(xlim = c(-3, 3))

#We make predictions with the projected submodels. For point estimates we can use method 
#proj_predict. Test inputs can be provided using the keyword newdata. 
#It also the test targets ynew are provided in newdata, then the function evaluates
#the log predictive density at these points. For instance, the following computes 
#the mean of the predictive distribution and evaluates the log density at the training 
#points using the most relevant variables.

#eval internal training data performance
pred <- proj_predict(vs, newdata = tr_backup, nterms = nv)
y = train.data$outcome
ggplot() + geom_point(aes(x = colMeans(pred), y = y)) +
  labs(x = "train prediction", y = "y")
pr <- as.integer(colMeans(pred) >= 0.5)
# posterior classification accuracy
bayes_train_acc <- mean(xor(pr,as.integer(y == 0)))
# posterior balanced classification accuracy
bayes_train_acc <- (mean(xor(pr[y == 0] > 0.5,as.integer(y[y == 0]))) +
                      mean(xor(pr[y == 1] < 0.5,as.integer(y[y == 1]))))/2
# save ROC curve
roccurve1 <- roc(outcome ~ c( colMeans(pred)), data = tr_backup)
auc1 <- auc(roccurve1)

ggroc(roccurve2, legacy.axes = T) +
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

SelectedData_str ='Train_FullTrainData_Test_TrainData'
ggsave(paste(save_path, 'Model_BAYES_REDUCED', SelectedData_str,'_ROC.pdf', sep = ''),
       device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#how to get CIs without running variable selection each time? This runs reduced
#var model on 80% slices of data but this isn't right - underestimates variance
#auc_smp=vector()
#for (i in 1:100) {
#  sidx=sample(1:round(nrow(train.data)*0.8))
#  pred <- proj_predict(vs, newdata = train.data[sidx, ], nterms = nv)
#  roccurve1 <- roc(outcome ~ c( colMeans(pred)), data =  train.data[sidx, ])
#  auc1 <- auc(roccurve1)
#  auc_smp[i] = auc1
#}

#eval external validation data performance
pred <- proj_predict(vs, newdata = test_backup, nterms = nv)
y = test.data$outcome
ggplot() + geom_point(aes(x = colMeans(pred), y = y)) +
  labs(x = "test prediction", y = "y")
pr <- as.integer(colMeans(pred) >= 0.5)
# posterior classification accuracy
bayes_test_acc <- mean(xor(pr,as.integer(y == 0)))
# posterior balanced classification accuracy
bayes_test_acc <- (mean(xor(pr[y == 0] > 0.5,as.integer(y[y == 0]))) +
                     mean(xor(pr[y == 1] < 0.5,as.integer(y[y == 1]))))/2

# save ROC curve
roccurve2 <- roc(outcome ~ c( colMeans(pred)), data = test_backup)
auc2 <- auc(roccurve2)

ggroc(roccurve2, legacy.axes = T) +
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
            label = paste('AUC: ', sprintf("%0.2f",auc2), sep = '') )

SelectedData_str ='Train_FullTrainData_Test_TestData'
ggsave(paste(save_path, 'Model_BAYES_REDUCED', SelectedData_str,'_ROC.pdf', sep = ''),
       device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#-----Test Bayesian Logistic Regression-------------------
