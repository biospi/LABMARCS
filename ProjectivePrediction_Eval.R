#Run this script after BRM fit and running cv_varsel to find reduced variable ranking

#------------------------------------------------------------------------
#Now we evaluate the PROJECTIVE PREDICTION model - we first build a projection

#1.The model is evaluated first using ALL 69 dummy variables in cv_varsel with LOO. 
#The reduced version is done at end of script

#2.We then examine these variables and create list of reduced variable candidates where
#we remove any variables associated with a biomarker if the '_NA' category has the highest ranking
#for improving AUC

#3 We fit a second BRM model using this list of reduced variables
#with all conisdered variables, then we
#------------------------------------------------------------------------

#helper functions for understanding var sel results
#summary.vsel(), print.vsel(), plot.vsel(), suggest_size.vsel(), solution_terms.vsel()

cv_freq=vs$pct_solution_terms_cv
if (!is.null(cv_freq)) {
  cv_freq = cv_freq[, 2:dim(cv_freq)[2] ] #remove size column
}


if (!exists('b_style')) { b_style = 'NA' }

sink(paste(save_path, b_style, '_MaxTerms', nt_max, '_K', N_k, 'Bayes-varSelect-Summary.txt',sep = ''))
#examine variable selection results
print('LOG ODDS')
print(refmodel$fit)
print('ODDS')
print(exp(fixef(refmodel$fit)))
print('VARIABLE SELECTION RESULT')
print(vs)
out=summary(vs, stats = 'elpd', type = c("mean", "se", "lower", "upper", "diff", "diff.se"))
print('ELPD SUMMARY')
print(out)
print('ELPD DETAILED SELECTION')
print(out$selection)
out=summary(vs, stats = 'auc', type = c("mean", "se", "lower", "upper", "diff", "diff.se"))
print('AUC SUMMARY')
print(out)
print('AUC DETAILED SELECTION')
print(out$selection)
#should have frequencies of variables selected
print('VARIABLE SELECTION LOO FREQUENCY')
print(cv_freq)
sink()

if (!is.null(cv_freq)) {
  #flips it upside down so urea is at the top
  cv_freq2 = apply(cv_freq,2,rev)
  colnames(cv_freq2) = rev(colnames(cv_freq))
  dmelt = melt(cv_freq2)
  colnames(dmelt) = c('Rank','Biomarker','value')
  ggplot(dmelt, aes(x = Rank,
                     y = Biomarker,
                     fill = value)) + geom_tile()
  ggsave(paste(save_path, b_style, '_MaxTerms', nt_max, '_K', N_k, 'Bayes_VarSelect_FrequencyHeatmap.pdf',sep = ''), device = 'pdf',
         width = 20, height = 20, units = 'cm', dpi = 300)
}

plot(vs, stats = c('auc', 'elpd'))
ggsave(paste(save_path, b_style, '_MaxTerms', nt_max, '_K', N_k, 'Bayes-varSelect-AUC-ELPD.pdf',sep = ''), device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

#number of terms to use #default alpha is 0.32 (68%) so variables in 1SD
#nv <- dim(cv_freq)[2] # suggest_size(vs,alpha = 0.2) #0.2 maps to 80
#Note sometimes suggest_size returns NA even when everything else goes fine, bug?

# Visualise the most relevant variables in the full model -->
mcmc_areas(as.matrix(refmodel$fit),
           pars = c("b_Intercept", paste0("b_", solution_terms(vs)))) +
  coord_cartesian(xlim = c(-3, 3)) 
ggsave(paste(save_path, b_style, '_MaxTerms', nt_max, '_K', N_k, 'Bayes-MCMC_ReducedVar.pdf',sep = ''), device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)






#------------------------------------------------------------------------------
#We make predictions with the projected submodels. For point estimates we can use method 
#proj_predict. Test inputs can be provided using the keyword newdata. 
#It also the test targets ynew are provided in newdata, then the function evaluates
#the log predictive density at these points. For instance, the following computes 
#the mean of the predictive distribution and evaluates the log density at the training 
#points using the most relevant variables.

nv = length(vs$solution_terms)

pp_str = paste('ProjPred_V',nv, '_', sep = '')
proj <- project(vs, nterms = nv ,seed = 123456,ns = 2000)

#---------------------------------------------
#eval internal training data performance
SelectedData_str ='Train_FullTrainData_Test_TrainData'
pp_data = tr_backup
source(paste(work_path, 'ProjectivePrediction_Perf.R', sep = ''))
glm_traindata_batch_df9[m_ctr,] <- c('ProjPred', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)

write.table(glm_traindata_batch_df9, file = paste(output_path, pp_str, SelectedData_str, 
                                                  'Batch_ProjPred_Summary_Table.csv',
                                                  sep = ''), 
            row.names = FALSE, sep = ',')

#---------------------------------------------
#Now eval external validation data performance
SelectedData_str ='Train_FullTrainData_Test_Generalise'
pp_data = test_backup
source(paste(work_path, 'ProjectivePrediction_Perf.R', sep = ''))
glm_traindata_batch_df10[m_ctr,] <- c('ProjPred', readingwanted_str, dateRange, outcome_str,
                                     out_acc,out_auc, out_brier, out_sen, out_spec)

write.table(glm_traindata_batch_df10, file = paste(output_path, pp_str, SelectedData_str, 
                                                  'Batch_ProjPred_Summary_Table.csv',
                                                  sep = ''), 
            row.names = FALSE, sep = ',')


#---------------------------------------------
#----Clinician advised variables to select
#To be able to select these we need to run variable selection with all the variables (~70)
# several NAs as the top contributor in improving AUC - how to handle?

#AUD=G 0.71 / I AUC=0.84  with all variables
solterms = c(       "UreaAbnormal", "UreaNA", ##AUC=0.62
                    #"poctLACNA" #NA first ranked for biomarker
                    #"O2NA"     #NA first ranked for biomarker
                    #"CO2NA"  #NA first ranked for biomarker
                    "Age", #AUC=0.64
                    "PTAbnormal","PTNA", ##AUC=0.68
                    "NLRMild","NLRModerate","NLRSevere","NLRNA" #AUC =  G 0.70  // I 0.75 **       
                    #"LDHNA" #NA first ranked for biomarker            
                    #"poctpHAbnormal","poctpHNA", #AUC=0.70**
                    #"LymphocytesMild","LymphocytesModerate","LymphocytesSevere","LymphocytesNA", #AUC= I 0.76/ G 0.70**
                    #"APTTMild", "APTTModerate", "APTTNA", #AUC=0.68
                    #"eGFRAbnormal", "eGFRNA",      #.68
                    #"NeutrophilsMild", "NeutrophilsModerate", "NeutrophilsSevere", "NeutrophilsNA", #AUC= I 0.77 / G 0.69
                    #"FERNA" #NA first ranked for biomarker
                    #"fibNA" #NA first ranked for biomarker            
                    #"CRPAbnormal", "CRPNA", #AUC=0.69       
                    #"DDMAbnormal", "DDMNA",  #AUC=0.69
                    #"HBSevere", "HBModerate", "HBMild", "HBNA", #AUC=0.69
                    #'WCCSevere', "WCCModerate", "WCCMild", "WCCNA", #AUC=0.69
                    #'PLTSevere', "PLTModerate", "PLTMild", "PLTCNA" #AUC G 0.70 // AUC I 0.80 **
)


#We make predictions with the projected submodels. For point estimates we can use method 
#proj_predict. Test inputs can be provided using the keyword newdata. 
#It also the test targets ynew are provided in newdata, then the function evaluates
#the log predictive density at these points. For instance, the following computes 
#the mean of the predictive distribution and evaluates the log density at the training 
#points using the most relevant variables.
nv <- length(solterms)
pp_str = paste('ProjPred_V',nv, '_', sep = '')
proj <- project(vs, solution_terms = solterms,ns = 2000)

#---------------------------------------------
#eval internal training data performance
SelectedData_str ='Train_FullTrainData_Test_TrainData'
pp_data = tr_backup
source(paste(work_path, 'ProjectivePrediction_Perf.R', sep = ''))
glm_traindata_batch_df11[m_ctr,] <- c('ProjPred', readingwanted_str, dateRange, outcome_str,
                                      out_acc,out_auc, out_brier, out_sen, out_spec)

write.table(glm_traindata_batch_df11, file = paste(output_path, pp_str, SelectedData_str, 
                                                   'Batch_ProjPred_Summary_Table.csv',
                                                   sep = ''), 
            row.names = FALSE, sep = ',')

#---------------------------------------------
#Now eval external validation data performance
SelectedData_str ='Train_FullTrainData_Test_Generalise'
pp_data = test_backup
source(paste(work_path, 'ProjectivePrediction_Perf.R', sep = ''))
glm_traindata_batch_df12[m_ctr,] <- c('ProjPred', readingwanted_str, dateRange, outcome_str,
                                      out_acc,out_auc, out_brier, out_sen, out_spec)

write.table(glm_traindata_batch_df12, file = paste(output_path, pp_str, SelectedData_str, 
                                                   'Batch_ProjPred_Summary_Table.csv',
                                                   sep = ''), 
            row.names = FALSE, sep = ',')


#Get the model coefficients 
beta_store <- matrix(data = NA, nrow = nv, ncol = length(proj$submodl))
alpha_store <- matrix(data = NA, nrow = 1, ncol = length(proj$submodl))
for (xcx in seq(1,length(proj$submodl))) {
  beta_store[,xcx] = exp(proj$submodl[[xcx]]$beta)
  alpha_store[,xcx] = exp(proj$submodl[[xcx]]$alpha)
  
}
rownames(beta_store) <- rownames(proj$submodl[[1]]$beta)

#get coefficient and upper/lower bound
c2_5 = round(dim(beta_store)[2]*0.025)
c50 = round(dim(beta_store)[2]*0.5)
c97_5 = round(dim(beta_store)[2]*0.975)
beta_coef_store <- matrix(nrow = nv, ncol = 3)
rownames(beta_coef_store) <- rownames(proj$submodl[[1]]$beta)

for (xcx in seq(1,dim(beta_store)[1])) {
  tmp = sort(beta_store[xcx,])
  beta_coef_store[xcx,1] = tmp[c2_5]
  beta_coef_store[xcx,2] = tmp[c50]
  beta_coef_store[xcx,3] = tmp[c97_5]
}

tmp = sort(alpha_store)
alpha_coef_store = matrix()
alpha_coef_store[1] = tmp[c2_5]
alpha_coef_store[2] = tmp[c50]
alpha_coef_store[3] = tmp[c97_5]

sink(paste(save_path, '_PP_Projection_Coefficients.csv',sep = ''))
print('Predictor Variables')
print(beta_coef_store)
print('Intercept')
print(alpha_coef_store)
sink()

