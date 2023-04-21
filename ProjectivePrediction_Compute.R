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

if (use_precomputed) {
  
  #Note LOO for the full set of variables is memory intense and needs >16GB RAM
  #it's easier to run on BioSpi save the RData file and then load here
  
  load(paste(work_path, fn, sep = ''))
  
  
  #vs = cv_vs
  #note if you use another compute make sure the strings in work, output & save_path
  #match else you'll get errors, so you may need to update here after loading e.g.
  #work_path <- 'C:/Users/bs16044/OneDrive - University of Bristol/NewPathName/'
  #output_path <- paste(work_path, 'output/', sep = '')
  #save_path = paste(output_path, 'Day-',dateRange,'_Reading-',readingwanted_str,
  #                  '_Outcome-',outcome_str, '_Imputation-',imputation_str, '_',sep = '')
  
} else {

  
  names(tr_backup) = str_replace_all(names(tr_backup),"_","")
  names(test_backup) = str_replace_all(names(test_backup),"_","")
  
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
  for (ii in c(1:dim(tr_backup)[2])) { #skip #2 age
    tr_backup[,ii] <- as.numeric(tr_backup[,ii])
  }
  
  for (ii in c(1:dim(test_backup)[2])) { #skip #2 age
    test_backup[,ii] <- as.numeric(test_backup[,ii])
  }
  
  
  #no need to recompute as it was done during bayesian CV section
  #fit hs prior version - params set to get convergence - reference model
  #brmfit <- brm(outcome ~ .,
  #              data = tr_backup, #train.data,
  #              family = bernoulli(),
  #              prior = prior(horseshoe(), class = b),
  #              cores = parallel::detectCores(),
  #              backend = 'cmdstanr',
  #              iter = 2000,
  #              control = list(adapt_delta = 0.999, 
  #                             step_size = 0.01, 
  #                             max_treedepth = 15),
  #              silent = 2)
  #
  
  #refmodel <- get_refmodel(brmfit)
  refmodel <- get_refmodel(StatModel)
  
  #validate_search = FALSE variable selection only once on full data set
  #TRUE is default and does var selection every time (but otder presented in out put is
  #for all data)
  
  
  if (cv_style == 'Naive') {#NO cross validation - takes 6-7 minutes
    print('naive variable selection...')
    N_k = 0
    vs_simple = varsel(refmodel, nterms_max = nt_max, validate_search = vs_flag, search_terms = solterms_ls)
    vs = vs_simple

  } else if (cv_style == 'KFold') {
    #nterms_max uses too much memory if >50? takes 15? minutes
    print('kfold variable selection...')
    N_k = 5
    kf_vs = cv_varsel(refmodel, cv_method = 'kfold', nterms_max = nt_max, 
                      K = N_k, validate_search = vs_flag, search_terms = solterms_ls)
    vs = kf_vs
    
  } else if (cv_style == 'LOO') {
    #NOTE!! cv_varsel is computationally intense can take 8+ hours on ~500 patients data
    print('LOO variable selection...')
    N_k = 0
    cv_vs = cv_varsel(refmodel, cv_method = 'loo', nterms_max = nt_max, 
                      validate_search = vs_flag, search_terms = solterms_ls)
    vs = cv_vs
  }

}