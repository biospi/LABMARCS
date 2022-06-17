# Convert string "NA" to actual NA
is.na(fulldata)
# Impute missing values using MICE (Multiple Imputation by Chained Equations)
# Only looking at eGFR,WCC,Neutrophils,Lymphocytes,NLR,Hb,Platelets (PLT),CRP for time being
pdf(paste(save_path, 'mice_plot_before_imputation.pdf',sep = ''))

mice_plot <- aggr(fulldata, col = c("green","red","yellow"),
                  numbers = TRUE, sortVars = TRUE,
                  labels = names(fulldata), cex.axis = 0.7,
                  gap = 3, ylab = c("Missing data","Pattern"))
dev.off()

#Description of MICE algorithm
#https://onlinelibrary.wiley.com/doi/full/10.1002/mpr.329
n_imputations <- 5
imputed_train <- mice(train.data, m = n_imputations)#, seed = 107)
imputed_test <- mice(test.data, m = n_imputations)#, seed = 107)
imputed_full <- mice(fulldata, m = n_imputations)#, seed = 107)

#from observation only these below need to be imputed
imp_strs = c('CRP_val','eGFR_val','HB_val','PLT_val','Lymphocytes',
             'Neutrophils','WCC','NLR_val','PT_val')

#Stripplot can show results of imputation each variable needs to be indicated
#for some reason it will accept a string as input on command line but not here
#using eval/parse should get around but that also doesn't work?
#stripplot(imputed_train, 'CRP_val' ,xlab = "Imputation number") #works

#Just use one of the imputed datasets (3rd out of 5)
#MICE is sampling from possible data distributions so need to run analysis
#per imputation - for now only do one
train.data <- complete(imputed_train, 3)
test.data <- complete(imputed_test, 3)
fulldata <- complete(imputed_full, 3)
is.na(fulldata)

# Impute missing values using MICE (Multiple Imputation by Chained Equations)
# Only looking at eGFR,WCC,Neutrophils,Lymphocytes,NLR,Hb,Platelets (PLT),CRP for time being
pdf(paste(save_path, 'mice_plot_after_imputation.pdf',sep = ''))
mice_plot <- aggr(fulldata, col = c("green","red","yellow"),
                  numbers = TRUE, sortVars = TRUE,
                  labels = names(fulldata), cex.axis = 0.7,
                  gap = 3, ylab = c("Missing data","Pattern"))
dev.off()
