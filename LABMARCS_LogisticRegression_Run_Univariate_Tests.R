print("Run univariate models")

if (!BatchAnalysisOn) {
  #Initialise results tibble, else done in the batch file
  univar_batch_result_tb <- tibble(
    day = integer(),
    outcome = integer(),
    readingtype = integer(),
    variable = character(),
    variable_dum = character(),
    oddsratio = numeric(),
    pval = numeric(),
    levels = character()
  )
}

# Initialise model outputs into lists
modelunivar <- vector(mode = "list", length = (length(fulldata_nodummy) - 1))
modeldemo <- vector(mode = "list", length = (length(fulldata_nodummy) - 3))

family_ls = c('glm.fit','firthglm.fit')

#build individual models for each variable we have recorded
for (i in 3:(dim(fulldata_nodummy)[2] - 1) ) { #skip age,gender (#1&2) & outcome (last) columns
  modelunivar[[i]] <- glm(fulldata_nodummy$outcome ~ fulldata_nodummy[, i],
                          family = "binomial",
                          method = family_ls[IncludeFirthBias + 1])
  
  #get coefficients p-values - and print if significant
  tmp <- coef(summary(modelunivar[[i]]))
  
  #if any predictor (except intercept) is below .05 & print summary of each
  if (sum(tmp[2:dim(tmp)[1],4] <= 0.05) > 0) {
    
    for (j in 2:dim(tmp)[1]) {
      #create new line for each predictor
      ctr <- dim(univar_batch_result_tb)[1] + 1
      univar_batch_result_tb[ctr,'day'] <- dateRange
      univar_batch_result_tb[ctr,'outcome'] <- outcomeselection
      univar_batch_result_tb[ctr,'readingtype'] <- readingwanted
      univar_batch_result_tb[ctr,'variable'] <- names(fulldata_nodummy)[i]
      univar_batch_result_tb[ctr,'variable_dum'] <- row.names(tmp)[j]
      univar_batch_result_tb[ctr,'oddsratio'] <- exp(tmp[j,1])
      univar_batch_result_tb[ctr,'pval'] <- tmp[j,4]
      univar_batch_result_tb[ctr,'levels'] <- toString(levels(fulldata_nodummy[[i]]))
    }
  }
}

print("Saving results tables...")
write.csv(univar_batch_result_tb, 
          paste(output_path, 'UnivariateGLM_Batch_Result.csv',sep = ''), 
          row.names = FALSE)

