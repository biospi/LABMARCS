#Using univariate analysis include only vars that were statistically 
#significant (or clinically significant), Commented out variable at end is the reference group

if (0) { #reduced model based on projpred
  dum_var_ls <- c('Age_50_59','Age_60_69','Age_70_79','Age_80above', # Age_Below50 
                  "NLR_val_Normal","NLR_val_Mild","NLR_val_Moderate","NLR_val_Severe",#"NLR_val_NA"                
                  "O2_val_Normal","O2_val_Abnormal", #"O2_val_Test not taken"      
                  "CO2_val_Normal","CO2_val_Abnormal",  #"CO2_val_Test not taken"     
                  "Urea_val_Normal","Urea_val_Abnormal", #"Urea_val_Test not taken"
                  "outcome" #always have outcome in final column
  )
}

if (1) { #curated selection (no vars that aren't significant in univar)
  dum_var_ls <- c("Gender_F", #Gender_M ODD gender is not significant in GLM
                  'Age', # Age_50_59','Age_60_69','Age_70_79','Age_80above', # Age_Below50 
                  "BE_val_Normal","BE_val_Abnormal",  #"BE_val_Test not taken"
                  "BNP_val_Normal","BNP_val_Abnormal",#"BNP_val_Test not taken"
                  "CRP_val_Normal","CRP_val_Abnormal", #"CRP_val_NA"
                  "DDM_val_Normal","DDM_val_Abnormal", #"DDM_val_Test not taken"
                  "eGFR_val_Normal","eGFR_val_Abnormal",  #"eGFR_val_NA"
                  "FER_val_Normal","FER_val_Mild","FER_val_Moderate","FER_val_Severe",# "FER_val_Test not taken"
                  #"fib_val_Normal","fib_val_Mild","fib_val_Severe", #"fib_val_Test not taken"
                  "Glucose_val_Normal","Glucose_val_Abnormal", #"Glucose_val_Test not taken"
                  "HB_val_Normal","HB_val_Mild","HB_val_Moderate","HB_val_Severe", #"HB_val_NA"
                  #"HBA1c_val_Normal","HBA1c_val_Abnormal",  #"HBA1c_val_Test not taken"
                  "LDH_val_Normal","LDH_val_Mild","LDH_val_Moderate","LDH_val_Severe", #"LDH_val_Test not taken", 
                  #"PCT_val_Abnormal", #"PCT_val_Test not taken"     NEver normal?
                  "PLT_val_Normal","PLT_val_Mild","PLT_val_Moderate","PLT_val_Severe", #"PLT_val_NA"                        
                  "trig_val_Normal","trig_val_Abnormal",  #"trig_val_Test not taken"
                  "trop_val_Normal","trop_val_Abnormal",  #"trop_val_Test not taken"
                  "Lymphocytes_Normal","Lymphocytes_Mild","Lymphocytes_Moderate", "Lymphocytes_Severe", #"Lymphocytes_NA"
                  "Neutrophils_Normal","Neutrophils_Mild","Neutrophils_Moderate","Neutrophils_Severe",#"Neutrophils_NA"             
                  "WCC_Normal","WCC_Mild","WCC_Moderate","WCC_Severe", #"WCC_NA"                     
                  "NLR_val_Normal","NLR_val_Mild","NLR_val_Moderate","NLR_val_Severe",#"NLR_val_NA"                
                  "APTT_val_Normal","APTT_val_Mild","APTT_val_Moderate",#"APTT_val_Test not taken"   
                  "PT_val_Normal","PT_val_Abnormal", #"PT_val_Test not taken"
                  "poctLAC_val_Normal","poctLAC_val_Abnormal", #"poctLAC_val_Test not taken" 
                  "O2_val_Normal","O2_val_Abnormal", #"O2_val_Test not taken"      
                  "CO2_val_Normal","CO2_val_Abnormal",  #"CO2_val_Test not taken"     
                  "poctpH_val_Normal","poctpH_val_Abnormal",  #"poctpH_val_Test not taken"  
                  "viral_coinfection_TRUE", #"viral_coinfection_FALSE"        
                  #"bc_coinfection_TRUE", #"bc_coinfection_FALSE"
                  "resp_coinfection_TRUE", #"resp_coinfection_FALSE"         
                  #"urine_coinfection_TRUE", #"urine_coinfection_FALSE"
                  "Urea_val_Normal","Urea_val_Abnormal", #"Urea_val_Test not taken"
                  #"OnAdmission_FALSE","OnAdmission_NA",#"OnAdmission_TRUE"       
                  "outcome" #always have outcome in final column
  )
}




# #IF we only include those that have 30% > data
if (0) {
   dum_var_ls <- c("outcome",
                   'Age_50_59','Age_60_69','Age_70_79','Age_80above', # Age_Below50 
                   "Gender_F", #Gender_M
                   #"BE_val_Normal","BE_val_Abnormal",  #"BE_val_Test not taken"
                   #"BNP_val_Normal","BNP_val_Abnormal",#"BNP_val_Test not taken"
                   "CRP_val_Normal","CRP_val_Abnormal", #"CRP_val_NA"
                   #"DDM_val_Normal","DDM_val_Abnormal", #"DDM_val_Test not taken"
                  "eGFR_val_Normal","eGFR_val_Abnormal",  #"eGFR_val_NA"
    #              "FER_val_Normal","FER_val_Mild","FER_val_Moderate","FER_val_Severe",# "FER_val_Test not taken"
     #             "fib_val_Normal","fib_val_Mild","fib_val_Severe", #"fib_val_Test not taken"
    #              "Glucose_val_Normal","Glucose_val_Abnormal", #"Glucose_val_Test not taken"
                  "HB_val_Normal","HB_val_Mild","HB_val_Moderate","HB_val_Severe", #"HB_val_NA"
     #             "HBA1c_val_Normal","HBA1c_val_Abnormal",  #"HBA1c_val_Test not taken"
      #            "LDH_val_Normal","LDH_val_Mild","LDH_val_Moderate","LDH_val_Severe", #"LDH_val_Test not taken",
       #           "PCT_val_Abnormal", #"PCT_val_Test not taken"
                  "PLT_val_Normal","PLT_val_Mild","PLT_val_Moderate","PLT_val_Severe", #"PLT_val_NA"
          #        "trig_val_Normal","trig_val_Abnormal",  #"trig_val_Test not taken"
          #        "trop_val_Normal","trop_val_Abnormal",  #"trop_val_Test not taken"
                  "Lymphocytes_Normal","Lymphocytes_Mild","Lymphocytes_Moderate", "Lymphocytes_Severe", #"Lymphocytes_NA"
                  "Neutrophils_Normal","Neutrophils_Mild","Neutrophils_Moderate","Neutrophils_Severe",#"Neutrophils_NA"
                  "WCC_Normal","WCC_Mild","WCC_Moderate","WCC_Severe", #"WCC_NA"
                  "NLR_val_Normal","NLR_val_Mild","NLR_val_Moderate","NLR_val_Severe",#"NLR_val_NA"
        #          "APTT_val_Normal","APTT_val_Mild","APTT_val_Moderate",#"APTT_val_Test not taken"
         #         "PT_val_Normal","PT_val_Test not taken",# "PT_val_NA" CHECK!!!
          #        "poctLAC_val_Normal","poctLAC_val_Abnormal", #"poctLAC_val_Test not taken"
           #       "O2_val_Normal","O2_val_Abnormal", #"O2_val_Test not taken"
          #        "CO2_val_Normal","CO2_val_Abnormal",  #"CO2_val_Test not taken"
          #        "poctpH_val_Normal","poctpH_val_Abnormal",  #"poctpH_val_Test not taken"
                  "viral_coinfection_TRUE", #"viral_coinfection_FALSE"
                  "bc_coinfection_TRUE", #"bc_coinfection_FALSE"
                  "resp_coinfection_TRUE", #"resp_coinfection_FALSE"
                  "urine_coinfection_TRUE", #"urine_coinfection_FALSE"
                  "Urea_val_Normal","Urea_val_Abnormal", #"Urea_val_Test not taken"
                  "OnAdmission_FALSE","OnAdmission_NA"#"OnAdmission_TRUE"
  )
}

# 
# #INCLUDE ALL
if (0) {
  dum_var_ls <- c("outcome",
  'Age_50_59','Age_60_69','Age_70_79','Age_80above', # Age_Below50 
  "Gender_F", #Gender_M
  "BE_val_Normal","BE_val_Abnormal",  #"BE_val_Test not taken"
  "BNP_val_Normal","BNP_val_Abnormal",#"BNP_val_Test not taken"
  "CRP_val_Normal","CRP_val_Abnormal", #"CRP_val_NA"
  "DDM_val_Normal","DDM_val_Abnormal", #"DDM_val_Test not taken"
  "eGFR_val_Normal","eGFR_val_Abnormal",  #"eGFR_val_NA"
  "FER_val_Normal","FER_val_Mild","FER_val_Moderate","FER_val_Severe",# "FER_val_Test not taken"
  "fib_val_Normal","fib_val_Mild","fib_val_Severe", #"fib_val_Test not taken"
  "Glucose_val_Normal","Glucose_val_Abnormal", #"Glucose_val_Test not taken"
  "HB_val_Normal","HB_val_Mild","HB_val_Moderate","HB_val_Severe", #"HB_val_NA"
  "HBA1c_val_Normal","HBA1c_val_Abnormal",  #"HBA1c_val_Test not taken"
  "LDH_val_Normal","LDH_val_Mild","LDH_val_Moderate","LDH_val_Severe", #"LDH_val_Test not taken",
  "PCT_val_Abnormal", #"PCT_val_Test not taken"
  "PLT_val_Normal","PLT_val_Mild","PLT_val_Moderate","PLT_val_Severe", #"PLT_val_NA"
  "trig_val_Normal","trig_val_Abnormal",  #"trig_val_Test not taken"
  "trop_val_Normal","trop_val_Abnormal",  #"trop_val_Test not taken"
  "Lymphocytes_Normal","Lymphocytes_Mild","Lymphocytes_Moderate", "Lymphocytes_Severe", #"Lymphocytes_NA"
  "Neutrophils_Normal","Neutrophils_Mild","Neutrophils_Moderate","Neutrophils_Severe",#"Neutrophils_NA"
  "WCC_Normal","WCC_Mild","WCC_Moderate","WCC_Severe", #"WCC_NA"
  "NLR_val_Normal","NLR_val_Mild","NLR_val_Moderate","NLR_val_Severe",#"NLR_val_NA"
  "APTT_val_Normal","APTT_val_Mild","APTT_val_Moderate",#"APTT_val_Test not taken"
#  "PT_val_Normal","PT_val_Test not taken",# "PT_val_NA" CHECK!!! -- Too limited
  "poctLAC_val_Normal","poctLAC_val_Abnormal", #"poctLAC_val_Test not taken"
  "O2_val_Normal","O2_val_Abnormal", #"O2_val_Test not taken"
  "CO2_val_Normal","CO2_val_Abnormal",  #"CO2_val_Test not taken"
  "poctpH_val_Normal","poctpH_val_Abnormal",  #"poctpH_val_Test not taken"
  "viral_coinfection_TRUE", #"viral_coinfection_FALSE"
  "bc_coinfection_TRUE", #"bc_coinfection_FALSE"
  "resp_coinfection_TRUE", #"resp_coinfection_FALSE"
  "urine_coinfection_TRUE", #"urine_coinfection_FALSE"
  "Urea_val_Normal","Urea_val_Abnormal", #"Urea_val_Test not taken"
  "OnAdmission_FALSE","OnAdmission_NA"#"OnAdmission_TRUE"
  )
}