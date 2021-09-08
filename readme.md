# LABMARCS

## Overview
The LABMARCS code processes and performs machine learning regression analyses on hospital derived data for individuals who were diagnosed positive for COVID-19 over multiple sites in the Southwest. Examination of key biological markers and prediction of death and/or admission to ICU are considered as outcomes.

The data is not included here due to data protection and governance regulations, however all the analysis tools are provided.

## Motivation

As of 10 March 2021, there have been appoximetly 117 million confirmed cases of COVID-19 disease worldwide, with 4.2 million cases in the UK, resulting in over 110,000 deaths. COVID-19 has a wide spectrum of clinical features ranging from asymptomatic to severe systemic illness with a significant attributable mortality, while clinical manifestations are variable especially in the most vulnerable groups and immunocompromised people. The majority of people (85%) with COVID-19 will have mild or no symptoms. A proportion (10%) of them suffer from a severe infection needing hospital admission for supportive care such as oxygen and a minority (5%) will require intensive care therapy such as mechanical ventilation.  Within the subset of those requiring hospitalisation, early identification of those who will be likely to deteriorate and require transfer to an intensive care unit (ICU) or who may die is vital for clinical decision making. Healthcare systems across the world including the developed parts (Europe and USA) have faced with immense challenges in terms of capacity and resources to manage this pandemic, which may continue as we enter the relaxation of lock down measures including opening of schools and businesses.

The LABMARCS study aims to analyse all the available laboratory values of different metabolic pathways affected by COVID-19 infection over a comprehensive range of patient variables. We aim to understand their inter-relatedness and impact on key clinical outcomes such as need for admission to hospital, transfer of care to ITU and death. With the two main objectives of this study are:  (1) To explore independent associations of routinely measured physiological and blood parameters at hospital admission with disease severity (ICU admission or death at 28???days from hospital admission), adjusting for demographics and comorbidities. (2) To develop a prediction model for severe COVID-19 outcomes at 28???days in order to generate a simple severity scoring system so that the frontline clinicians can prioritise resources to the most critically ill patients to improve their outcome. Also help them to discharge patients home to free the hospital bed capacity for the most severely ill patients.

## Installation

This project requires access to the LABMARCS dataset which cannot be included here. To use this code, simply download the github repository and place inside a folder with the LABMARCS dataset. Ensure working directories throughout the code are modified to this folder.

## Version and depdendencies

This code was built and run with R version 4.0.3.

All neccessary libraries are given within the code and should be available and straightforward to install and run from CRAN.

## Brief Description of Included Scripts

The four main parts of the code include:

**one_row_per_id_all**: Performs preparation of the raw clinical data into a format which can be used more readily by regression analysis (which requires for our purposes one row per individual patient). This includes importing data for individual tests from .csv files generated from the hospital EPRs. This script then cleans this data, extracts test results related to each individual patient of interest and compresses each individual test result type into one measurement (if multiple measurements are provided). Where appropriate, test results are also assigned as 'Normal/Abnormal' according to clinical reference guidelines. (More information about the individual test results are given in the table below). Other data extracted alongside test results are patient gender, age and for a subset of the patients we can extract co-morbidity data. Once done processing, this script saves the data into a .csv file which can be used for analysis by 'LogisticLABMARCSall.R' and 'CoxLABMARCSall.R'.

**LogisticLABMARCSall**: This script uses the output from 'one_row_per_id_all.Rmd'.

Firstly, this script processes the data further. The data fields are trimmed to be suitable for a logistic regession (only including the desired outcome and co-variates). Co-variate distributions and correlation are examined. Continuous co-variates are transformed to be approximately normally distributed and regularised (mean set to 0, standard deviation set to 1). Co-variates with >70% missing data are dropped, data is windsorised at 1st and 99th percentile. Missing data is imputed using either k-nearest neighbour or MICE (set as an additional option).

Secondly, this script runs simple prelminary logistic regression models on the full dataset. We build models examining the univariate relationship of the outcome with each co-variate, plus models which repeat this process but additionally adjust on just age and gender.

Thirdly, this script examines multiple machine learning logistic regression prediction model:
(1) A global model which uses all co-variates as inputs using a simple 80/20 train/test split. Performance measures are generated.
(2) An automated stepwise model using all co-variates initially using a simple 80/20 train/test split. Performance measures are generated.
(3) A repeated nested cross-validated LASSO model, with outer and inner folds using an 80/20 train/test split. Summary measures of expected performance are generated (AUC).

For all models, the performance metrics and key graphs are displayed and saved in a subfolder 'LogisticOutputs' which is created in the main directory.

Fourthly, this script runs a full stability analysis on the LASSO model runs. This follows steps (i)-(vii) as described in the following paper:

*'Variable selection - A review and recommendations for the practicing statistician'*
George Heinz. Located: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5969114/


**CoxLABMARCSall**: This script uses the output from 'one_row_per_id_all.Rmd'.

NOTE: This script runs very similarly to LogisticLABMARCS.R and so only additional information is given below to highlight the differences:

Firstly, during data processing we derive the 'number of days' between events including positive test date, admission date, discharge date and death date, which when used in conjuction with the desired outcome (ICU and/or death) provides 'time to outcome' or just 'time' in the cox models.

Secondly, we run cox regression rather than logistic regression for the preliminary models.
*KNOWN BUG: Firth Bias with the cox regressions is not working*

Thirdly, we also run cox regression rather than logistic regression. The main performance measure in now c-index rather than AUC.

For all models, the performance metrics and key graphs are displayed and saved in a subfolder 'CoxOutputs' which is created in the main directory.

Fourthly, this script runs a full stability analysis however the cox model works on hazard ratios (HRs) and not odds ratios (ORs) and due to this and other potential important differences in the regression type, the stability analyses may need adapting. For the time being howeer we continue to follows steps (i)-(vii) as described in the following paper:

*'Variable selection - A review and recommendations for the practicing statistician'*
George Heinz. Located: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5969114/

**MasterAnalysis**: Furthermore, we include this top level file which runs through all the other three scripts sequentially (and runs each multiple times considering all combinations of the important optional parameters). This is useful to consider all key variations of the model (as described below) without needing to manually change the options within the script and rerun:

*dateRange*
Which tests to consider when we compress from the clinical data. The options are all tests taken on the same day, plus (1),(3) or (5) days after a positive COVID-19 test. 

*readingwanted*
Compression of clinical data to one row via (1) first test result after positive COVID test (2) worst test result among a set of tests (3) mean of the set of tests

*outcomewanted*
Which outcome we want to consider: (1) Any severe outcome (ICU or death) (2) Transfer to ICU (3) death

From these options and the two regression types we have 2 x 3 x 3 x 3 = 54 different models. This script then compares key performance measures (AUC for logistic regression and c-index for cox regression) for each model and displays this info side by side to aid in selecting the best options for the final chosen models.

## Quick guide

### Tutorial 1 - singular analysis

We consider processing the data and examining the results of just one model.

Step 1: Run the processing code 'one_row_per_id_all.Rmd' making sure to first examine the section of code below:

```{r}
setwd("C:/Users/lm13381/OneDrive - University of Bristol/Documents/AMR Project/LABMARCS/Code/LABMARCS-main/LABMARCS-main")
# MasterAnalysisOn==1 prevents the dateRange and readingwanted variables from
# being overwritten, but the option to turn this off and run this script without
# first running MasterAnalysis.R is available by setting MasterAnalysisOn==0 and then
# manually selecting the dateRange and readingwanted variables.
MasterAnalysisOn <- 1
if (MasterAnalysisOn==0) {
dateRange <- 5 # 1, 3 or 5 days
readingwanted <- 1 # 0-worst, 1-first, 2-mean
}
```

Make sure setwd() is alligned with your working directory and is where the raw data is stored.

When wanting to run a singular model set MasterAnalysisOn to 0.

Now set dateRange to 1,3 or 5. This represents the number of days after the COVID-19 positive test in which test data is considered. Positive test date is also included in all cases.

Now set readingwanted to 0, 1 or 2. This represents:
(0) The most extreme value for a particular test taken from all tests in the window set by dateRange
(1) The first test value available after the COVID-19 positive test, but limited to the cut-off specified by dateRange
(2) The mean value of all the test results availble in the window specified by dateRange

The values you enter for these two parameters will determine the name of the output .csv file generated. These are listed below as determined in the code:

```{r}
if (dateRange==1 & readingwanted==0) {
write.csv(x=totalBinary, file="totalBinary1worst.csv")
  }
if (dateRange==1 & readingwanted==1) {
write.csv(x=totalBinary, file="totalBinary1first.csv")
  }
if (dateRange==1 & readingwanted==2) {
write.csv(x=totalBinary, file="totalBinary1mean.csv")
  }
if (dateRange==3 & readingwanted==0) {
write.csv(x=totalBinary, file="totalBinary3worst.csv")
  }
if (dateRange==3 & readingwanted==1) {
write.csv(x=totalBinary, file="totalBinary3first.csv")
  }
if (dateRange==3 & readingwanted==2) {
write.csv(x=totalBinary, file="totalBinary3mean.csv")
  }
if (dateRange==5 & readingwanted==0) {
write.csv(x=totalBinary, file="totalBinary5worst.csv")
  }
if (dateRange==5 & readingwanted==1) {
write.csv(x=totalBinary, file="totalBinary5first.csv")
  }
if (dateRange==5 & readingwanted==2) {
write.csv(x=totalBinary, file="totalBinary5mean.csv")
  }
```

Step 2: Run the processing code 'LogisticLABMARCS.R' making sure to first examine the section of code below:

```{r}
# Pre-set switches in the code (change to modify desired behaviours)
 # Set the working directory (must include the .csvs generated by one_line_per_id_all)
  setwd("C:/Users/lm13381/OneDrive - University of Bristol/Documents/AMR Project/LABMARCS/Code/LABMARCS-main/LABMARCS-main")
  # Set LookForUpdates to 1 if you want to search for and install newest R version
  LookForUpdates <- 0
  # Set InstallPackages to 1 if you want to install the packages listed below all at once
  InstallPackages <- 0
  # Set LoadLibraries to 1 if you want to load all the packages listed below all at once
  LoadLibraries <- 1
  # outcomeselection (1) all severe outcomes (2) ICU admission (3) death
  outcomeselection <- 3
  # Chose to include Firth's bias in the models
  IncludeFirthBias <- 1
  # 0 - No imputation, 1- K nearest neighbour imputation, 2- MICE imputation
  ImputationAllowed <- 1
  # Model exclusively looking at comordbities (looks only at 1 hospital currently, also
  # apply due caution to looking at the stability analyses etc when ==1)
  Comorbidities <- 0
  # MasterAnalysisOn==1 prevents the dateRange and readingwanted variables from
  # being overwritten, but the option to turn this off and run this script without
  # first running MasterAnalysis.R is available by setting MasterAnalysisOn==0 and then
  # manually selecting the dateRange and readingwanted variables below
  MasterAnalysisOn <- 1
  if (MasterAnalysisOn==0) {
    # Set dateRange to 1,3 or 5. This represents the number of days after the COVID-19
    # positive test in which test data is considered. Positive test date is also included
    # in all cases.
    dateRange <- 1
    # Set readingwanted to 0, 1 or 2. This represents:
    # (0) The most extreme value for a particular test taken from
    #     all tests in the window set by dateRange
    # (1) The first test value available after the COVID-19 positive test,
    #     but limited to the cut-off specified by dateRange
    # (2) The mean value of all the test results availble in the window specified
    #     by dateRange
    readingwanted <- 0
  }
  }
```

Again, make sure setwd() is alligned with your working directory and is where the raw data is stored.

When wanting to run a singular model again set MasterAnalysisOn to 0.

Now set dateRange and readingwanted to match the values you selected in step 1.

Also select your chosen outcome measure to be 'outcomeselection==1' for death and/or transfer to the ICU, '==2' for just transfer to the ICU and '==3' for just death.

Additionally, you may set 'IncludeFirthBias==1' to include Firth Biasing in the simple preliminary models or '==0' if not wanted. 'ImputationAllowed==0' will only consider complete cases (not advised) '==1' will perform k nearest neighbour imputation and '==2' will perform MICE. 'Comorbidities==1' will additionally consider a range of co-morbidities but is only avilable for ~50% of the data and is still an ongoing peice of work so caution is advised, '==0' will not include these comorbidities.

The values you enter for dateRange and readingwanted will determine the save folder for the model outputs (which include model structures, graphs and key performance metrics). An example would be a subfolder is created called 'LogisticOutputs' with a subfolder called '1 Days 0' corresponding to dateRange==1 and readingwanted==0. 

Explicitly, these are determined in the code as:

```{r}
# Get current working directory so can revert to this at the end of the code
mainDir <- getwd()
# Name of folder for saving results from this model
subDir <- "LogisticOutputs"
# Name of sub sub folders containing results for micro-choices in the models
# e.g. 'dateRange' and 'readingwanted' in the window for results
subsubDir <- paste(dateRange,"days",readingwanted)
# Create directory for saving outputs (works fine if it already exists despite warning)
dir.create(file.path(mainDir, subDir))
dir.create(file.path(mainDir, subDir, subsubDir))
# Set current working directory to this folder
setwd(file.path(mainDir, subDir, subsubDir))
```

Step 3: Very similarly to step 2, you may also run a cox model. Simply follow the same instructions. The subfolder where results are saved will be entitled 'CoxOutputs', and the saved results will include the c-index rather than the AUC.

Step 4: Examine the saved model outputs to evaluate the model performances. These need tidying up and aren't exhaustive, but give a general idea of how the model is performing.


### Tutorial 2 - complete analysis and model comparison

Here we consider running all of the different available models using 'MasterAnalysis.R'

Step 1: Check within 'one_row_per_id_all.Rmd', 'LogisticLABMARCSall.R' and 'CoxLABMARCSall.R' that setwd() is set to your directory including the LABMARCS dataand that MasterAnalysisOn is set to 1 in all scripts (see tutorial 1 for the explicit code segment in each case). Also set MainDir to this same folder within 'MasterAnalysis.R'

Step 2: Ensure the other options in 'LogisticLABMARCSall.R' and 'CoxLABMARCSall.R' are as intended. The options 'IncludeFirthBias', 'ImputationAllowed', 'Comordbidities' and 'outcomeselection' are currently treated as fixed (although this could be expanded upon).

Step 3: In AvonCap.csv and 'one_row_per_id_all.Rmd' ALL A with an ~ above (cannot include this actual character here as Rmd does not support it, but can be found here: https://en.wikipedia.org/wiki/%C3%83) must be changed to A as the function we use to convert an Rmd to an R file (knitr::purl) has issues recognising UTF-8 characters. To do this, simply go into the data in excel or notepad and replace.

Step 4: Run the 'MasterAnalysis.R' script. WARNING: This might be quite time intensive depending on your settings for the number of runs in the repeated nested cross validation version of the LASSO regressions.

Step 5: Examine the output in the console to see the performance of the different models side by side.

## Table of Biomarkers

| Biomarker        | Type                       | Reference range                                                                                                                                                                   | Comments                                 |
|------------------|----------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------|
| BC               | Categorical: 34 categories |                                                                                                                                                                                   |                                          |
| Vir              | Categorical: 10 categories |                                                                                                                                                                                   |                                          |
| BE               | Continuous                 | 22-29                                                                                                                                                                             | Higher or lowe                           |
| BNP              | Continuous                 | Men under 70: <100pg/ml, Women under 70: <150 pg/ml, All 70yr and over: <300 pg/ml""                                                                                              | Normal or abnormal based on these ranges |
| APTT             | Continuous                 | 21-33 seconds                                                                                                                                                                     | =abnormal if>33seconds                   |
| PT               | Continuous                 | 9.5-13 seconds                                                                                                                                                                    | =abnormal if >13 seconds                 |
| CO2              | Continuous                 | 4.6-6.4 seconds                                                                                                                                                                   | = abnormal if >6.4or<4.6                 |
| O2               | Continuous                 | 11.0-14.4 seconds                                                                                                                                                                 | = abnormal if <11 or >14.4               |
| Covid CT         | Continuous                 |                                                                                                                                                                                   |                                          |
| CRP              | Continuous                 | < 6 mg/L                                                                                                                                                                          | Abnormal if >6                           |
| DDM              | Continuous                 | Age (Years)     D-dimer (ng/ml)    <60                 <500    61-70                 <600    71-80                 <700    81-90                 <800    >90                 <900 | Abnormal if outside these bounds         |
| eGFR             | Continuous                 | >90                                                                                                                                                                               | Abnormal if <90                          |
| Lymphocyte count | Continuous                 | 1.5-4.5 10^9/L                                                                                                                                                                    | Abnormal if outside these bounds         |
| Neutrophil count | Continuous                 | 2.0-7.5 10^9/L                                                                                                                                                                    | “                                        |
| NLR (?)          | Continuous                 | 0.78 and 3.53                                                                                                                                                                     | Abnormal if outside these bounds         |
| White Cell Count | Continuous                 | 4.0-11.0 10^9/L                                                                                                                                                                   | “                                        |
| FER              | Continuous                 | Male: 33-490, Female(0-44): 15-445, Female(45+yrs): 30-470                                                                                                                        | Abnormal if outside these bounds         |
| fib              | Continuous                 | 1.8-4.0 g/L                                                                                                                                                                       | “                                        |
| Glucose          | Continuous                 | Fasting: 3.0-6.0 mmol/L, Non-fasting: 3.0-7.8 mmol/L                                                                                                                              | “                                        |
| HB               | Continuous                 | Male 130-170 g/L, Female 120-150 g/L                                                                                                                                              | “                                        |
| HBA1c            | Continuous                 | >/= 48 mmol/mol probable diabetes, 42-48 mmol/mol increased risk                                                                                                                  | “                                        |
| LDH              | Continuous                 | 240-480 IU/L                                                                                                                                                                      | “                                        |
| PLT              | Continuous                 | 150-450 10^9/L                                                                                                                                                                    | “                                        |
| PCT              | Continuous                 | Normal range: <0.05ng/mL, <0.50ng/mL low risk of severe sepsis, >2.00ng/mL high risk severe sepsis                                                                                | “                                        |
| Lactate          | Continuous                 | 0.5-2.2 mmol/L                                                                                                                                                                    | Abnormal if >2.2                         |
| pH               | Continuous                 | 7.35-7.45                                                                                                                                                                         | Abnormal if outside these bounds         |
| trig             | Continuous                 | 0.5-1.7                                                                                                                                                                           | “                                        |
| trop             | Continuous                 | Normal: <14ng/L, Possible MI: 14-30 ng/L, Probable MI: >30 ng/L                                                                                                                   | Abnormal if outside these bounds         |
