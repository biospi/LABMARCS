# LABMARCS

## Overview
The LABMARCS code processes and performs machine learning regression analyses on hospital derived data for individuals who were diagnosed positive for COVID-19 over multiple sites in the Southwest. Examination of key biological markers and prediction of death and admission to ICU are considered as outcomes.

The data is not included here due to data protection and governance regulations, however all the analysis tools are provided. Please see the pre-print manuscript here for further details:
https://www.medrxiv.org/content/10.1101/2022.09.16.22279985v4

## Motivation

As of 10 March 2021, there have been appoximetly 117 million confirmed cases of COVID-19 disease worldwide, with 4.2 million cases in the UK, resulting in over 110,000 deaths. COVID-19 has a wide spectrum of clinical features ranging from asymptomatic to severe systemic illness with a significant attributable mortality, while clinical manifestations are variable especially in the most vulnerable groups and immunocompromised people. The majority of people (85%) with COVID-19 will have mild or no symptoms. A proportion (10%) of them suffer from a severe infection needing hospital admission for supportive care such as oxygen and a minority (5%) will require intensive care therapy such as mechanical ventilation.  Within the subset of those requiring hospitalisation, early identification of those who will be likely to deteriorate and require transfer to an intensive care unit (ICU) or who may die is vital for clinical decision making. Healthcare systems across the world including the developed parts (Europe and USA) have faced with immense challenges in terms of capacity and resources to manage this pandemic, which may continue as we enter the relaxation of lock down measures including opening of schools and businesses.

The LABMARCS study aims to analyse all the available laboratory values of different metabolic pathways affected by COVID-19 infection over a comprehensive range of patient variables. We aim to understand their inter-relatedness and impact on key clinical outcomes such as need for admission to hospital, transfer of care to ITU and death. With the two main objectives of this study are:  (1) To explore independent associations of routinely measured physiological and blood parameters at hospital admission with disease severity (ICU admission or death at 28 days from hospital admission), adjusting for demographics and comorbidities. (2) To develop a prediction model for severe COVID-19 outcomes at 28 days in order to generate a simple severity scoring system so that the frontline clinicians can prioritise resources to the most critically ill patients to improve their outcome. Also help them to discharge patients home to free the hospital bed capacity for the most severely ill patients.

## Installation

This project requires access to the LABMARCS dataset which cannot be included here. To use this code, simply download the github repository and set data/ouput paths specified in the key .R files below
to your local directory structure: Ensure working directories throughout the code are modified to these local folders.

DataPreparation.R
DataAnalysisPipeline.R
Analysis_Batch.R

## Version and depdendencies

This code was built and run with R version 4.1.3.

All neccessary libraries are specified within the code and should be available and straightforward to install and run from CRAN.

## Brief Description of Included Scripts

There are two main main parts of the code include:

**DataPreparation.R**: Performs preparation of the raw clinical data into a format which can be used more readily by regression analysis (which requires for our purposes one row per individual patient). This includes importing data for individual tests from .csv files generated from the hospital EPRs. This script then cleans this data, extracts test results related to each individual patient of interest and compresses each individual test result type into one measurement (if multiple measurements are provided). Where appropriate, test results are also assigned as 'Normal/Abnormal' according to clinical reference guidelines. (More information about the individual test results are given in the table below). Other data extracted alongside test results are patient gender, age and for a subset of the patients we can extract co-morbidity data. Once done processing, this script saves the data into a .csv file which can be used for analysis by all subsequent pieces of code. **This must be run before any other code** Once complete there is no need to run again unless you want to try different parameters.

**DataAnalysisPipeline.R**: This script uses the output from 'DataPreparation.R'.

First, this script processes the data further to get in shape for modelling. The data fields are trimmed to be suitable for a logistic regession (only including the desired outcome and co-variates). Co-variate distributions and correlation are examined. In early versions we looked into imputation (MICE, k-nn), transforming continuous data to normal distributions, windsorization. Some of these bits are commented out in the code as we settled on using 'Test Not Taken' instead of imputation, and found other data manipulations to not improve performance.

Second, this script runs several different regression models with varying complexity including:

(1) **Single_Biomarker_Tests_CrossValidate_with_AgeGenderCV_Control_PARALLEL.R** Single biomarker analysis using standard logistic GLM, same w/ LASSO, bayesian logistic regression with a flat prior and another with a horseshoe prior (the 4 models). Biomarkers are compared between a model using only age and gender versus a model with those two variables and the biomarker in question. These are evaluated for odds ratios, AUC, and several other measures

(2) **GLM_Models_without_CV.R** Fit the 4 models to all of the data. These are evaluated for odds ratios, AUC w/ ROC curve, calibration, and several other measures

(3) **Run_Models_CrossValidation.R** Fit the 4 models PLUS two more ('the 6 models'), including a GLM using the variables selected via LASSO and a projective prediction model using variables selected via ProjPred, which confusingly happens in the code later (see point #4 below)to an 80/20 split of the data for N repeats (currently N=20, for 100 total runs). These are evaluated for odds ratios, AUC w/ ROC curve, calibration, and several other measures and figures. This script also runs a stability analysis on the CV LASSO model runs, see *'Variable selection - A review and recommendations for the practicing statistician'* George Heinz. Located: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5969114/

(4) Reduced models with variable selection. This is slightly confusing as LASSO and ProjPred need to be run on the full data to get their chosen variables and then the reduced models are run,
either as a LASSO 'inspired' GLM **LASSO_Inspired_GLM_Models_without_CV.R**, or using one of the great features about ProjPred, using a model that is projected to a lower dimensionality from the fit on the original data, see **ProjectivePredictionVariableSelect.R** this can take 1-2 days of compute time ¬2022 Intel mobile i7 - This runs 

For all models, the performance metrics and key graphs are displayed and saved in a subfolder 'output' which is  by default created in the source directory. If you run all code available it will generate several hundred files

**Analysis_Batch.R**: When the project first started we were unsure on many parameters before decing we wanted the first reading with 3 days and would predict severe outcomes (death OR ICU).
This batch file is setup to run models covering a range of these parameters. The batch files re-runs datapreparation.R to make sure the raw data is prepped for these parameters/
Note, all manuscript numbers and figures were generated running only DataAnalysisPipeline.R with it's default values. The code as is *will take 3-4 days* to run for one group of parameters so without further optimisation the batch analysis is best avoided (or alternately turn off parts of the code that do bayesian MC/MC and/or projpred variable seleciton, which are the two main bottlenecks, aside from the fact that
most of the code is not parallel, except the individual biomarker analysis.

Below we describe several key variables:

*dateRange*
Which tests to consider when we compress from the clinical data. The options are all tests taken on the same day of admission, (or day of a positive COVID-19 test if already admitted).
We tried from 0,1,3,5,7 and 14 and found 3 was a good compromise to allows some tests that were not immediately available while not waiting too long

*readingwanted*
Compression of clinical data to one row via (1) first test result after positive COVID test (2) worst test result among a set of tests (3) mean of the set of tests.
We chose first as this is the most pragmatic as not all patients have multiple readings.

*outcomewanted*
Which outcome we want to consider: (1) Any severe outcome (ICU or death) (2) Transfer to ICU (3) death. ICU transfers are infrequent in our dataset, so a composite severe outcome was a better prediction outcome.
