library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(plotly)
library(BBmisc)
library(scales)
library(dplyr)
library(naniar)
library(gridExtra)
library(knitr)
library(moments)
library(formattable)
library(IRdisplay)
library(repr)
## ------------------------------------------------------------------------------
data_dir <- 'C:/Users/gq19765/SyncThing/Repositories/LABMARCS/data/'
output_dir <- 'C:/Users/gq19765/SyncThing/Repositories/LABMARCS/PatientViz_Sep22/processed_data/'
setwd(data_dir)
# Grab NBT Outcomes
NBT_outcomes <- read.csv(file("NBT_outcomes.csv"))
NBT_outcomes = NBT_outcomes %>% 
  rename(
    admissionDate = admission_date,
  )
NBT_outcomes$admissionDate = as.Date(NBT_outcomes$admissionDate, format="%d/%m/%Y")
NBT_outcomes = NBT_outcomes %>% 
  rename(
    dischargeDate = discharge_date,
  )
NBT_outcomes$dischargeDate = as.Date(NBT_outcomes$dischargeDate, format="%d/%m/%Y")
NBT_outcomes_deaths = NBT_outcomes %>%
  filter(DischargeOutcomeDesc == "Patient Died")
NBT_outcomes_deaths$deathDate = NBT_outcomes_deaths$dischargeDate
NBT_outcomes <- merge(NBT_outcomes,NBT_outcomes_deaths,all=TRUE)
NBT_outcomes$ITU_Start = as.Date(NBT_outcomes$ITU_Start, format="%d/%m/%Y")
NBT_outcomes$ITU_End = as.Date(NBT_outcomes$ITU_End, format="%d/%m/%Y")
NBT_outcomes2 = NBT_outcomes %>% select(uob_ID,admissionDate,dischargeDate,ITU_Start,ITU_End,deathDate)
## ------------------------------------------------------------------------------
# Grab UHB outcomes
UHB_outcomes1 <- read.csv(file("UHB_outcomes1.ICU_Update.csv"))
UHB_outcomes1 = UHB_outcomes1 %>% 
  rename(
    admissionDate = attend_date,
  )
UHB_outcomes1$admissionDate = as.Date(UHB_outcomes1$admissionDate, format="%d/%m/%Y")
UHB_outcomes1$dischargeDate = as.Date(UHB_outcomes1$admissionDate + as.integer(UHB_outcomes1$hospital_length_of_stay))
UHB_outcomes1 = UHB_outcomes1 %>% 
  rename(
    deathDate = fu_death_date,
  )
UHB_outcomes1$deathDate = as.Date(UHB_outcomes1$deathDate, format="%d/%m/%Y")
UHB_outcomes1$ITU_Start <- as.Date(NA)
UHB_outcomes1$ITU_End <- as.Date(NA)
UHB_outcomes12 = UHB_outcomes1 %>% select(uob_ID,admissionDate,dischargeDate,ITU_Start,ITU_End,deathDate)

UHB_outcomes2 <- read.csv(file("UHB_outcomes2.ICU_Update.csv"))
UHB_outcomes2 = UHB_outcomes2 %>% 
  rename(
    admissionDate = attend_dte,
  )
UHB_outcomes2$admissionDate = as.Date(UHB_outcomes2$admissionDate, format="%d/%m/%Y")
UHB_outcomes2 = UHB_outcomes2 %>% 
  rename(
    dischargeDate = outcome_dte,
  )
UHB_outcomes2$dischargeDate = as.Date(UHB_outcomes2$dischargeDate, format="%d/%m/%Y")
UHB_outcomes2_deaths = UHB_outcomes2 %>%
  filter(outcome == 3)
UHB_outcomes2_deaths$deathDate = UHB_outcomes2_deaths$dischargeDate
UHB_outcomes2 <- merge(UHB_outcomes2,UHB_outcomes2_deaths,all=TRUE)
UHB_outcomes2$ITU_Start <- as.Date(NA)
UHB_outcomes2$ITU_End <- as.Date(NA)
UHB_outcomes22 = UHB_outcomes2 %>% select(uob_ID,admissionDate,dischargeDate,ITU_Start,ITU_End,deathDate)
## ------------------------------------------------------------------------------
# Grab Weston Outcomes
WestonOutcomes <- read.csv(file("Weston.Outcomes.csv"))
WestonOutcomes = WestonOutcomes %>% 
  rename(
    admissionDate = Admission.date,
  )
WestonOutcomes$admissionDate = as.Date(WestonOutcomes$admissionDate, format="%d/%m/%Y")
WestonOutcomes = WestonOutcomes %>% 
  rename(
    dischargeDate = Discharge.date,
  )
WestonOutcomes$dischargeDate = as.Date(WestonOutcomes$dischargeDate, format="%d/%m/%Y")
WestonOutcomes = WestonOutcomes %>% 
  rename(
    ITU_Start = ICU.Admission.Date
  )
WestonOutcomes$ITU_Start = as.Date(WestonOutcomes$ITU_Start, format="%d/%m/%Y")
WestonOutcomes = WestonOutcomes %>% 
  rename(
    ITU_End = ICU.Discharge.Date
  )
WestonOutcomes$ITU_End = as.Date(WestonOutcomes$ITU_End, format="%d/%m/%Y")
WestonOutcomes = WestonOutcomes %>% 
  rename(
    deathDate = Date.of.Death
  )
WestonOutcomes$deathDate = as.Date(WestonOutcomes$deathDate, format="%d/%m/%Y")
WestonOutcomes2 = WestonOutcomes %>% select(uob_ID,admissionDate,dischargeDate,ITU_Start,ITU_End,deathDate)
## ------------------------------------------------------------------------------
## Demographics Table
dem <- read.csv(file("masterlist.update.csv"))
dem <- dem %>%
  rename(Age = agecat) %>%
  distinct(uob_ID, .keep_all = TRUE) %>%
  dplyr::select(uob_ID,Gender,Age)
# Export dem DataFrame
write.csv(dem,paste(output_dir,"dem.csv",sep=""))

# Merge outcomes data
totalOutcomes <- rbind(UHB_outcomes12,UHB_outcomes22,NBT_outcomes2,WestonOutcomes2) # NBT Outcomes must be excluded as inconsistent
totalOutcomes$deathDate = as.Date(totalOutcomes$deathDate, format="%d/%m/%Y")
totalOutcomes$ITU_Start = as.Date(totalOutcomes$ITU_Start, format="%d/%m/%Y")
totalOutcomes$ITU_End = as.Date(totalOutcomes$ITU_End, format="%d/%m/%Y")
totalOutcomes$dischargeDate = as.Date(totalOutcomes$dischargeDate, format="%d/%m/%Y")
# Export outcomes DataFrame
write.csv(totalOutcomes,paste(output_dir,"outcomes.csv",sep=""))
## ------------------------------------------------------------------------------
BE <- read.csv(file("BE.csv"))
BE$Date.Booked.In = as.Date(BE$Date.Booked.In, format="%d/%m/%Y")
BE = BE %>% 
  rename(
    date = Date.Booked.In,
  )
BE = BE %>%
  rename(
    BE_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
BNP <- read.csv(file("BNP.csv"))
BNP$Date.Booked.In = as.Date(BNP$Date.Booked.In, format="%d/%m/%Y")
BNP = BNP %>% 
  rename(
    date = Date.Booked.In,
  )
BNP = BNP %>%
  rename(
    BNP_val = Numeric.Result
  )
BNP_transform <- BNP
## ------------------------------------------------------------------------------
CRP <- read.csv(file("CRP.csv"))
CRP$Date.Booked.In = as.Date(CRP$Date.Booked.In, format="%d/%m/%Y")
CRP = CRP %>% 
  rename(
    date = Date.Booked.In,
  )
CRP = CRP %>%
  rename(
    CRP_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
CovidCT <- read.csv(file("CovidCT.csv"))
CovidCT['Measure']='CovidCT'
CovidCT$Specimen.Date = as.Date(CovidCT$Specimen.Date, format="%d/%m/%Y")
CovidCT = CovidCT %>% 
  rename(
    date = Specimen.Date,
  )
# Export CovidCT
write.csv(CovidCT,paste(output_dir,"covidCT.csv",sep=""))
## ------------------------------------------------------------------------------
DDM <- read.csv(file("DDM.csv"))
DDM$Date.Booked.In = as.Date(DDM$Date.Booked.In, format="%d/%m/%Y")
DDM = DDM %>% 
  rename(
    date = Date.Booked.In,
  )
DDM = DDM %>%
  rename(
    DDM_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
eGFR <- read.csv(file("eGFR.csv"))
eGFR$Date.Booked.In = as.Date(eGFR$Date.Booked.In, format="%d/%m/%Y")
eGFR = eGFR %>% 
  rename(
    date = Date.Booked.In,
  )
eGFR = eGFR %>%
  rename(
    eGFR_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
FER <- read.csv(file("FER.csv"))
FER$Date.Booked.In = as.Date(FER$Date.Booked.In, format="%d/%m/%Y")
FER = FER %>% 
  rename(
    date = Date.Booked.In,
  )
FER = FER %>%
  rename(
    FER_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
fib <- read.csv(file("fib.csv"))
fib$Date.Booked.In = as.Date(fib$Date.Booked.In, format="%d/%m/%Y")
fib = fib %>% 
  rename(
    date = Date.Booked.In,
  )
fib = fib %>%
  rename(
    fib_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
Glucose <- read.csv(file("Glucose.csv"))
Glucose$Date.Booked.In = as.Date(Glucose$Date.Booked.In, format="%d/%m/%Y")
Glucose = Glucose %>% 
  rename(
    date = Date.Booked.In,
  )
Glucose = Glucose %>%
  rename(
    Glucose_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
HB <- read.csv(file("HB.csv"))
HB$Date.Booked.In = as.Date(HB$Date.Booked.In, format="%d/%m/%Y")
HB = HB %>% 
  rename(
    date = Date.Booked.In,
  )
HB = HB %>%
  rename(
    HB_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
HBA1c <- read.csv(file("HBA1c.csv"))
HBA1c$Date.Booked.In = as.Date(HBA1c$Date.Booked.In, format="%d/%m/%Y")
HBA1c = HBA1c %>% 
  rename(
    date = Date.Booked.In,
  )
HBA1c = HBA1c %>%
  rename(
    HBA1c_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
LDH <- read.csv(file("LDH.csv"))
LDH$Date.Booked.In = as.Date(LDH$Date.Booked.In, format="%d/%m/%Y")
LDH = LDH %>% 
  rename(
    date = Date.Booked.In,
  )
LDH = LDH %>%
  rename(
    LDH_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
PCT <- read.csv(file("PCT.csv"))
PCT$Date.Booked.In = as.Date(PCT$Date.Booked.In, format="%d/%m/%Y")
PCT = PCT %>% 
  rename(
    date = Date.Booked.In,
  )
PCT = PCT %>%
  rename(
    PCT_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
PLT <- read.csv(file("PLT.csv"))
PLT$Date.Booked.In = as.Date(PLT$Date.Booked.In, format="%d/%m/%Y")
PLT = PLT %>% 
  rename(
    date = Date.Booked.In,
  )
PLT = PLT %>%
  rename(
    PLT_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
trig <- read.csv(file("trig.csv"))
trig$Date.Booked.In = as.Date(trig$Date.Booked.In, format="%d/%m/%Y")
trig = trig %>% 
  rename(
    date = Date.Booked.In,
  )
trig = trig %>%
  rename(
    trig_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
trop <- read.csv(file("trop.csv"))
trop$Date.Booked.In = as.Date(trop$Date.Booked.In, format="%d/%m/%Y")
trop = trop %>% 
  rename(
    date = Date.Booked.In,
  )
trop = trop %>%
  rename(
    trop_val = Numeric.Result
  )
## ------------------------------------------------------------------------------
Vir <- read.csv(file("Vir.csv"))
Vir['Measure']='Vir'
Vir$Sample.Date = as.Date(Vir$Sample.Date, format="%d/%m/%Y")
Vir = Vir %>% 
  rename(
    date = Sample.Date,
  )
Vir = Vir %>% 
  rename(
    Adenovirus = Adenovirus..PCR.,
  )
Vir = Vir %>% 
  rename(
    Human_Metapneumovirus = Human.metapneumovirus..PCR.,
  )
Vir = Vir %>% 
  rename(
    Influenza_A = Influenza.A..PCR.,
  )
Vir = Vir %>% 
  rename(
    Influenza_B = Influenza.B..PCR.,
  )
Vir = Vir %>% 
  rename(
    Parainfluenza_Type_1 = Parainfluenza.Type.1..PCR.,
  )
Vir = Vir %>% 
  rename(
    Parainfluenza_Type_2 = Parainfluenza.Type.2..PCR.,
  )
Vir = Vir %>% 
  rename(
    Parainfluenza_Type_3 = Parainfluenza.Type.3..PCR.,
  )
Vir = Vir %>% 
  rename(
    Parainfluenza_Type_4 = Parainfluenza.Type.4..PCR.,
  )
Vir = Vir %>% 
  rename(
    Respiratory_Syncytial_Virus = Respiratory.Syncytial.Virus..PCR.,
  )
Vir = Vir %>% 
  rename(
    Rhinovirus = Rhinovirus..PCR.,
  )
## ------------------------------------------------------------------------------
FBC <- read.csv(file("FBC.csv"))
FBC$Date.Booked.In = as.Date(FBC$Date.Booked.In, format="%d/%m/%Y")
FBC = FBC %>% 
  rename(
    date = Date.Booked.In,
  )
## ------------------------------------------------------------------------------
FBCLymph = FBC %>% select(uob_ID,date,Result.Lymphocytes)
FBCLymph = FBCLymph %>% 
  rename(
    Lymphocytes = Result.Lymphocytes,
  )
## ------------------------------------------------------------------------------
FBCNeutr = FBC %>% select(uob_ID,date,Result.Neutrophils)
FBCNeutr = FBCNeutr %>% 
  rename(
    Neutrophils = Result.Neutrophils,
  )
## ------------------------------------------------------------------------------
FBCWCC = FBC %>% select(uob_ID,date,Result.WCC)
FBCWCC = FBCWCC %>% 
  rename(
    WCC = Result.WCC,
  )
## ------------------------------------------------------------------------------
FBCNLR = FBC %>% select(uob_ID,date,NLR)
FBCNLR = FBCNLR %>% 
  rename(
    NLR_val = NLR,
  )
## ------------------------------------------------------------------------------
CovidCT <- read.csv(file("CovidCT.csv"))
CovidCT['Measure']='CovidCT'
CovidCT$Specimen.Date = as.Date(CovidCT$Specimen.Date, format="%d/%m/%Y")
CovidCT = CovidCT %>% 
  rename(
    date = Specimen.Date,
  )
## ------------------------------------------------------------------------------
Clot <- read.csv(file("Clot.csv"))
Clot$Date.Booked.In = as.Date(Clot$Date.Booked.In, format="%d/%m/%Y")
Clot = Clot %>% 
  rename(
    date = Date.Booked.In,
  )
## ------------------------------------------------------------------------------
ClotAPTT = Clot %>% select(uob_ID,date,APTT)
ClotAPTT = ClotAPTT %>% 
  rename(
    APTT_val = APTT,
  )
## ------------------------------------------------------------------------------
ClotPT = Clot %>% select(uob_ID,date,PT)
ClotPT = ClotPT %>% 
  rename(
    PT_val = PT,
  )
## ------------------------------------------------------------------------------
Antigen <- read.csv(file("Antigen.csv"))
Antigen = Antigen %>% 
  rename(
    date = Date.of.Specimen,
  )
## ------------------------------------------------------------------------------
BC <- read.csv(file("BC.csv"))
BC['Measure']='BC'
BC$Date.of.Specimen = as.Date(BC$Date.of.Specimen, format="%d/%m/%Y")
BC = BC %>% 
  rename(
    date = Date.of.Specimen,
  )
## ------------------------------------------------------------------------------
Resp <- read.csv(file("Resp.csv"))
Resp['Measure']='Resp'
Resp$Date.of.Specimen = as.Date(Resp$Date.of.Specimen, format="%d/%m/%Y")
Resp = Resp %>% 
  rename(
    date = Date.of.Specimen,
  )
## ------------------------------------------------------------------------------
Urine <- read.csv(file("Urine.csv"))
Urine['Measure']='Urine'
Urine$Date.of.Specimen = as.Date(Urine$Date.of.Specimen, format="%d/%m/%Y")
Urine = Urine %>% 
  rename(
    date = Date.of.Specimen,
  )
## ------------------------------------------------------------------------------
poctLAC <- read.csv(file("poctLAC.csv"))
#Format date
poctLAC$Date.of.Specimen = as.Date(poctLAC$Date.of.Specimen, format="%d/%m/%Y")
poctLAC = poctLAC %>% 
  rename(
    date = Date.of.Specimen,
  )
poctLAC = poctLAC %>% 
  rename(
    time = Time.of.Specimen,
  )
poctLAC = poctLAC %>%
  rename(
    poctLAC_val = Numeric.Result
  )
#Combine date and time
poctLAC$dateTime = as.POSIXct(paste(poctLAC$date, poctLAC$time), format="%Y-%m-%d %H:%M:%S")
## ------------------------------------------------------------------------------
poctO2 <- read.csv(file("poctO2.csv"))
## ------------------------------------------------------------------------------
O2 <- poctO2 %>%
  filter(Test.Desc == "Arterial pO2")
O2$Date.of.Specimen = as.Date(O2$Date.of.Specimen, format="%d/%m/%Y")
O2 = O2 %>% 
  rename(
    date = Date.of.Specimen,
  )
O2 = O2 %>% 
  rename(
    time = Time.of.Specimen,
  )
O2 = O2 %>% 
  rename(
    O2_val = Numeric.Result,
  )
#Combine date and time
O2$dateTime = as.POSIXct(paste(O2$date, O2$time), format="%Y-%m-%d %H:%M:%S")
## ------------------------------------------------------------------------------
CO2 <- poctO2 %>%
  filter(Test.Desc == "Arterial pCO2")
#Format date
CO2$Date.of.Specimen = as.Date(CO2$Date.of.Specimen, format="%d/%m/%Y")
CO2 = CO2 %>% 
  rename(
    date = Date.of.Specimen,
  )
CO2 = CO2 %>% 
  rename(
    time = Time.of.Specimen,
  )
CO2 = CO2 %>% 
  rename(
    CO2_val = Numeric.Result,
  )
#Combine date and time
CO2$dateTime = as.POSIXct(paste(CO2$date, CO2$time), format="%Y-%m-%d %H:%M:%S")
## ------------------------------------------------------------------------------
poctpH <- read.csv(file("poctpH.csv"))
#Format date
poctpH$Date.of.Specimen = as.Date(poctpH$Date...Time.of.Specimen, format="%d/%m/%Y")
poctpH = poctpH %>% 
  rename(
    date = Date...Time.of.Specimen,
  )
poctpH = poctpH %>% 
  rename(
    time = Time.of.Specimen,
  )
poctpH = poctpH %>% 
  rename(
    poctpH_val = Numeric.Result,
  )
#Combine date and time
poctpH$dateTime = as.POSIXct(paste(poctpH$date, poctpH$time), format="%Y-%m-%d %H:%M:%S")
## ------------------------------------------------------------------------------
AvonCap <- read.csv(file("AvonCap.Update.csv"))
AvonCap = AvonCap %>%
    rename(
        Community_Acquired_Pneumonia_radiologically_confirmed = Final.Standard.of.Care.LRTD.related.diagnosis..choice.CAP...radiologically.confirmed.,
        Community_Acquired_Pneumonia_clinically_confirmed = Final.Standard.of.Care.LRTD.related.diagnosis..choice.CAP...clinically.confirmed..but.not.on.radiology..,
        Community_Acquired_Pneumonia_no_radiology_performed = Final.Standard.of.Care.LRTD.related.diagnosis..choice.CAP...no.radiology.performed.,
        Acute_bronchitis = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Acute.bronchitis.,
        Exacerbation_of_COPD = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Exacerbation.of.COPD.,
        Empyema_lung_abscess = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Empyema.lung.abscess.,
        LRTI_not_further_specified = Final.Standard.of.Care.LRTD.related.diagnosis..choice.LRTI...not.further.specified.,
        Congestive_heart_failure = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Congestive.heart.failure.,
        Non_infectious_process = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Non.infectious.process.,
        Non_LRTD_infection_related_diagnosis = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Non.LRTD.infection.related.diagnosis.,
        Other_LRTI_specified = Other.LRTI.Specified,
        NYHA_Heart_failure = NYHA...Heart.Failure..nbsp.,
        CRB65_Score = CRB65.Score,
        NEWS2_Score = NEWS.2.Score,
        Respiratory_Disease_None = Respiratory.Disease..choice.None.,
        COPD = Respiratory.Disease..choice.COPD..Chronic.Obstructive.Pulmonary.Disease.Emphysema..,
        Asthma = Respiratory.Disease..choice.Asthma.,
        Bronchiectasis = Respiratory.Disease..choice.Bronchiectasis.,
        Pulmonary_Fibrosis_Interstitial_Lung_Disease = Respiratory.Disease..choice.Pulmonary.Fibrosis.Interstitial.Lung.Disease.,
        Respiratory_Disease_other = Respiratory.Disease..choice.Other.,
        Chronic_heart_disease_none = Chronic.Heart.Disease..choice.None.,
        Hypertension = Chronic.Heart.Disease..choice.Hypertension.,
        Atrial_Fibrillation = Chronic.Heart.Disease..choice.Atrial.Fibrillation.,
        Ischaemic_heart_disease = Chronic.Heart.Disease..choice.Ischaemic.heart.disease.,
        Heart_failure = Chronic.Heart.Disease..choice.Heart.failure.CCF.,
        Chronic_heart_disease_other = Chronic.Heart.Disease..choice.Other.,
        Chronic_Kidney_Disease = Chronic.Kidney.Disease..CKD..Mod.Severe..eGFR..30..Cr.265.umol.L..dialysis..transplantation..uremic.syndrome,
        Liver_disease = Liver.Disease.Mild...nbsp.cirrhosis.without.portal.HTN..chronic.hepatitis..Mod.Severe...nbsp.cirrhosis.with.portal.HTN.....variceal.bleeding,
        Diabetes = Diabetes,
        Cognitive_Impairment_Dementia_none = Cognitive.Impairment.Dementia..choice.None.,
        Dementia = Cognitive.Impairment.Dementia..choice.Dementia.,
        Cognitive_impairment = Cognitive.Impairment.Dementia..choice.Cognitive.Impairment.,
        CVA_Stroke = Cognitive.Impairment.Dementia..choice.CVA..stroke..,
        TIA_mini_stroke = Cognitive.Impairment.Dementia..choice.TIA..mini.stroke..,
        Hemiplegiahemiplegia_or_paraplegia = Hemiplegiahemiplegia.or.paraplegia,
        Peripheral_vascular_disease = Peripheral.Vascular.Disease.Intermittent.claudication..periph..arterial.bypass.for.insufficiency..gangrene..acute.arterial.insufficiency..untreated.aneurysm....6cm..nbsp.,
        Immunosuppressive_medication = Immunosuppressive.Medication.includes.oral.steroids..biologics..chemotherapy.,
        Immunodeficiency = Immunodeficiency.eg.SCID..hypogammaglobulinaemia..splenectomy.,
        Connective_tissue_disease = Connective.Tissue.Disease..SLE..polymyositis..mixed.nbsp.Connective.Tissue.Disease..polymyalgia.rheumatica..moderate.to.severe.Rheumatoid.Arthritis.,
        HIV_negative_or_not_tested = HIV.status..choice.Negative..no.HIV...or.not.tested.,
        HIV_positive = HIV.status..choice.HIV.,
        AIDS = HIV.status..choice.AIDS.,
        Solid_organ_cancer_malignancy = Solid.Organ.Cancer.Malignancy.Initially.treated.in.the.last.5.years.exclude.non.melanomatous.skin.cancers.and.in.situ.cervical.carcinoma,
        Haematological_malignancy_leukaemia_none = Haematological.Malignancy.Leukaemia...nbsp.CML..CLL..AML..ALL..Polycythaemia.Vera.Lymphoma...nbsp.NHL..Hodgkin.s..WaldenstrÃ.m..multiple.myeloma...choice.None.,
        Leukaemia = Haematological.Malignancy.Leukaemia...nbsp.CML..CLL..AML..ALL..Polycythaemia.Vera.Lymphoma...nbsp.NHL..Hodgkin.s..WaldenstrÃ.m..multiple.myeloma...choice.Leukaemia.,
        Lymphoma = Haematological.Malignancy.Leukaemia...nbsp.CML..CLL..AML..ALL..Polycythaemia.Vera.Lymphoma...nbsp.NHL..Hodgkin.s..WaldenstrÃ.m..multiple.myeloma...choice.Lymphoma.,
        Organ_transplantation = Organ.Transplantation,
        Pregnancy_post_partum = Pregnancy.Post.partum,
        Gastric_Duodenal_Ulcer_disease = Gastric.Duodenal.Ulcer.Disease.Patients.who.have.required.treatment.for.PUD.nbsp.,
        Rockwood_frailty_score = Rockwood.Frailty.Score,
        Radiology_result = Radiology.Result
    )
## ------------------------------------------------------------------------------
BE_transform <- subset(BE, BE_val < 57.5)
## ------------------------------------------------------------------------------
BNP_transform <- BNP
BNP_transform$BNP_val <- log10(BNP_transform$BNP_val)
## ------------------------------------------------------------------------------
ClotAPTT_transform <- ClotAPTT
ClotAPTT_transform$APTT_val <- log10(ClotAPTT_transform$APTT_val)
## ------------------------------------------------------------------------------
ClotPT_transform <- ClotPT
ClotPT_transform$PT_val <- log10(ClotPT_transform$PT_val)
## ------------------------------------------------------------------------------
CO2_transform <- CO2
CO2_transform$CO2_val <- log10(CO2_transform$CO2_val)
## ------------------------------------------------------------------------------
O2_transform <- O2
O2_transform$O2_val <- log10(O2_transform$O2_val)
## ------------------------------------------------------------------------------
CRP_transform <- CRP
CRP_transform$CRP_val <- (CRP_transform$CRP_val)^(1/3)
## ------------------------------------------------------------------------------
DDM_transform <- DDM
DDM_transform$DDM_val <- log10(DDM_transform$DDM_val)
## ------------------------------------------------------------------------------
eGFR_transform <- eGFR
## ------------------------------------------------------------------------------
FBCLymph_transform <- FBCLymph
FBCLymph_transform$Lymphocytes <- log10(FBCLymph_transform$Lymphocytes)
## ------------------------------------------------------------------------------
FBCNeutr_transform <- FBCNeutr
FBCNeutr_transform$Neutrophils <- (FBCNeutr_transform$Neutrophils)^(1/3)
## ------------------------------------------------------------------------------
FBCNLR_transform <- FBCNLR
FBCNLR_transform$NLR_val <- log10(FBCNLR_transform$NLR_val)
## ------------------------------------------------------------------------------
FBCWCC_transform <- FBCWCC
FBCWCC_transform$WCC <- (FBCWCC_transform$WCC)^(1/3)
## ------------------------------------------------------------------------------
FER_transform <- FER
FER_transform$FER_val <- log10(FER_transform$FER_val)
## ------------------------------------------------------------------------------
fib_transform <- fib
## ------------------------------------------------------------------------------
Glucose_transform <- Glucose
Glucose_transform$Glucose_val <- log10(Glucose_transform$Glucose_val)
## ------------------------------------------------------------------------------
HB_transform <- HB
## ------------------------------------------------------------------------------
HBA1c_transform <- HBA1c
HBA1c_transform$HBA1c_val <- (HBA1c_transform$HBA1c_val)^(1/3)
## ------------------------------------------------------------------------------
LDH_transform <- LDH
LDH_transform$LDH_val <- log10(LDH_transform$LDH_val)
## ------------------------------------------------------------------------------
PCT_transform <- PCT
PCT_transform$PCT_val <- log10(PCT_transform$PCT_val)
## ------------------------------------------------------------------------------
PLT_transform <- PLT
PLT_transform$PLT_val <- sqrt(PLT_transform$PLT_val)
## ------------------------------------------------------------------------------
poctLAC_transform <- poctLAC
poctLAC_transform$poctLAC_val <- log10(poctLAC_transform$poctLAC_val)
## ------------------------------------------------------------------------------
poctpH_transform <- poctpH
## ------------------------------------------------------------------------------
trig_transform <- trig
trig_transform$trig_val <- log10(trig_transform$trig_val)
## ------------------------------------------------------------------------------
trop_transform <- trop
trop_transform$trop_val <- log10(trop_transform$trop_val)
## ------------------------------------------------------------------------------
BE_transform$BE_val <- rescale(BE_transform$BE_val)
BE_transform = BE_transform %>% select(uob_ID,date,BE_val)
BE_transform['Measure'] = 'BE'
BE_transform = BE_transform %>%
    rename ( 
        Numeric.Result = BE_val
    )
BNP_transform$BNP_val <- rescale(BNP_transform$BNP_val)
BNP_transform = BNP_transform %>% select(uob_ID,date,BNP_val)
BNP_transform['Measure'] = 'BNP'
BNP_transform = BNP_transform %>%
    rename ( 
        Numeric.Result = BNP_val
    )
CRP_transform$CRP_val <- rescale(CRP_transform$CRP_val)
CRP_transform = CRP_transform %>% select(uob_ID,date,CRP_val)
CRP_transform['Measure'] = 'CRP'
CRP_transform = CRP_transform %>%
    rename ( 
        Numeric.Result = CRP_val
    )
DDM_transform$DDM_val <- rescale(DDM_transform$DDM_val)
DDM_transform = DDM_transform %>% select(uob_ID,date,DDM_val)
DDM_transform['Measure'] = 'DDM'
DDM_transform = DDM_transform %>%
    rename ( 
        Numeric.Result = DDM_val
    )
eGFR_transform$eGFR_val <- rescale(eGFR_transform$eGFR_val)
eGFR_transform = eGFR_transform %>% select(uob_ID,date,eGFR_val)
eGFR_transform['Measure'] = 'eGFR'
eGFR_transform = eGFR_transform %>%
    rename ( 
        Numeric.Result = eGFR_val
    )
FER_transform$FER_val <- rescale(FER_transform$FER_val)
FER_transform = FER_transform %>% select(uob_ID,date,FER_val)
FER_transform['Measure'] = 'FER'
FER_transform = FER_transform %>%
    rename ( 
        Numeric.Result = FER_val
    )
fib_transform$fib_val <- rescale(fib_transform$fib_val)
fib_transform = fib_transform %>% select(uob_ID,date,fib_val)
fib_transform['Measure'] = 'fib'
fib_transform = fib_transform %>%
    rename ( 
        Numeric.Result = fib_val
    )
Glucose_transform$Glucose_val <- rescale(Glucose_transform$Glucose_val)
Glucose_transform = Glucose_transform %>% select(uob_ID,date,Glucose_val)
Glucose_transform['Measure'] = 'Glucose'
Glucose_transform = Glucose_transform %>%
    rename ( 
        Numeric.Result = Glucose_val
    )
HB_transform$HB_val <- rescale(HB_transform$HB_val)
HB_transform = HB_transform %>% select(uob_ID,date,HB_val)
HB_transform['Measure'] = 'HB'
HB_transform = HB_transform %>%
    rename ( 
        Numeric.Result = HB_val
    )
ClotAPTT_transform$APTT_val <- rescale(ClotAPTT_transform$APTT_val)
ClotAPTT_transform = ClotAPTT_transform %>% select(uob_ID,date,APTT_val)
ClotAPTT_transform['Measure'] = 'APTT'
ClotAPTT_transform = ClotAPTT_transform %>%
    rename ( 
        Numeric.Result = APTT_val
    )
ClotPT_transform$PT_val <- rescale(ClotPT_transform$PT_val)
ClotPT_transform = ClotPT_transform %>% select(uob_ID,date,PT_val)
ClotPT_transform['Measure'] = 'PT'
ClotPT_transform = ClotPT_transform %>%
    rename ( 
        Numeric.Result = PT_val
    )
CO2_transform$CO2_val <- rescale(CO2_transform$CO2_val)
CO2_transform = CO2_transform %>% select(uob_ID,date,CO2_val)
CO2_transform['Measure'] = 'CO2'
CO2_transform = CO2_transform %>%
    rename ( 
        Numeric.Result = CO2_val
    )
O2_transform$O2_val <- rescale(O2_transform$O2_val)
O2_transform = O2_transform %>% select(uob_ID,date,O2_val)
O2_transform['Measure'] = 'O2'
O2_transform = O2_transform %>%
    rename ( 
        Numeric.Result = O2_val
    )
FBCLymph_transform$Lymphocytes <- rescale(FBCLymph_transform$Lymphocytes)
FBCLymph_transform = FBCLymph_transform %>% select(uob_ID,date,Lymphocytes)
FBCLymph_transform['Measure'] = 'Lymphocytes'
FBCLymph_transform = FBCLymph_transform %>%
    rename ( 
        Numeric.Result = Lymphocytes
    )
FBCNeutr_transform$Neutrophils <- rescale(FBCNeutr_transform$Neutrophils)
FBCNeutr_transform = FBCNeutr_transform %>% select(uob_ID,date,Neutrophils)
FBCNeutr_transform['Measure'] = 'Neutrophils'
FBCNeutr_transform = FBCNeutr_transform %>%
    rename ( 
        Numeric.Result = Neutrophils
    )
FBCNLR_transform$NLR_val <- rescale(FBCNLR_transform$NLR_val)
FBCNLR_transform = FBCNLR_transform %>% select(uob_ID,date,NLR_val)
FBCNLR_transform['Measure'] = 'NLR'
FBCNLR_transform = FBCNLR_transform %>%
    rename ( 
        Numeric.Result = NLR_val
    )
FBCWCC_transform$WCC <- rescale(FBCWCC_transform$WCC)
FBCWCC_transform = FBCWCC_transform %>% select(uob_ID,date,WCC)
FBCWCC_transform['Measure'] = 'WCC'
FBCWCC_transform = FBCWCC_transform %>%
    rename ( 
        Numeric.Result = WCC
    )
HBA1c_transform$HBA1c_val <- rescale(HBA1c_transform$HBA1c_val)
HBA1c_transform = HBA1c_transform %>% select(uob_ID,date,HBA1c_val)
HBA1c_transform['Measure'] = 'HBA1c'
HBA1c_transform = HBA1c_transform %>%
    rename ( 
        Numeric.Result = HBA1c_val
    )
poctpH_transform$poctpH_val <- rescale(poctpH_transform$poctpH_val)
poctpH_transform = poctpH_transform %>% select(uob_ID,date,poctpH_val)
poctpH_transform['Measure'] = 'poctpH'
poctpH_transform$date = as.Date(poctpH$date, format="%d/%m/%Y")
poctpH_transform = poctpH_transform %>%
    rename ( 
        Numeric.Result = poctpH_val
    )
poctLAC_transform$poctLAC_val <- rescale(poctLAC_transform$poctLAC_val)
poctLAC_transform = poctLAC_transform %>% select(uob_ID,date,poctLAC_val)
poctLAC_transform['Measure'] = 'poctLAC'
poctLAC_transform = poctLAC_transform %>%
    rename ( 
        Numeric.Result = poctLAC_val
    )
LDH_transform$LDH_val <- rescale(LDH_transform$LDH_val)
LDH_transform = LDH_transform %>% select(uob_ID,date,LDH_val)
LDH_transform['Measure'] = 'LDH'
LDH_transform = LDH_transform %>%
    rename ( 
        Numeric.Result = LDH_val
    )
PCT_transform$PCT_val <- rescale(PCT_transform$PCT_val)
PCT_transform = PCT_transform %>% select(uob_ID,date,PCT_val)
PCT_transform['Measure'] = 'PCT'
PCT_transform = PCT_transform %>%
    rename ( 
        Numeric.Result = PCT_val
    )
PLT_transform$PLT_val <- rescale(PLT_transform$PLT_val)
PLT_transform = PLT_transform %>% select(uob_ID,date,PLT_val)
PLT_transform['Measure'] = 'PLT'
PLT_transform = PLT_transform %>%
    rename ( 
        Numeric.Result = PLT_val
    )
trig_transform$trig_val <- rescale(trig_transform$trig_val)
trig_transform = trig_transform %>% select(uob_ID,date,trig_val)
trig_transform['Measure'] = 'trig'
trig_transform = trig_transform %>%
    rename ( 
        Numeric.Result = trig_val
    )
trop_transform$trop_val <- rescale(trop_transform$trop_val)
trop_transform = trop_transform %>% select(uob_ID,date,trop_val)
trop_transform['Measure'] = 'trop'
trop_transform = trop_transform %>%
    rename ( 
        Numeric.Result = trop_val
    )
## ------------------------------------------------------------------------------
labs <- rbind(BE_transform,BNP_transform,CRP_transform,DDM_transform,eGFR_transform,FER_transform,fib_transform,Glucose_transform,HB_transform,ClotAPTT_transform,ClotPT_transform,CO2_transform,O2_transform,FBCLymph_transform,FBCNeutr_transform,FBCNLR_transform,FBCWCC_transform,HBA1c_transform,poctpH_transform,poctLAC_transform,LDH_transform,PCT_transform,PLT_transform,trig_transform,trop_transform)
labs$Measure <- factor(total$Measure, levels = c("BNP","LDH","HBA1c","trig","trop","DDM","FER","PCT","fib","Glucose","PT","APTT","PLT","CRP","eGFR","poctLAC","poctpH","O2","CO2","WCC","Lymphocytes","Neutrophils","NLR","HB","BE"))
# Export labs DataFrame
write.csv(labs,paste(output_dir,"labs.csv",sep=""))
## ------------------------------------------------------------------------------
vir_info <- function(patientID) {
    cat(paste("#### Virology"))
    info <- filter(Vir,Vir$uob_ID==patientID)
    info <- info %>% select(uob_ID,date,SARS.CoV.2.RNA,Adenovirus,Human_Metapneumovirus,Influenza_A,Influenza_B,Parainfluenza_Type_1,Parainfluenza_Type_2,Parainfluenza_Type_3,Parainfluenza_Type_4,Respiratory_Syncytial_Virus,Rhinovirus)
    print(kable(info))
    cat("\n\n")
}
# Export virology DataFrame
write.csv(Vir,paste(output_dir,"virology.csv",sep=""))
## --------------------------------------------------------------
bc_info <- function(patientID) {
    cat("#### Blood Culture")
    info <- filter(BC,BC$uob_ID==patientID)
    print(kable(info))
    cat("\n\n")
}
# Export blood culture DataFrame
write.csv(BC,paste(output_dir,"blood_culture.csv",sep=""))
## --------------------------------------------------------------
avonCap_info <- function(patientID) {
    info <- filter(AvonCap,AvonCap$uob_ID==patientID)
    if(length(info[,1]) > 0){
        cat("#### AvonCap")
        info <- info %>% select(uob_ID,Community_Acquired_Pneumonia_radiologically_confirmed, Community_Acquired_Pneumonia_clinically_confirmed, Community_Acquired_Pneumonia_no_radiology_performed, Acute_bronchitis, Exacerbation_of_COPD, Empyema_lung_abscess, LRTI_not_further_specified, Congestive_heart_failure, Non_infectious_process, Non_LRTD_infection_related_diagnosis, Other_LRTI_specified, NYHA_Heart_failure, CRB65_Score, NEWS2_Score, Respiratory_Disease_None, COPD, Asthma, Bronchiectasis, Pulmonary_Fibrosis_Interstitial_Lung_Disease, Respiratory_Disease_other, Chronic_heart_disease_none, Hypertension, Atrial_Fibrillation, Ischaemic_heart_disease, Heart_failure, Chronic_heart_disease_other, Chronic_Kidney_Disease, Liver_disease, Diabetes, Cognitive_Impairment_Dementia_none, Dementia, Cognitive_impairment, CVA_Stroke, TIA_mini_stroke, Hemiplegiahemiplegia_or_paraplegia, Peripheral_vascular_disease, Immunosuppressive_medication, Immunodeficiency, Connective_tissue_disease, HIV_negative_or_not_tested, HIV_positive, AIDS, Solid_organ_cancer_malignancy, Haematological_malignancy_leukaemia_none, Leukaemia, Lymphoma, Organ_transplantation, Pregnancy_post_partum, Gastric_Duodenal_Ulcer_disease, Rockwood_frailty_score, Radiology_result)
        info <- as.data.frame(t(info))
        print(kable(info))
        cat("\n\n")
    }
}
# Export avoncap DataFrame
write.csv(AvonCap,paste(output_dir,"avoncap.csv",sep=""))

