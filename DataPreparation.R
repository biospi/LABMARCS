# Libraries
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
library(ggplot2)
library(finalfit)
library(data.table)

# Global variables
# BatchAnalysisOn==1 prevents the dateRange and readingwanted variables from
# being overwritten, but the option to turn this off and run this script without
# first running MasterAnalysis.R is available by setting BatchAnalysisOn==0 and then
# manually selecting the dateRange and readingwanted variables.
# Location of Rscripts & save intermediate processed files

if (!exists('work_path')) {
  work_path <- 'C:/Users/bs16044/OneDrive - University of Bristol/HDR-UK-AMR/LABMARCS/source/'
  output_path <- paste(work_path, 'output/', sep='')
}

# Load in original data files
if (!exists('data_path')) {
  data_path <- paste(work_path, 'data/', sep = '')
}

#Set needed variables if we are not running LABMARCS_Analysis_Batch.R
if (!exists('BatchAnalysisOn')) { 
  dateRange <- 3 # 1, 3, 5, 7, 14 days
  readingwanted <- 1 # 0-worst, 1-first, 2-mean
}

if (readingwanted == 0) {
  readingwanted_str <- 'worst'
} else if (readingwanted == 1) {
  readingwanted_str <- 'first'
} else if (readingwanted == 2) {
  readingwanted_str <- 'mean'
}

print(paste("Date range: ", dateRange))
print(paste("Reading wanted: ", readingwanted))


## Outcome data

### NBT Outcomes

setwd(data_path)

NBT_outcomes <- read.csv("NBT_outcomes.csv")
NBT_outcomes = NBT_outcomes %>% 
  rename(
    ID = uob_ID,
    admissionDate = admission_date,
    dischargeDate = discharge_date,
  )
NBT_outcomes$admissionDate = as.Date(NBT_outcomes$admissionDate, format = "%d/%m/%Y")
NBT_outcomes$dischargeDate = as.Date(NBT_outcomes$dischargeDate, format = "%d/%m/%Y")
NBT_outcomes_deaths = NBT_outcomes %>%
  filter(DischargeOutcomeDesc == "Patient Died")
NBT_outcomes_deaths$deathDate = NBT_outcomes_deaths$dischargeDate
NBT_outcomes <- merge(NBT_outcomes,NBT_outcomes_deaths,all = TRUE)
NBT_outcomes$ITU_Start = as.Date(NBT_outcomes$ITU_Start, format = "%d/%m/%Y")
NBT_outcomes$ITU_End = as.Date(NBT_outcomes$ITU_End, format = "%d/%m/%Y")
NBT_outcomes$Site = "NBT"
NBT_outcomes2 <- dplyr::select(NBT_outcomes,c(ID,admissionDate,dischargeDate,
                                       ITU_Start,ITU_End,deathDate,Site))


### UHB Outcomes
UHB_outcomes1 <- read.csv(file("UHB_outcomes1.ICU_Update.csv"))

UHB_outcomes1 = UHB_outcomes1 %>% 
  rename(
    ID = uob_ID,
    admissionDate = attend_date,
    deathDate = fu_death_date,
  )
UHB_outcomes1$admissionDate = as.Date(UHB_outcomes1$admissionDate, format = "%d/%m/%Y")
UHB_outcomes1$dischargeDate = as.Date(UHB_outcomes1$admissionDate + 
                                        as.integer(UHB_outcomes1$hospital_length_of_stay))
UHB_outcomes1$deathDate = as.Date(UHB_outcomes1$deathDate, format = "%d/%m/%Y")
UHB_outcomes1$ITU_Start <- as.Date(UHB_outcomes1$ITU_Start, format = "%d/%m/%Y")
UHB_outcomes1$ITU_End <- as.Date(UHB_outcomes1$ITU_End, format = "%d/%m/%Y")
UHB_outcomes1$Site = "UHB1"
UHB_outcomes12 = UHB_outcomes1 %>% dplyr::select(ID,admissionDate,dischargeDate,
                                                 ITU_Start,ITU_End,deathDate,Site)

UHB_outcomes2 <- read.csv(file("UHB_outcomes2.ICU_Update.csv"))

UHB_outcomes2 = UHB_outcomes2 %>% 
  rename(
    ID = uob_ID,
    admissionDate = attend_dte,
  )
UHB_outcomes2$admissionDate = as.Date(UHB_outcomes2$admissionDate, format = "%d/%m/%Y")
UHB_outcomes2 = UHB_outcomes2 %>% 
  rename(
    dischargeDate = outcome_dte,
  )
UHB_outcomes2$dischargeDate = as.Date(UHB_outcomes2$dischargeDate, format = "%d/%m/%Y")
UHB_outcomes2_deaths = UHB_outcomes2 %>%
  filter(outcome == 3)
UHB_outcomes2_deaths$deathDate = UHB_outcomes2_deaths$dischargeDate
UHB_outcomes2 <- merge(UHB_outcomes2,UHB_outcomes2_deaths,all = TRUE)
UHB_outcomes2$ITU_Start <- as.Date(UHB_outcomes2$ITU_Start, format = "%d/%m/%Y")
UHB_outcomes2$ITU_End <- as.Date(UHB_outcomes2$ITU_End, format = "%d/%m/%Y")
UHB_outcomes2$Site = "UHB2"
UHB_outcomes22 = UHB_outcomes2 %>% dplyr::select(ID,admissionDate,dischargeDate,
                                                 ITU_Start,ITU_End,deathDate,Site)


### Weston outcomes
WestonOutcomes <- read.csv(file("Weston.Outcomes.csv"))
WestonOutcomes = WestonOutcomes %>% 
  rename(
    ID = uob_ID,
    admissionDate = Admission.date,
    dischargeDate = Discharge.date,
    ITU_Start = ICU.Admission.Date,
    ITU_End = ICU.Discharge.Date,
    deathDate = Date.of.Death,
  )
WestonOutcomes$admissionDate = as.Date(WestonOutcomes$admissionDate, format = "%d/%m/%Y")
WestonOutcomes$dischargeDate = as.Date(WestonOutcomes$dischargeDate, format = "%d/%m/%Y")
WestonOutcomes$ITU_Start = as.Date(WestonOutcomes$ITU_Start, format = "%d/%m/%Y")
WestonOutcomes$ITU_End = as.Date(WestonOutcomes$ITU_End, format = "%d/%m/%Y")
WestonOutcomes$deathDate = as.Date(WestonOutcomes$deathDate, format = "%d/%m/%Y")
WestonOutcomes$Site = "Weston"
WestonOutcomes2 = WestonOutcomes %>% dplyr::select(ID,admissionDate,dischargeDate,
                                                   ITU_Start,ITU_End,deathDate, Site)


## Merge Outcome data
# NBT Outcomes must be excluded as inconsistent? 
# Not sure why Ed made this note - Ask!
totalOutcomes <- rbind(UHB_outcomes12,UHB_outcomes22,NBT_outcomes2,WestonOutcomes2) 
totalOutcomes$deathDate = as.Date(totalOutcomes$deathDate, format = "%d/%m/%Y")
totalOutcomes$ITU_Start = as.Date(totalOutcomes$ITU_Start, format = "%d/%m/%Y")
totalOutcomes$ITU_End = as.Date(totalOutcomes$ITU_End, format = "%d/%m/%Y")
totalOutcomes$dischargeDate = as.Date(totalOutcomes$dischargeDate, format = "%d/%m/%Y")


## Demographics Table
#Note master list has some IDs incorrectly saved should be 4 character code but
#export sometimes inteprets as scientific notation

dem <- read.csv(file("masterlist.update.csv")) #n=1159
dem <- dem %>%
  rename(ID = uob_ID, Age = agecat) %>%
  distinct(ID, .keep_all = TRUE) %>%
  dplyr::select(ID,Gender,Age)

## Merge outcomes with demographics (not only 944 unique IDs in outcomes)
total <- merge(totalOutcomes,dem, by = "ID") #n=991

# Remove those without admission information
total <- total %>% filter(!is.na(admissionDate))

# Missing discharge info
# Remove those without discharge information
total <- total %>% filter(!is.na(dischargeDate))

# Age less than 18
# Remove those less than 18
# Age (Age)
# Age grouped, make into average # NEED TO GET WORKING FOR NEW AGE DATA
total$Age[total$Age == "20-21"] <- "20.5"
total$Age[total$Age == "99-102"] <- "100.5"

total$Age <- as.numeric(total$Age)
total <- total %>% filter(Age >= 18)

### Those that have died
deceasedSubset = subset(total, !is.na(total$deathDate))
deceasedSubset$died = TRUE

### Add new column for died / didn't die to total
total = merge(total,deceasedSubset, all.x = TRUE)
total$died[is.na(total$died)] <- FALSE

### Those that went to ICU
ICU_subset = subset(totalOutcomes, !is.na(totalOutcomes$ITU_Start))
ICU_subset$went_to_icu = TRUE

### Add new column for went to ICU / didn't go to ICU
total = merge(total,ICU_subset, all.x = TRUE)
total$went_to_icu[is.na(total$went_to_icu)] <- FALSE

### Died or went to ICU
total$severeOutcome[total$went_to_icu == TRUE | total$died == TRUE] <- TRUE
total$severeOutcome[is.na(total$severeOutcome)] <- FALSE


## Add FIRST covid-positive date
CovidCT <- read.csv(file("CovidCT.csv"))
CovidCT['Measure'] = 'CovidCT'
CovidCT$Specimen.Date = as.Date(CovidCT$Specimen.Date, format = "%d/%m/%Y")
CovidCT = CovidCT %>% 
  rename(
     ID = uob_ID,
     date = Specimen.Date,
  )

uniqueIDs = unique(CovidCT$ID)
CovidCT['positiveDate'] <- as.Date(NA)

for (i in uniqueIDs) {
  for (j in length(CovidCT$date[CovidCT$ID == i])) {
      CovidCT[CovidCT$ID == i,]$positiveDate <- as.Date(CovidCT[CovidCT$ID == i,]$date[1], 
                                                        format = "%d%m%Y")
  }
}

CovidCT <- CovidCT %>% dplyr::select(ID,positiveDate) %>% distinct()

total <- left_join(total,CovidCT)

#exclude people without covid-19 test
total <- total[!is.na(total$positiveDate),]


# Number of people who have multiple admissions is 48 at this point
# All of them have covid before or during first stay so only use first stay
ctr <- 0
multi_admit_ids <- c()
for (id in unique(total$ID)) {
  id_match <- total$ID == id
  if (sum(id_match) > 1) {
    ctr <- ctr + 1
    multi_admit_ids[ctr] <- id
    #if more than one copy the first entry onto all others
    total[id_match,] <- total[id_match,][1,]
  }
}
#the final set of valid pateints for the study
total <- unique(total) #n=843

#------Establish Critical Date-----
#need to step through and choose the critical date for patients. if they are in 
#hospital its on covid positive. if they are not in hospital its when they are admitted
critDate = data.frame()

for (i in 1:dim(total)[1]) {

  pos_after_admin_num = total[i,"positiveDate"] - total[i,"admissionDate"] 
  pos_on_admin = pos_after_admin_num == 0
  pos_after_admin = pos_after_admin_num >= 0
    
  pos_before_discharge_num = total[i,"positiveDate"] - total[i,"dischargeDate"]  
  pos_on_discharge = pos_before_discharge_num == 0
  pos_before_discharge = pos_before_discharge_num < 0
  

  if (pos_on_admin & !pos_on_discharge) {
    #print('Covid on admit; use admit date/pos date')
    critDate[i,1] = total[i,"admissionDate"]
    critDate[i,2] = 0 #community
    critDate[i,3] = 1
    
  } else if (pos_after_admin & pos_before_discharge) {
      #print('Covid while admitted,; use pos date')
      #print(sprintf('pos to discharge time: %d', pos_before_discharge_num[[1]]))
      critDate[i,1] = total[i,"positiveDate"]
      critDate[i,2] = 1 #nosocomial
      critDate[i,3] = 2

  } else if (!pos_after_admin & pos_before_discharge) {
    #print('Covid before admit; use admit date')
    critDate[i,1] = total[i,"admissionDate"]
    critDate[i,2] = 0 #community
    critDate[i,3] = 3
    
      } else if (pos_on_admin & pos_on_discharge) {
    #print('Covid on admit and discharge same day')
    critDate[i,1] = total[i,"admissionDate"]
    critDate[i,2] = 1 #nosocomial
    critDate[i,3] = 4
    
    
  } else {
      #print('Already in hospital, covid positive & discharge on same day - Exclude')
      #print(sprintf('%s, %s, %s',total[i,"admissionDate"],
      #             total[i,"positiveDate"],total[i,"dischargeDate"]))
      #print(sprintf('%d, %d, %d, %d',pos_after_admin_num[[1]],pos_after_admin[[1]],
      #             pos_before_discharge_num[[1]],pos_before_discharge[[1]] ))
      critDate[i,1] = total[i,"positiveDate"]
      critDate[i,2] = 1 
      critDate[i,3] = 5
      
  }

  #Marker to delineate separation between patients if printing to console
  #print('---------------------------------------------------------------------')
}

#add new critical date column
total['CriticalDate'] = critDate[,1]
total['CriticalDateType'] = critDate[,3]
total['Nosocomial'] = critDate[,2]

#break down of tpyes of patient
hist(total$CriticalDateType,breaks = 0:5)$counts #405 343   53   29   13
hist(total$CriticalDateType,breaks = 0:5)$counts/dim(total)[1] #405 343   53   29   13
#a$counts/853
#0.47 0.40 0.00.06 0.000.033 0.00 0.015

#We could exclude cases where already admitted and covid was detected on day of discharge 
#Maha felt suymptoms/labs must be mild to get discharged so despite brief time they
#are still valid
#total = total[ critDate[,2] != 5, ]

#define 
ids_train = total[total$Site != 'Weston','ID']
ids_test = total[total$Site == 'Weston','ID']


### -- Load variable data ---
# Note: Code below is very repetitive - needs to compressed & efficiently
# written perhaps with function/class - note many idiosyncrasies across
# the biomarkers though, so for now keep as is


#### BE - Bicarbonate Excess
BE <- read.csv(file("BE.csv"))
BE$Date.Booked.In = as.Date(BE$Date.Booked.In, format = "%d/%m/%Y")

BE = BE %>% 
  rename(
    date = Date.Booked.In,
    ID = uob_ID,
    BE_val = Numeric.Result,
    BE_code = Test.Code,
    BE_desc = Test.Desc,
    BE_rnl = Result..Not.Large.
  )

BE <- BE[complete.cases(BE),]
BE2 <- data.frame(ID = character(), BE_val = character())

#create list with entries only for common ids
BE_common_indaterange <- data.frame(ID = character(), 
                                    BE_val = numeric(), BE_rnl = character(), 
                                    BE_code = character(), BE_desc = character())


commonIDs <- intersect(total$ID,BE$ID)

for (id in (unique(commonIDs))) {
  # Get all dates for this ID and variable
  idDates = BE[BE$ID == id,]$date
  # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID == id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = BE[BE$ID == id,][(idDates >= idPositive & idDates < 
                                   (idPositive + dateRange)),]
  
  # If all_values contains any values in date range, then proceed
  if (length(!is.na(all_values$BE_val)) > 0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values$BE_val,na.rm = TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values$BE_val,na.rm = TRUE))
    # Compare the lowest and highest values against the range, evaluate to true
    # if either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((lowest < 22 | highest > 29),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values$BE_val[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first < 22 | first > 29),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values$BE_val)
      value = if_else((average < 22 | average > 29),"Abnormal","Normal")
    }
    
    BE2 <- BE2 %>% add_row(ID = id,BE_val = value)
    
    #list with original values to verify above categorical transform
    #The 'date' data structure is a bit special - R insists that even though it appears
    #as a single row it is actually multiple could convert back to character but for now omit
    BE_common_indaterange <- BE_common_indaterange %>% 
      add_row(all_values[, c('ID','BE_val', 'BE_rnl','BE_code', 'BE_desc')])
    
  }
}

bio_mark_fr <- data.frame( test = character() )

for (id in unique(BE_common_indaterange$ID)){
  bio_mark_fr = add_row(bio_mark_fr,test = 
                          names(which.max(table(BE_common_indaterange[BE_common_indaterange$ID == id,'BE_code']))))
}

BE <- BE2
rm(BE2)



#### UREA
Urea <- read.csv(file("Urea.csv"))
Urea$Date.Booked.In = as.Date(Urea$Date.Booked.In, format = "%d/%m/%Y")
Urea = Urea %>% 
  rename(
    date = Date.Booked.In,
    ID = uob_ID,
    Urea_val = Numeric.Result,
    Urea_code = Test.Code,
    Urea_desc = Test.Desc,
    Urea_rnl = Result..Not.Large.
  )

Urea <- Urea[complete.cases(Urea),]
Urea2 <- data.frame(ID = character(), Urea_val = character())

#create list with entries only for common ids
Urea_common_indaterange <- data.frame(ID = character(), 
                              Urea_val = numeric(), Urea_rnl = character(), 
                              Urea_code = character(), Urea_desc = character())

commonIDs <- unique(intersect(total$ID,Urea$ID))

for (id in (commonIDs)) {
  # Get all dates for this ID and variable
  idDates = Urea[Urea$ID == id,]$date
  # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID == id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = Urea[Urea$ID == id,][(idDates >= idPositive & idDates < 
                                   (idPositive + dateRange)),]

  # If all_values contains any values, then proceed
  c1 = length(!is.na(all_values$Urea_val)) > 0
  c2 = !is.na(idPositive)
  
  if (c1 & c2) {
    
    if (readingwanted == 0) {
      # Determine the lowest value
      lowest = suppressWarnings(min(all_values$Urea_val,na.rm = TRUE))
      # Determine the highest value
      highest = suppressWarnings(max(all_values$Urea_val,na.rm = TRUE))
      # Compare the lowest and highest values against the range, evaluate to true
      # if either are outside the range, and assign "Abnormal", else "Normal"
      value = if_else((lowest < 2.5 | highest > 7),"Abnormal","Normal") #Philip says above 7.8 but NICE says 7
    }
    if (readingwanted == 1) {
      # select earliest test value in the list
      first <- all_values$Urea_val[1]
      # assign "Abnormal" if outside clinical range, else "Normal"
      value = if_else((first < 2.5 | first > 7),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values$Urea_val)
      value = if_else((average < 2.5 | average > 7),"Abnormal","Normal")
    }
    
    Urea2 <- Urea2 %>% add_row(ID = id,Urea_val = value)
    
    #list with original values to verify above categorical transform
    #The 'date' data structure is a bit special - R insists that even though it appears
    #as a single row it is actually multiple could convert back to character but for now omit
    Urea_common_indaterange <- Urea_common_indaterange %>% 
      add_row(all_values[, c('ID','Urea_val', 'Urea_rnl','Urea_code', 'Urea_desc')])
    
  } else { #use to debug why Urea seems to have 10% less data than expected

    #print(all_values$Urea_val)

  }
} #for (id in (unique(commonIDs)))

Urea <- Urea2
rm(Urea2)



#### BNP - B-type natriuretic peptide
BNP <- read.csv(file("BNP.csv"))
BNP$Date.Booked.In = as.Date(BNP$Date.Booked.In, format = "%d/%m/%Y")
BNP = BNP %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
BNP = BNP %>%
  rename(
    BNP_val = Numeric.Result
  )

BNP = BNP %>% dplyr::select(ID,date,BNP_val)
BNP <- BNP[complete.cases(BNP),]
BNP2 <- data.frame(ID = character(),
                  BNP_val = character())

commonIDs <- intersect(total$ID,BNP$ID)

for (id in unique(commonIDs)) {
  # Get all dates for this ID and variable
  idDates = BNP[BNP$ID == id,]$date
  # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID == id][1]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = BNP[BNP$ID == id,][(idDates >= idPositive & idDates < 
                                   (idPositive + dateRange)),]$BNP_val
  # If all_values contains any values, then proceed
  if (length(!is.na(all_values)) > 0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm = TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm = TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    age = total$Age[total$ID == id]
    gender = total$Gender[total$ID == id]

    if (age < 70) {
      # Men under 70
      if (gender == "M") {
        if (highest >= 100) {
          value = "Abnormal"
        } else {
          value = "Normal"
        }
      } else{
        # Women under 70
        if (highest >= 150) {
          value = "Abnormal"
        } else {
          value = "Normal"
        }
      }
    } else {
      # All 70 and over
      if (highest >= 300) {
        value = "Abnormal"
      } else {
        value = "Normal"
      }
    }
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    age = total$Age[total$ID == id]
    gender = total$Gender[total$ID == id]

    if (age < 70) {
      # Men under 70
      if (gender == "M") {
        if (first >= 100) {
          value = "Abnormal"
        } else {
          value = "Normal"
        }
      } else{
        # Women under 70
        if (first >= 150) {
          value = "Abnormal"
        } else {
          value = "Normal"
        }
      }
    } else {
      # All 70 and over
      if (first >= 300) {
        value = "Abnormal"
      } else {
        value = "Normal"
      }
    }
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      age = total$Age[total$ID == id]
    gender = total$Gender[total$ID == id]

    if (age < 70) {
      # Men under 70
      if (gender == "M") {
        if (average >= 100) {
          value = "Abnormal"
        } else {
          value = "Normal"
        }
      } else{
        # Women under 70
        if (average >= 150) {
          value = "Abnormal"
        } else {
          value = "Normal"
        }
      }
    } else {
      # All 70 and over
      if (average >= 300) {
        value = "Abnormal"
      } else {
        value = "Normal"
      }
    }
    }
    # Add this 
    BNP2 <- BNP2 %>% add_row(ID = id,BNP_val = value)
  }
}

BNP <- BNP2
rm(BNP2)



#### CRP - C-Reactive Protein
CRP <- read.csv(file("CRP.csv"))
CRP$Date.Booked.In = as.Date(CRP$Date.Booked.In, format = "%d/%m/%Y")
CRP = CRP %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
CRP = CRP %>%
  rename(
    CRP_val = Numeric.Result
  )

CRP = CRP %>% dplyr::select(ID,date,CRP_val)
CRP <- CRP[complete.cases(CRP),]
CRP2 <- data.frame(ID=character(),
                  CRP_val=character())
commonIDs <- intersect(total$ID,CRP$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = CRP[CRP$ID==id,]$date
  # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = CRP[CRP$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$CRP_val
  # If all_values contains any values, then proceed
  if(length(!is.na(all_values)) > 0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm = TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm = TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((highest >= 6),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first >= 6),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average >= 6),"Abnormal","Normal")
    }
    # Add this 
    CRP2 <- CRP2 %>% add_row(ID = id,CRP_val = value)
  }
}

CRP <- CRP2
rm(CRP2)



#### DDM - D-Dimer 
DDM <- read.csv(file("DDM.csv"))
#DDM_N <- DDM %>% 
#  filter(str_detect(Source.PCG.Code,"N")) %>%
#  transmute(
#    uob_ID = uob_ID,
#    Date.Booked.In = Date.Booked.In,
#    Source.PCG.Code = Source.PCG.Code,
#    Result..Not.Large.= Result..Not.Large.,
#    Numeric.Result = as.double(Result..Not.Large.)*1000
#    )

# DDM$Numeric.Result[str_detect(DDM$Source.PCG.Code,"N")] <- DDM_N$Numeric.Result
DDM$Date.Booked.In = as.Date(DDM$Date.Booked.In, format="%d/%m/%Y")
DDM = DDM %>% 
  rename(
    ID = uob_ID,
    date = Date.Booked.In,
    DDM_val = Numeric.Result,
  )

DDM = DDM %>% dplyr::select(ID,date,DDM_val)
DDM <- DDM[complete.cases(DDM),]
DDM2 <- data.frame(ID = character(),
                  DDM_val = character())
commonIDs <- intersect(total$ID,DDM$ID)

for (id in unique(commonIDs)) {
  # Get all dates for this ID and variable
  idDates = DDM[DDM$ID == id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID == id][1]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = DDM[DDM$ID == id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$DDM_val
  # If all_values contains any values, then proceed
  if (length(!is.na(all_values)) > 0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
      # Determine the lowest value
      lowest = suppressWarnings(min(all_values,na.rm = TRUE))
      # Determine the highest value
      highest = suppressWarnings(max(all_values,na.rm = TRUE))
      # Compare the lowest and highest values against the range, evaluate to true if
      # either are outside the range, and assign "Abnormal", else "Normal"
      age = total$Age[total$ID == id]
      
      if (age <= 60) {
        value = ifelse(highest >= 500,"Abnormal","Normal")
      }else if (age > 60 & age <= 70) {
        value = ifelse(highest >= 600,"Abnormal","Normal")
      }else if (age > 70 & age <= 80) {
        value = ifelse(highest >= 700,"Abnormal","Normal")
      }else if (age > 80 & age <= 90) {
        value = ifelse(highest >= 800,"Abnormal","Normal")
      }else {
        value = ifelse(highest >= 900,"Abnormal","Normal")
        }
    }
    
    if (readingwanted == 1) {
      # select earliest test value in the list
      first <- all_values[1]
      # assign "Abnormal" if outside clinical range, else "Normal"
      age = total$Age[total$ID == id]
      
      if (age <= 60) {
        value = ifelse(first >= 500,"Abnormal","Normal")
      }else if (age > 60 & age <= 70) {
        value = ifelse(first >= 600,"Abnormal","Normal")
      }else if (age > 70 & age <= 80) {
        value = ifelse(first >= 700,"Abnormal","Normal")
      }else if (age > 80 & age <= 90) {
        value = ifelse(first >= 800,"Abnormal","Normal")
      }else {
        value = ifelse(first >= 900,"Abnormal","Normal")
        }
    }
    
    if (readingwanted == 2) {
      average = mean(all_values)
      age = total$Age[total$ID == id]
      
      if (age <= 60) {
        value = ifelse(average >= 500,"Abnormal","Normal")
      }else if (age > 60 & age <= 70) {
        value = ifelse(average >= 600,"Abnormal","Normal")
      }else if (age > 70 & age <= 80) {
        value = ifelse(average >= 700,"Abnormal","Normal")
      }else if (age > 80 & age <= 90) {
        value = ifelse(average >= 800,"Abnormal","Normal")
      }else {
        value = ifelse(average >= 900,"Abnormal","Normal")
      }
    }
    # Add this 
    DDM2 <- DDM2 %>% add_row(ID = id,DDM_val = value)
  }
}

DDM <- DDM2
rm(DDM2)



#### eGFR - Estimated Glomerular Filtration Rate
eGFR <- read.csv(file("eGFR.csv"))
eGFR$Date.Booked.In = as.Date(eGFR$Date.Booked.In, format="%d/%m/%Y")
eGFR = eGFR %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
eGFR = eGFR %>%
  rename(
    eGFR_val = Numeric.Result
  )

eGFR$eGFR_val[eGFR$Result..Not.Large. == '>90'] <- 90.1
eGFR = eGFR %>% dplyr::select(ID,date,eGFR_val)
eGFR <- eGFR[complete.cases(eGFR),]
eGFR2 <- data.frame(ID = character(),
                  eGFR_val = character())
commonIDs <- intersect(total$ID,eGFR$ID)

for (id in (unique(commonIDs))) {
  # Get all dates for this ID and variable
  idDates = eGFR[eGFR$ID == id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID == id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = eGFR[eGFR$ID == id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$eGFR_val
  
  # If all_values contains any values, then proceed
  if (length(!is.na(all_values)) > 0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm = TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((lowest <= 90),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first <= 90),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average <= 90),"Abnormal","Normal")
    }
    # Add this 
    eGFR2 <- eGFR2 %>% add_row(ID = id,eGFR_val = value)
  }
}

eGFR <- eGFR2
rm(eGFR2)



#### FER - Ferritin
FER <- read.csv(file("FER.csv"))
FER$Date.Booked.In = as.Date(FER$Date.Booked.In, format="%d/%m/%Y")
FER = FER %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
FER = FER %>%
  rename(
    FER_val = Numeric.Result
  )

FER = FER %>% dplyr::select(ID,date,FER_val)
FER <- FER[complete.cases(FER),]
FER2 <- data.frame(ID=character(),
                  FER_val=character())
commonIDs <- intersect(total$ID,FER$ID)
fer_comparator <- function(v,age,gender){
  # upper limit of normal
  uln = 
  ifelse(gender == "M", 490,
  ifelse(age < 45, 445,470))
  
  value = 
  ifelse(v < uln, "Normal",
  ifelse(v < 735, "Mild",
  ifelse(v < 2450, "Moderate","Severe")))
  return(value)
}

for (id in unique(commonIDs)){
  # Get all dates for this ID and variable
  idDates = FER[FER$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id][1]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = FER[FER$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$FER_val
  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
   
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
  
    age = total$Age[total$ID==id]
    gender = total$Gender[total$ID==id]
    
    value = fer_comparator(highest,age,gender)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    age = total$Age[total$ID==id]
    gender = total$Gender[total$ID==id]

    value = fer_comparator(first,age,gender)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    age = total$Age[total$ID==id]
    gender = total$Gender[total$ID==id]

    value = fer_comparator(average,age,gender)
    }
    # Add this 
    FER2 <- FER2 %>% add_row(ID=id,FER_val=value)
  }
}

FER <- FER2
rm(FER2)



#### Fib - Fibrinogen
fib <- read.csv(file("fib.csv"))
fib$Date.Booked.In = as.Date(fib$Date.Booked.In, format="%d/%m/%Y")
fib = fib %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
fib = fib %>%
  rename(
    fib_val = Numeric.Result
  )

fib$fib_val[fib$Result..Not.Large.=='>6.0'] <- 6.1
fib = fib %>% dplyr::select(ID,date,fib_val)
fib <- fib[complete.cases(fib),]
fib2 <- data.frame(ID=character(),
                  fib_val=character())
commonIDs <- intersect(total$ID,fib$ID)

fib_comparator <- function(v){
  value = 
  ifelse(v < 1, "Severe",
  ifelse(v < 1.8, "Mild","Normal"))
  return(value)
}

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = fib[fib$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = fib[fib$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$fib_val
  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    
    value = fib_comparator(lowest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = fib_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = fib_comparator(average)
    }
    # Add this 
    fib2 <- fib2 %>% add_row(ID=id,fib_val=value)
  }
}

fib <- fib2
rm(fib2)



#### Glucose
Glucose <- read.csv(file("Glucose.csv"))
Glucose$Date.Booked.In = as.Date(Glucose$Date.Booked.In, format="%d/%m/%Y")
Glucose = Glucose %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
Glucose = Glucose %>%
  rename(
    Glucose_val = Numeric.Result
  )

Glucose = Glucose %>% dplyr::select(ID,date,Glucose_val)
Glucose <- Glucose[complete.cases(Glucose),]
Glucose2 <- data.frame(ID=character(),
                  Glucose_val=character())
commonIDs <- intersect(total$ID,Glucose$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = Glucose[Glucose$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = Glucose[Glucose$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$Glucose_val
  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((lowest < 3 | highest > 7.8),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first < 3 | first > 7.8),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average < 3 | average > 7.8),"Abnormal","Normal")
    }
    # Add this 
    Glucose2 <- Glucose2 %>% add_row(ID=id,Glucose_val=value)
  }
}

Glucose <- Glucose2
rm(Glucose2)



#### HB - Hemoglobin
HB <- read.csv(file("HB.csv"))
HB$Date.Booked.In = as.Date(HB$Date.Booked.In, format="%d/%m/%Y")
HB = HB %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
HB = HB %>%
  rename(
    HB_val = Numeric.Result
  )

HB = HB %>% dplyr::select(ID,date,HB_val)
HB <- HB[complete.cases(HB),]
HB2 <- data.frame(ID=character(),
                  HB_val=character())
commonIDs <- intersect(total$ID,HB$ID)

hb_comparator <- function(v,gender){
  # Lower limit of normal
  lln = ifelse(gender == "M", 130,120)
  
  value = 
  ifelse(v < 80, "Severe",
  ifelse(v < 100, "Moderate",
  ifelse(v < lln, "Mild","Normal")))
  return(value)
}

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = HB[HB$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = HB[HB$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$HB_val
  
  gender = total$Gender[total$ID==id]

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    value = hb_comparator(lowest,gender)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    value = hb_comparator(first,gender)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = hb_comparator(average,gender)   
    }
    # Add this 
    HB2 <- HB2 %>% add_row(ID=id,HB_val=value)
  }
}

HB <- HB2
rm(HB2)



#### HBA1c - glycated haemoglobin
HBA1c <- read.csv(file("HBA1c.csv"))
HBA1c$Date.Booked.In = as.Date(HBA1c$Date.Booked.In, format="%d/%m/%Y")
HBA1c = HBA1c %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
HBA1c = HBA1c %>%
  rename(
    HBA1c_val = Numeric.Result
  )

HBA1c = HBA1c %>% dplyr::select(ID,date,HBA1c_val)
HBA1c <- HBA1c[complete.cases(HBA1c),]
HBA1c2 <- data.frame(ID=character(),
                  HBA1c_val=character())
commonIDs <- intersect(total$ID,HBA1c$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = HBA1c[HBA1c$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = HBA1c[HBA1c$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$HBA1c_val
  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((highest >= 48),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first >= 48),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average >= 48),"Abnormal","Normal")
    }
    # Add this 
    HBA1c2 <- HBA1c2 %>% add_row(ID=id,HBA1c_val=value)
  }
}

HBA1c <- HBA1c2
rm(HBA1c2)



#### LDH - Lactate dehydrogenase
LDH <- read.csv(file("LDH.csv"))
LDH$Date.Booked.In = as.Date(LDH$Date.Booked.In, format="%d/%m/%Y")
LDH = LDH %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
LDH = LDH %>%
  rename(
    LDH_val = Numeric.Result
  )

LDH = LDH %>% dplyr::select(ID,date,LDH_val)
LDH <- LDH[complete.cases(LDH),]
LDH2 <- data.frame(ID=character(),
                  LDH_val=character())
commonIDs <- intersect(total$ID,LDH$ID)

LDH_comparator <- function(v){
  value = 
  ifelse(v > 1440, "Severe",
  ifelse(v > 720, "Moderate",
  ifelse(v > 480, "Mild","Normal")))
  return(value)
}

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = LDH[LDH$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = LDH[LDH$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$LDH_val

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    value = LDH_comparator(highest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = LDH_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = LDH_comparator(average)
    }
    # Add this 
    LDH2 <- LDH2 %>% add_row(ID=id,LDH_val=value)
  }
}

LDH <- LDH2
rm(LDH2)



#### PCT - Procalcitonin 
PCT <- read.csv(file("PCT.csv"))
PCT$Date.Booked.In = as.Date(PCT$Date.Booked.In, format="%d/%m/%Y")
PCT = PCT %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
PCT = PCT %>%
  rename(
    PCT_val = Numeric.Result
  )

PCT = PCT %>% dplyr::select(ID,date,PCT_val)
PCT <- PCT[complete.cases(PCT),]
PCT2 <- data.frame(ID=character(),
                  PCT_val=character())
commonIDs <- intersect(total$ID,PCT$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = PCT[PCT$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = PCT[PCT$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$PCT_val

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((highest >= 0.2),"Abnormal","Normal") #0.05 too low (some use .1)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first >= 0.2),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average >= 0.2),"Abnormal","Normal")
    }
    # Add this 
    PCT2 <- PCT2 %>% add_row(ID=id,PCT_val=value)
  }
}

PCT <- PCT2
rm(PCT2)



#### PLT - Platelet Count
PLT <- read.csv(file("PLT.csv"))
PLT$Date.Booked.In = as.Date(PLT$Date.Booked.In, format="%d/%m/%Y")
PLT = PLT %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
PLT = PLT %>%
  rename(
    PLT_val = Numeric.Result
  )

PLT = PLT %>% dplyr::select(ID,date,PLT_val)
PLT <- PLT[complete.cases(PLT),]
PLT2 <- data.frame(ID=character(),
                  PLT_val=character())
commonIDs <- intersect(total$ID,PLT$ID)

LDH_comparator <- function(v){
  value = 
  ifelse(v < 50, "Severe",
  ifelse(v < 100, "Moderate",
  ifelse(v < 150, "Mild","Normal")))
  return(value)
}

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = PLT[PLT$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = PLT[PLT$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$PLT_val

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    value = LDH_comparator(lowest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = LDH_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = LDH_comparator(average)
    }
    # Add this 
    PLT2 <- PLT2 %>% add_row(ID=id,PLT_val=value)
  }
}

PLT <- PLT2
rm(PLT2)



#### Trig - Triglycerides
trig <- read.csv(file("trig.csv"))
trig$Date.Booked.In = as.Date(trig$Date.Booked.In, format = "%d/%m/%Y")
trig = trig %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
trig = trig %>%
  rename(
    trig_val = Numeric.Result
  )

trig = trig %>% dplyr::select(ID,date,trig_val)
trig <- trig[complete.cases(trig),]
trig2 <- data.frame(ID = character(),
                  trig_val = character())
commonIDs <- intersect(total$ID,trig$ID)

for (id in (unique(commonIDs))) {
  # Get all dates for this ID and variable
  idDates = trig[trig$ID == id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID == id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = trig[trig$ID == id,][(idDates >= idPositive & idDates < 
                                       (idPositive + dateRange)),]$trig_val

  # If all_values contains any values, then proceed
  if (length(!is.na(all_values)) > 0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm = TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm = TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((lowest < 0.5 | highest > 1.7),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first < 0.5 | first > 1.7),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average < 0.5 | average > 1.7),"Abnormal","Normal")
    }
    # Add this 
    trig2 <- trig2 %>% add_row(ID = id,trig_val = value)
  }
}

trig <- trig2
rm(trig2)

#### Trop - Troponin
trop <- read.csv(file("trop.csv"))
trop$Date.Booked.In = as.Date(trop$Date.Booked.In, format="%d/%m/%Y")
trop = trop %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )
trop = trop %>%
  rename(
    trop_val = Numeric.Result
  )

trop = trop %>% dplyr::select(ID,date,trop_val)
trop <- trop[complete.cases(trop),]
trop2 <- data.frame(ID=character(),
                  trop_val=character())
commonIDs <- intersect(total$ID,trop$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = trop[trop$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = trop[trop$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$trop_val

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((highest >= 14),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
     value = if_else((first >= 14),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
       value = if_else((average >= 14),"Abnormal","Normal")
    }
    # Add this 
    trop2 <- trop2 %>% add_row(ID=id,trop_val=value)
  }
}

trop <- trop2
rm(trop2)

#### FBC - Full Blood Count
#Info on White Blood Cells, Neutrophils & Lymphocytes

FBC <- read.csv(file("FBC.csv"))
FBC$Date.Booked.In = as.Date(FBC$Date.Booked.In, format="%d/%m/%Y")
FBC = FBC %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )

#Lymphocytes
FBCLymph = FBC %>% dplyr::select(ID,date,Result.Lymphocytes)
FBCLymph = FBCLymph %>% 
  rename(
    Lymphocytes = Result.Lymphocytes,
  )

FBCLymph = FBCLymph %>% dplyr::select(ID,date,Lymphocytes)
FBCLymph <- FBCLymph[complete.cases(FBCLymph),]
FBCLymph2 <- data.frame(ID=character(),
                  Lymphocytes=character())
commonIDs <- intersect(total$ID,FBCLymph$ID)

Lymph_comparator <- function(v){
  value = 
  ifelse(v < 0.49, "Severe",
  ifelse(v < 0.99, "Moderate",
  ifelse(v < 1.49, "Mild",
  ifelse(v < 4.5, "Normal","Severe"))))
  return(value)
}

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = FBCLymph[FBCLymph$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = FBCLymph[FBCLymph$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$Lymphocytes

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = Lymph_comparator(highest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = Lymph_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = Lymph_comparator(average)
    }
    # Add this 
    FBCLymph2 <- FBCLymph2 %>% add_row(ID=id,Lymphocytes=value)
  }
}

FBCLymph <- FBCLymph2
rm(FBCLymph2)




#Neutrophils 
FBCNeutr = FBC %>% dplyr::select(ID,date,Result.Neutrophils)
FBCNeutr = FBCNeutr %>% 
  rename(
    Neutrophils = Result.Neutrophils,
  )

FBCNeutr = FBCNeutr %>% dplyr::select(ID,date,Neutrophils)
FBCNeutr <- FBCNeutr[complete.cases(FBCNeutr),]
FBCNeutr2 <- data.frame(ID=character(),
                  Neutrophils=character())
commonIDs <- intersect(total$ID,FBCNeutr$ID)

Neutr_comparator <- function(v){
  value = 
  ifelse(v < 0.5, "Severe",
  ifelse(v < 1, "Moderate",
  ifelse(v < 2, "Mild",
  ifelse(v < 7.5, "Normal", "Severe"))))
  return(value)
}

#Save individual readings for Manuscript Plot
FBCNeutr_train_ls = list()
FBCNeutr_test_ls = list()

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = FBCNeutr[FBCNeutr$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = FBCNeutr[FBCNeutr$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$Neutrophils

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    
    value = Neutr_comparator(lowest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = Neutr_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = Neutr_comparator(average)
    }
    # Add this 
    FBCNeutr2 <- FBCNeutr2 %>% add_row(ID = id,Neutrophils = value)
  }
  
  #Manuscript Plot - Filter by ids_train or ids_test to make selective histogram
  if (sum(id == ids_train) == 1) {
    #Append individual readings for Manuscript Plot
    FBCNeutr_train_ls = append(FBCNeutr_train_ls,all_values)
  } else {
    FBCNeutr_test_ls = append(FBCNeutr_test_ls,all_values)
  }
  
}

FBCNeutr <- FBCNeutr2
rm(FBCNeutr2)


#----Example plot for Paper
#Note FBCNeutr_train/test _ls is in loop above to save values after filtering patients

FBCNeutr_ls = FBCNeutr_train_ls #generates training data histogram
label_txt = 'Training Data'
ypl = 60
xl = ''

#FBCNeutr_ls = FBCNeutr_test_ls #generates testing data histogram
#label_txt = 'Validation Data'
#ypl = 30
#xl = 'Neutrophil Count x 10^9/L'

#First convert to tibble and plot histogram outside loop
FBCNeutr_df = tibble(Neutrophil = sapply(FBCNeutr_ls,c))

#Example Biomarker Plot for Paper
spl = 4
qplot(data = FBCNeutr_df, x = `Neutrophil`, geom = 'histogram',  fill = I("lightblue"), 
      col = I("darkblue"), binwidth = 0.5, xlab = xl, ylab = 'Frequency') +
  geom_histogram(color = "darkblue", fill = "lightblue", binwidth = 0.5) + 
  geom_vline(xintercept = 0.5,colour = "red") +
  geom_vline(xintercept = 1,colour = "red") +
  geom_vline(xintercept = 2,colour = "red") +
  geom_vline(xintercept = 7.5,colour = "red") +
  annotate("text", label = "Severe", x = 0.15, y = ypl, size = spl, 
           colour = "black", angle = 90, hjust = 0) +
  annotate("text", label = "Moderate", x = .75, y = ypl, size = spl, 
           colour = "black", angle = 90, hjust = 0) +
  annotate("text", label = "Mild", x = 1.5, y = ypl, size = spl, 
           colour = "black", angle = 90, hjust = 0) +
  annotate("label", label = "Normal", x = 5, y = ypl, size = spl, 
           colour = "black") +
  annotate("text", label = "Severe", x = 10, y = ypl, size = spl, 
           colour = "black", hjust = 0) +
  theme(text = element_text(size = 20)) + coord_cartesian(xlim = c(0, 35))

ggsave(paste(output_path,label_txt,'.BiomarkerDistribution_Neutrophil.pdf', sep = ''), dpi = 320 )
#-------------------------------




#WCC White cell count
FBCWCC = FBC %>% dplyr::select(ID,date,Result.WCC)
FBCWCC = FBCWCC %>% 
  rename(
    WCC = Result.WCC,
  )

FBCWCC = FBCWCC %>% dplyr::select(ID,date,WCC)
FBCWCC <- FBCWCC[complete.cases(FBCWCC),]
FBCWCC2 <- data.frame(ID=character(),
                  WCC=character())
commonIDs <- intersect(total$ID,FBCWCC$ID)

WCC_comparator <- function(v){
  value = 
  ifelse(v < 0.5, "Severe",
  ifelse(v < 1, "Moderate",
  ifelse(v < 4, "Mild",
  ifelse(v < 11, "Normal", "Severe"))))
  return(value)
}

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = FBCWCC[FBCWCC$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = FBCWCC[FBCWCC$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$WCC

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    
    value = WCC_comparator(lowest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = WCC_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = WCC_comparator(average)
    }
    # Add this 
    FBCWCC2 <- FBCWCC2 %>% add_row(ID=id,WCC=value)
  }
}

FBCWCC <- FBCWCC2
rm(FBCWCC2)

FBCNLR = FBC %>% dplyr::select(ID,date,NLR)
FBCNLR = FBCNLR %>% 
  rename(
    NLR_val = NLR,
  )

FBCNLR = FBCNLR %>% dplyr::select(ID,date,NLR_val)
FBCNLR <- FBCNLR[complete.cases(FBCNLR),]
FBCNLR2 <- data.frame(ID=character(),
                  NLR_val=character())

commonIDs <- intersect(total$ID,FBCNLR$ID)

NLR_comparator <- function(v){
  value = 
  ifelse(v < 3, "Normal",
  ifelse(v < 8, "Mild",
  ifelse(v < 18, "Moderate","Severe")))
  return(value)
}


for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = FBCNLR[FBCNLR$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = FBCNLR[FBCNLR$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$NLR_val

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    
    value = NLR_comparator(highest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = NLR_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = NLR_comparator(average)
    }
    # Add this 
    FBCNLR2 <- FBCNLR2 %>% add_row(ID=id,NLR_val=value)
  }
}

FBCNLR <- FBCNLR2
rm(FBCNLR2)


#### CovidCT - Cycle Threshold (CT) value of PCR for COVID
CovidCT <- read.csv(file("CovidCT.csv"))
CovidCT['Measure']='CovidCT'
CovidCT$Specimen.Date = as.Date(CovidCT$Specimen.Date, format="%d/%m/%Y")
CovidCT = CovidCT %>% 
  rename(
    date = Specimen.Date,
     ID = uob_ID,
  )

#### Clot
Clot <- read.csv(file("Clot.csv"))
Clot$Date.Booked.In = as.Date(Clot$Date.Booked.In, format="%d/%m/%Y")
Clot = Clot %>% 
  rename(
    date = Date.Booked.In,
     ID = uob_ID,
  )

ClotAPTT = Clot %>% dplyr::select(ID,date,APTT)
ClotAPTT = ClotAPTT %>% 
  rename(
    APTT_val = APTT,
  )

ClotAPTT = ClotAPTT %>% dplyr::select(ID,date,APTT_val)
ClotAPTT <- ClotAPTT[complete.cases(ClotAPTT),]
ClotAPTT2 <- data.frame(ID=character(),
                  APTT_val=character())
commonIDs <- intersect(total$ID,ClotAPTT$ID)

APTT_comparator <- function(v){
  value = 
  ifelse(v < 33, "Normal",
  ifelse(v < 49.5, "Mild",
  ifelse(v < 82.5, "Moderate","Severe")))
  return(value)
}

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = ClotAPTT[ClotAPTT$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = ClotAPTT[ClotAPTT$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$APTT_val
  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    value = APTT_comparator(highest)
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = APTT_comparator(first)
    }
    if (readingwanted == 2) {
    average = mean(all_values)
    value = APTT_comparator(average)
    # Add this 
    }
  ClotAPTT2 <- ClotAPTT2 %>% add_row(ID=id,APTT_val=value)
  }
}
ClotAPTT <- ClotAPTT2
rm(ClotAPTT2)

ClotPT = Clot %>% dplyr::select(ID,date,PT)
ClotPT = ClotPT %>% 
  rename(
    PT_val = PT,
  )
ClotPT = ClotPT %>% dplyr::select(ID,date,PT_val)
ClotPT <- ClotPT[complete.cases(ClotPT),]
ClotPT2 <- data.frame(ID=character(),
                  PT_val=character())
commonIDs <- intersect(total$ID,ClotPT$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = ClotPT[ClotPT$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = ClotPT[ClotPT$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$PT_val
  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((highest >= 13),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first >= 13),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average >= 13),"Abnormal","Normal")
    }
    # Add this 
    ClotPT2 <- ClotPT2 %>% add_row(ID=id,PT_val=value)
  }
}

ClotPT <- ClotPT2
rm(ClotPT2)

#### Antigen 
#Data is very spares only a few dozen tests - omitted
#Antigen <- read.csv(file("Antigen.csv"))
#Antigen = Antigen %>% 
#  rename(
#    date = Date.of.Specimen,
#    ID = uob_ID,
#  )


#### poctLAC - Point of Care Testing - Lactate
poctLAC <- read.csv(file("poctLAC.csv"))
#Format date
poctLAC$Date.of.Specimen = as.Date(poctLAC$Date.of.Specimen, format="%d/%m/%Y")
poctLAC = poctLAC %>% 
  rename(
    date = Date.of.Specimen,
     ID = uob_ID,
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
# dplyr::select relevant variables
poctLAC$date = as.Date(poctLAC$date, format="%d/%m/%Y")
poctLAC = poctLAC %>% dplyr::select(ID,date,poctLAC_val)
poctLAC <- poctLAC[complete.cases(poctLAC),]
poctLAC2 <- data.frame(ID=character(),
                  poctLAC_val=character())
commonIDs <- intersect(total$ID,poctLAC$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = poctLAC[poctLAC$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = poctLAC[poctLAC$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$poctLAC_val

  # If all_values contains any values, then proceed
    if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((lowest < 0.5 | highest > 2.2),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first < 0.5 | first > 2.2),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average < 0.5 | average > 2.2),"Abnormal","Normal")
    }
    # Add this 
    poctLAC2 <- poctLAC2 %>% add_row(ID=id,poctLAC_val=value)
  }
}

poctLAC <- poctLAC2
rm(poctLAC2)

#### poctO2 - Point of Care Testing - O2 and CO2
poctO2 <- read.csv(file("poctO2.csv"))

O2 <- poctO2 %>%
  filter(Test.Desc == "Arterial pO2")
O2$Date.of.Specimen = as.Date(O2$Date.of.Specimen, format="%d/%m/%Y")
O2 = O2 %>% 
  rename(
    date = Date.of.Specimen,
     ID = uob_ID,
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
O2 = O2 %>% dplyr::select(ID,date,O2_val)
O2 <- O2[complete.cases(O2),]
O22 <- data.frame(ID=character(),
                  O2_val=character())
commonIDs <- intersect(total$ID,O2$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = O2[O2$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = O2[O2$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$O2_val

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((lowest < 11 | highest > 14.4),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first < 11 | first > 14.4),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average < 11 | average > 14.4),"Abnormal","Normal")
    }
    # Add this 
    O22 <- O22 %>% add_row(ID=id,O2_val=value)
  }
}

O2 <- O22
rm(O22)

CO2 <- poctO2 %>%
  filter(Test.Desc == "Arterial pCO2")

#Format date
CO2$Date.of.Specimen = as.Date(CO2$Date.of.Specimen, format="%d/%m/%Y")
CO2 = CO2 %>% 
  rename(
    date = Date.of.Specimen,
     ID = uob_ID,
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
CO2 = CO2 %>% dplyr::select(ID,date,CO2_val)
CO2 <- CO2[complete.cases(CO2),]
CO22 <- data.frame(ID=character(),
                  CO2_val=character())
commonIDs <- intersect(total$ID,CO2$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = CO2[CO2$ID==id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = CO2[CO2$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$CO2_val

  # If all_values contains any values, then proceed
  if(length(!is.na(all_values))>0 & !is.na(idPositive)) {
    if (readingwanted == 0) {
    # Determine the lowest value
    lowest = suppressWarnings(min(all_values,na.rm=TRUE))
    # Determine the highest value
    highest = suppressWarnings(max(all_values,na.rm=TRUE))
    # Compare the lowest and highest values against the range, evaluate to true if
    # either are outside the range, and assign "Abnormal", else "Normal"
    value = if_else((lowest < 4.6 | highest > 6.4),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
    # select earliest test value in the list
    first <- all_values[1]
    # assign "Abnormal" if outside clinical range, else "Normal"
    value = if_else((first < 4.6 | first > 6.4),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values)
      value = if_else((average < 4.6 | average > 6.4),"Abnormal","Normal")
    }
    # Add this 
    CO22 <- CO22 %>% add_row(ID=id,CO2_val=value)
  }
}

CO2 <- CO22
rm(CO22)


#### poctpH - Point of Care Testing - pH
poctpH <- read.csv(file("poctpH.csv"))

#Format date
poctpH$Date.of.Specimen = as.Date(poctpH$Date...Time.of.Specimen, format = "%d/%m/%Y")
poctpH = poctpH %>% 
  rename(
    date = Date...Time.of.Specimen,
    ID = uob_ID,
    time = Time.of.Specimen,
    poctpH_val = Numeric.Result,
    poctpH_code = Test.Code,      
    poctpH_desc = Test.Desc
  )

#Combine date and time
poctpH$dateTime = as.POSIXct(paste(poctpH$date, poctpH$time), 
                             format = "%Y-%m-%d %H:%M:%S")
poctpH$date = as.Date(poctpH$date, format = "%d/%m/%Y")
poctpH = poctpH %>% dplyr::select(-dateTime)
poctpH <- poctpH[complete.cases(poctpH),]

poctpH2 <- data.frame(ID = character(),
                  poctpH_val = character())
poctpH_common_indaterange <- data.frame(ID = character(), 
                                        poctpH_val = numeric(), 
                                        poctpH_code = character(), poctpH_desc = character())

commonIDs <- intersect(total$ID,poctpH$ID)

for (id in (unique(commonIDs))){
  # Get all dates for this ID and variable
  idDates = poctpH[poctpH$ID == id,]$date
    # Sort the data by date
  idDates = sort(idDates, decreasing = FALSE)
  # Find covid positive test for this ID
  idPositive = total$CriticalDate[total$ID==id]
  # Get all values for this ID and variable within the daterange from positive date
  all_values = poctpH[poctpH$ID == id,][(idDates >= idPositive & idDates < 
                                         (idPositive + dateRange)),]

    # If all_values contains any values, then proceed
  if ( (length(!is.na(all_values$poctpH_val)) > 0) & !is.na(idPositive) ) {
    if (readingwanted == 0) {
      # Determine the lowest value
      lowest = suppressWarnings(min(all_values$poctpH_val,na.rm = TRUE))
      # Determine the highest value
      highest = suppressWarnings(max(all_values$poctpH_val,na.rm = TRUE))
      # Compare the lowest and highest values against the range, evaluate to true if
      # either are outside the range, and assign "Abnormal", else "Normal"
      value = if_else((lowest < 7.35 | highest > 7.45),"Abnormal","Normal")
    }
    if (readingwanted == 1) {
      # select earliest test value in the list
      first <- all_values$poctpH_val[1]
      # assign "Abnormal" if outside clinical range, else "Normal"
      value = if_else((first < 7.35 | first > 7.45),"Abnormal","Normal")
    }
    if (readingwanted == 2) {
      average = mean(all_values$poctpH_val)
      value = if_else((average < 7.35 | average > 7.45),"Abnormal","Normal")
    }
    # Add this 
    poctpH2 <- poctpH2 %>% add_row(ID = id,poctpH_val = value)
    
    #list with original values to verify above categorical transform
    #The 'date' data structure is a bit special - R insists that even though it appears
    #as a single row it is actually multiple could convert back to character but for now omit
    poctpH_common_indaterange <- poctpH_common_indaterange %>% 
      add_row(all_values[, c('ID','poctpH_val','poctpH_code', 'poctpH_desc')])
  }
}

bio_mark_fr <- data.frame( test = character() )

for (id in unique(poctpH_common_indaterange$ID)) {
  bio_mark_fr = add_row(bio_mark_fr,test = 
                          names(which.max(table(poctpH_common_indaterange[poctpH_common_indaterange$ID == id,'poctpH_desc']))))
}

poctpH <- poctpH2
rm(poctpH2)


#### Vir - Virology
Vir <- read.csv(file("Vir.csv"))
Vir['Measure']='Vir'
Vir$Sample.Date = as.Date(Vir$Sample.Date, format="%d/%m/%Y")
Vir = Vir %>% 
  rename(
     ID = uob_ID,
    date = Sample.Date,
    Adenovirus = Adenovirus..PCR.,
    Human_Metapneumovirus = Human.metapneumovirus..PCR.,
    Influenza_A = Influenza.A..PCR.,
    Influenza_B = Influenza.B..PCR.,
    Parainfluenza_Type_1 = Parainfluenza.Type.1..PCR.,
    Parainfluenza_Type_2 = Parainfluenza.Type.2..PCR.,
    Parainfluenza_Type_3 = Parainfluenza.Type.3..PCR.,
    Parainfluenza_Type_4 = Parainfluenza.Type.4..PCR.,
    Respiratory_Syncytial_Virus = Respiratory.Syncytial.Virus..PCR.,
    Rhinovirus = Rhinovirus..PCR.,
  )


Vir <- Vir %>% mutate(viral_coinfection = if_else((Adenovirus  == "POSITIVE" | Human_Metapneumovirus  == "POSITIVE" | Influenza_A  == "POSITIVE" | Influenza_B  == "POSITIVE" | Parainfluenza_Type_1  == "POSITIVE" | Parainfluenza_Type_2  == "POSITIVE" | Parainfluenza_Type_3  == "POSITIVE" | Parainfluenza_Type_4  == "POSITIVE" | Respiratory_Syncytial_Virus  == "POSITIVE" | Rhinovirus  == "POSITIVE"),TRUE,FALSE))
Vir <- Vir %>% dplyr::select(ID,date,viral_coinfection)
Vir <- Vir[complete.cases(Vir),]
Vir2 <- data.frame(ID=character(),
                  viral_coinfection=logical())
commonIDs <- intersect(total$ID,Vir$ID)

for (id in unique(commonIDs)){
    # Return all values for the admission date for this ID
    # Find the average value for these dates
    # Store in new table for that variable where we have one value per date per
  idDates = Vir[Vir$ID==id,]$date
  idPositive = total$CriticalDate[total$ID==id]
  presence = TRUE %in% (Vir[Vir$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$viral_coinfection)
  Vir2 <- Vir2 %>% add_row(ID=id,viral_coinfection=presence)

}

Vir <- Vir2
rm(Vir2)


#### BC
BC <- read.csv(file("BC.csv"))
BC['Measure']='BC'
BC$Date.of.Specimen = as.Date(BC$Date.of.Specimen, format="%d/%m/%Y")
BC = BC %>% 
  rename(
    date = Date.of.Specimen,
     ID = uob_ID,
  )

BC <- BC %>% mutate(bc_coinfection = if_else((Organism.Desc != "Negative BC" & Source.Desc != "? Contaminant"),TRUE,FALSE))
BC <- BC %>% dplyr::select(ID,date,bc_coinfection)
BC <- BC[complete.cases(BC),]
BC2 <- data.frame(ID=character(),
                  bc_coinfection=logical())
commonIDs <- intersect(total$ID,Vir$ID)

for (id in unique(commonIDs)){
    # Return all values for the admission date for this ID
    # Find the average value for these dates
    # Store in new table for that variable where we have one value per date per
  idDates = BC[BC$ID==id,]$date
  idPositive = total$CriticalDate[total$ID==id]
  presence = TRUE %in% (BC[BC$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$bc_coinfection)
  BC2 <- BC2 %>% add_row(ID=id,bc_coinfection=presence)

}

BC <- BC2
rm(BC2)

#### Resp
Resp <- read.csv(file("Resp.csv"))
Resp['Measure']='Resp'
Resp$Date.of.Specimen = as.Date(Resp$Date.of.Specimen, format="%d/%m/%Y")
Resp = Resp %>% 
  rename(
     ID = uob_ID,
    date = Date.of.Specimen,
  )

Resp <- Resp %>% mutate(resp_coinfection = if_else((Organism.Desc != "Negative Sp"),TRUE,FALSE))
Resp <- Resp %>% dplyr::select(ID,date,resp_coinfection)
Resp <- Resp[complete.cases(Resp),]
Resp2 <- data.frame(ID=character(),
                  resp_coinfection=logical())
commonIDs <- intersect(total$ID,Vir$ID)

for (id in unique(commonIDs)){
    # Return all values for the admission date for this ID
    # Find the average value for these dates
    # Store in new table for that variable where we have one value per date per
  idDates = Resp[Resp$ID==id,]$date
  idPositive = total$CriticalDate[total$ID==id]
  presence = TRUE %in% (Resp[Resp$ID==id,][(idDates >= idPositive & idDates < (idPositive + dateRange)),]$resp_coinfection)
  Resp2 <- Resp2 %>% add_row(ID=id,resp_coinfection=presence)

}

Resp <- Resp2
rm(Resp2)


#### Urine
Urine <- read.csv(file("Urine.csv"))
Urine['Measure']='Urine'
Urine$Date.of.Specimen = as.Date(Urine$Date.of.Specimen, format="%d/%m/%Y")
Urine = Urine %>% 
  rename(
     ID = uob_ID,
    date = Date.of.Specimen,
  )

Urine <- Urine %>% mutate(urine_coinfection = if_else((Organism.Desc != "Negative MSU"),TRUE,FALSE))
Urine <- Urine %>% dplyr::select(ID, date, urine_coinfection)
Urine <- Urine[complete.cases(Urine),]
Urine2 <- data.frame(ID = character(), urine_coinfection = logical())
commonIDs <- intersect(total$ID, Vir$ID)

for (id in unique(commonIDs)){
    # Return all values for the admission date for this ID
    # Find the average value for these dates
    # Store in new table for that variable where we have one value per date per
  idDates = Urine[Urine$ID == id,]$date
  idPositive = total$CriticalDate[total$ID == id]
  presence = TRUE %in% (Urine[Urine$ID == id,][(idDates >= idPositive & idDates < 
                                                (idPositive + dateRange)),]$urine_coinfection)
  Urine2 <- Urine2 %>% add_row(ID = id, urine_coinfection = presence)

}

Urine <- Urine2
rm(Urine2)






# Merge values with total table by admission date
variables = list(BE,BNP,CRP,DDM,eGFR,FER,fib,Glucose,HB,HBA1c,LDH,PCT,PLT,trig,
                 trop,FBCLymph,FBCNeutr,FBCWCC,FBCNLR,ClotAPTT,ClotPT,poctLAC,O2,
                 CO2,poctpH,Vir,BC,Resp,Urine,Urea)

for (variable in variables) {
  total <- merge(total,variable, all.x = TRUE)
}

#### Add any co-infection column 
total <- total %>% mutate(coinfection = if_else((viral_coinfection == TRUE | resp_coinfection == TRUE | bc_coinfection == TRUE | urine_coinfection == TRUE),TRUE,FALSE,FALSE))
totalBinary <- total

#### Comorbidities
#AvonCap <- as.data.frame(read_csv(file("AvonCap.csv"))) #recent update keeps giving error:
# Error in nchar(x, "width") : invalid multibyte string, element 1
#not clear why as this file has not been edited and opens fine in notepad++
#using read.csv instead of read_csv gets around
AvonCap <- tibble(read.csv("AvonCap.Update.UTF.csv"))

AvonCap = AvonCap %>% rename(ID = uob_ID)
AvonCap <- AvonCap[!duplicated(AvonCap$ID),]
AvonCap <- subset(AvonCap, select = -c(Repeat.Instrument,Repeat.Instance,
                                       COVID.19.diagnosis))
AvonCap = AvonCap %>%
    rename(
        Community.Pneumonia.Radio = Final.Standard.of.Care.LRTD.related.diagnosis..choice.CAP...radiologically.confirmed.,
        Community.Pneumonia.Clin = Final.Standard.of.Care.LRTD.related.diagnosis..choice.CAP...clinically.confirmed..but.not.on.radiology..,
        Community.Acquired.Pneumonia.No = Final.Standard.of.Care.LRTD.related.diagnosis..choice.CAP...no.radiology.performed.,
        Acute_bronchitis = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Acute.bronchitis.,
        Exacerbation_of_COPD = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Exacerbation.of.COPD.,
        Empyema_lung_abscess = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Empyema.lung.abscess.,
        LRTI_not_further_specified = Final.Standard.of.Care.LRTD.related.diagnosis..choice.LRTI...not.further.specified.,
        Congestive_heart_failure = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Congestive.heart.failure.,
        Non_infectious_process = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Non.infectious.process.,
        Non_LRTD_infection_related_diagnosis = Final.Standard.of.Care.LRTD.related.diagnosis..choice.Non.LRTD.infection.related.diagnosis.,
        Oth.LRTI.Specified =
        Final.Standard.of.Care.LRTD.related.diagnosis..choice.Other.LRTI.specified.,
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
        Organ_transplantation = Organ.Transplantation,
        Pregnancy_post_partum = Pregnancy.Post.partum,
        Gastric_Duodenal_Ulcer_disease = Gastric.Duodenal.Ulcer.Disease.Patients.who.have.required.treatment.for.PUD.nbsp.,
        Rockwood_frailty_score = Rockwood.Frailty.Score,
        Radiology_result = Radiology.Result
    )
AvonCap <- subset(AvonCap, select = c(ID,PSI,
                                      NYHA_Heart_failure,CRB65_Score,
                                      NEWS2_Score,COPD,Asthma,Bronchiectasis,
                                      Respiratory_Disease_other, Hypertension,
                                      Chronic_Kidney_Disease,Liver_disease,
                                      Diabetes,CVA_Stroke,TIA_mini_stroke,
                                      Immunodeficiency, HIV_positive))
totalBinary <- merge(total,AvonCap,by = c("ID"),all.x = TRUE)

# Check values against ranges and create normal/abnormal binary column

# Missingness
drop <- c("ID","admissionDate","dischargeDate","Gender","Age","died",
          "went_to_icu","severeOutcome")

missingData = total[,!(names(total) %in% drop)]

fn <- paste(output_path, 'totalBinary', as.character(dateRange), 
            readingwanted_str, sep = '')

tmp = missingData %>% ff_glimpse()
write.csv(tmp[2], file = paste(fn,'.missing_data_summary.csv', sep = ''))

missingData %>% missing_plot()
ggsave(paste(fn,'.missing_values_map.pdf', sep = ''),device = 'pdf',
       width = 20, height = 20, units = 'cm', dpi = 300)

all_columns <- c("BE_val","BNP_val","CRP_val","DDM_val","eGFR_val","FER_val",
                 "fib_val","Glucose_val","HB_val","HBA1c_val","LDH_val","PCT_val",
                 "PLT_val","trig_val","trop_val","Lymphocytes","Neutrophils",
                 "WCC","NLR_val","APTT_val","PT_val","poctLAC_val","O2_val",
                 "CO2_val","poctpH_val","viral_coinfection","bc_coinfection",
                 "resp_coinfection","urine_coinfection",'Urea_val')

#Note these variables were highlighted as being suitable for imputation missing at random
#but imputation does NOT take place, could be done here on continuous values
#or in analysis stage with categoricals

imputed_columns <- c("eGFR_val","WCC","Neutrophils","Lymphocytes","NLR_val",
                     "HB_val","PLT_val","CRP_val")

# Test not taken columns
tnt_columns <- setdiff(all_columns, imputed_columns)

# Replace NA with "Test not taken"
totalBinary[tnt_columns][is.na(totalBinary[tnt_columns])] <- "Test not taken"

#the final file is used as an intermediate for other analysis so save with .R files
fn <- paste(work_path, 'totalBinary', as.character(dateRange), 
            readingwanted_str, sep = '')

write.csv(x = totalBinary, file = paste(fn,'.csv', sep = ''))

