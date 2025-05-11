library(tidyverse)
library(dplyr)
library(pillar)
library(readxl)
library(hms)
library(psych)

### R extract & data prep #########
#set cwd to folder you want to do your analysis in
setwd("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork")

#import the downloaded data and run encoding R code
script_path <- file.path(getwd(), "helper", "ukb673864.r")

# Run the script
source(script_path)

#rename default 'bd' dataframe as core_vars
core_vars <- bd

# withdraw latest participants (download latest withdraw list from basket)
withdrawn <- read.table("./withdraw99491_10_20231013.txt", quote="\"", comment.char="")
names(withdrawn) <- "eid"
core_vars <- core_vars[! core_vars$f.eid %in% withdrawn$eid,]

#make new data frame to store variables
UKB_master <- data.frame(core_vars$f.eid)
comment(UKB_master )<-c("Core variables for 'Rhythms of Life' project")
colnames(UKB_master)[1]<-"eid"

# can double-check the withdrawal worked with: which(UKB_master$eid == withdrawn)



## Primary demographics ####################

#sex (31)
sex <- core_vars$f.31.0.0
comment(sex)<-c("Datafield=31.0.0")
sex <- factor(sex)

#age (21003)
age <- core_vars$f.21003.0.0
comment(age)<-c("Datafield = 21003.0.0")

#year of birth (34)
year_born <- core_vars$f.34.0.0
comment(year_born)<-c("Datafield = 34.0.0")

#month of birth (52)
month_born <- core_vars$f.52.0.0
comment(month_born)<-c("Datafield = 52.0.0")

#ethnicity (21000)
ethnicity <- core_vars$f.21000.0.0
comment(ethnicity)<-c("Datafield = 21000.0.0")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, sex, age, year_born, month_born, ethnicity)

#remove environment variables no longer needed outside of dataframe
rm(sex, age, year_born, month_born, ethnicity)

#small ethicnicty var 
UKB_master <- UKB_master %>%
  mutate(ethnicity_5 = case_when(
    
    ethnicity == "Prefer not to answer" ~ NA,
    ethnicity == "Do not know" ~ NA,
    ethnicity == "White" ~ "White",
    ethnicity == "Mixed" ~ "Mixed",
    ethnicity == "Asian or Asian British" ~ "Asian",
    ethnicity == "Black or Black British" ~ "Black",
    ethnicity == "Chinese" ~ "Chinese",
    ethnicity == "Other ethnic group" ~ "Other",
    ethnicity == "British" ~ "White",
    ethnicity == "Irish" ~ "White",
    ethnicity == "Any other white background" ~ "White",
    ethnicity == "White and Black Caribbean" ~ "Mixed",
    ethnicity == "White and Black African" ~ "Mixed",
    ethnicity == "White and Asian" ~ "Mixed",
    ethnicity == "Any other mixed background" ~ "Mixed",
    ethnicity == "Indian" ~ "Asian",
    ethnicity == "Pakistani" ~ "Asian",
    ethnicity == "Bangladeshi" ~ "Asian",
    ethnicity == "Any other Asian background" ~ "Asian",
    ethnicity == "Caribbean" ~ "Black",
    ethnicity == "African" ~ "Black",
    ethnicity == "Any other Black background" ~ "Black",
    TRUE ~ NA
  )
  )     


## Education & Employment #############################################

#	Age completed full-time education (845)
age_completed_education <- core_vars$f.845.0.0
comment(age_completed_education)<-c("Datafield = 845")
age_completed_education <- ifelse(age_completed_education == -2, 0,
                                         ifelse(age_completed_education == -1, NA,
                                                ifelse(age_completed_education == -3, NA,
                                                       age_completed_education)))
#-2 represents "Never went to school"
#-1 represents "Do not know"
#-3 represents "Prefer not to answer"


#	Qualifications (6138)
qualifications <- core_vars$f.6138.0.0
comment(qualifications)<-c("Datafield = 6138")
table(qualifications)
qualifications <- ifelse(qualifications == "None of the above", NA,
                     ifelse(qualifications == "Prefer not to answer", NA,
                            qualifications))

qualifications <- factor(qualifications, levels = c(3:8), labels = c("University degree", "A levels", "GCSEs", "CSEs", "HND", "Professional qualification"))

# 1	College or University degree
# 2	A levels/AS levels or equivalent
# 3	O levels/GCSEs or equivalent
# 4	CSEs or equivalent
# 5	NVQ or HND or HNC or equivalent
# 6	Other professional qualifications eg: nursing, teaching
# -7	None of the above
# -3	Prefer not to answer


#	Current employment status (6142)
employed <- core_vars$f.6142.0.0
comment(employed)<-c("Datafield = 6142")

#	Current employment status (corrected) (20119)
employed_corr <- core_vars$f.20119.0.0
comment(employed_corr)<-c("Datafield = 20119")

#	Job involves night shift work (3426)
current_night_shift <- core_vars$f.3426.0.0
comment(current_night_shift)<-c("Datafield = 3426")
# Recode 
current_night_shift[current_night_shift == "Prefer not to answer"] <- NA
current_night_shift[current_night_shift == "Do not know"] <- NA
current_night_shift <- droplevels(current_night_shift)

#	Job involves shift work (826)
current_shift_work <- core_vars$f.826.0.0
comment(current_shift_work)<-c("Datafield = 826")

# Recode 
current_shift_work[current_shift_work == "Prefer not to answer"] <- NA
current_shift_work[current_shift_work == "Do not know"] <- NA
current_shift_work <- droplevels(current_shift_work)

# Coding	Meaning
# 1	Never/rarely
# 2	Sometimes
# 3	Usually
# 4	Always
# -1	Do not know
# -3	Prefer not to answer

#add variables to master dataframe
UKB_master <- cbind(UKB_master, age_completed_education, qualifications, employed,
                    employed_corr, current_night_shift, current_shift_work)

#remove environment variables no longer needed outside of dataframe
rm(age_completed_education, qualifications, employed, employed_corr, current_night_shift,
   current_shift_work)


## Early life  #############################################

#Adopted as a child (1767)
adopted <- core_vars$f.1767.0.0
comment(adopted)<-c("Datafield = 1767")

#Birth weight (20022)
birth_weight  <- core_vars$f.20022.0.0
comment(birth_weight)<-c("Datafield = 20022")

#Birth weight metric (120)
birth_weight_metric  <- core_vars$f.120.0.0
comment(birth_weight_metric)<-c("Datafield = 120")

#Breastfed as a baby (1677)
breastfed <- core_vars$f.1677.0.0
comment(breastfed)<-c("Datafield = 1677")

#Country of Birth (non-UK origin) (20115)
country_birth_nonuk <- core_vars$f.20115.0.0
comment(country_birth_nonuk)<-c("Datafield = 20115")

#Country of birth (UK/elsewhere) (1647)
country_birth_uk <- core_vars$f.1647.0.0
comment(country_birth_uk)<-c("Datafield = 1647")

#Handedness (chirality/laterality) (1707)
handedness <- core_vars$f.1707.0.0
comment(handedness)<-c("Datafield = 1707")

#maternal smoking around birth (1787)
maternal_smoking <- core_vars$f.1787.0.0
comment(maternal_smoking)<-c("Datafield = 1787")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, adopted, birth_weight, birth_weight_metric,
                    breastfed, country_birth_uk, country_birth_nonuk, handedness,
                    maternal_smoking)

#remove environment variables no longer needed outside of dataframe
rm(adopted, birth_weight, birth_weight_metric, breastfed, country_birth_uk,
   country_birth_nonuk, maternal_smoking, handedness)


## Health  #############################################

#smoking status (20116)
smoking_status <- core_vars$f.20116.0.0
comment(smoking_status)<-c("Datafield = 20116")
smoking_status[smoking_status == "Prefer not to answer"] <- NA
smoking_status <- droplevels(smoking_status)

#alcohol intake frequency (1558)
alcohol_intake <- core_vars$f.1558.0.0
comment(alcohol_intake)<-c("Datafield = 1558")
alcohol_intake <- ifelse(alcohol_intake == 'Prefer not to answer', NA, alcohol_intake)
alcohol_intake <- factor(alcohol_intake, levels = c(2:7), labels = c("Daily or almost daily","Three or four times a week","Once or twice a week","One to three times a month","Special occasions only","Never"))

#overall health rating (2178)
health_self_report <- core_vars$f.2178.0.0
comment(health_self_report)<-c("Datafield = 2178")
health_self_report <- ifelse(health_self_report == 'Prefer not to answer' | health_self_report == "Do not know", NA, health_self_report)
health_self_report <- factor(health_self_report, levels = c(3:6), labels = c("Excellent","Good","Fair","Poor"))

#number of medications (137)
medication_number <- core_vars$f.137.0.0
comment(medication_number)<-c("Datafield = 137")

#medication code (20003)
medication_code <- core_vars$f.20003.0.0
comment(medication_code)<-c("Datafield = 20003")

#attendance/disability/mobility allowance (6146)
disability_allowance <- core_vars$f.6146.0.0
comment(disability_allowance)<-c("Datafield = 6146")

#longstanding illness or disability (2188)
disability_self_report <- core_vars$f.2188.0.0
comment(disability_self_report)<-c("Datafield = 2188")

#seen psychiatrist for nerves/anxiety/depression (2100)
depress_psych <- core_vars$f.2100.0.0
comment(depress_psych)<-c("Datafield = 2100")

depress_psych <- ifelse(depress_psych == 'Prefer not to answer', NA, depress_psych)
depress_psych<- ifelse(depress_psych == 'Prefer not to answer', NA,
                              ifelse(depress_psych == "Do not know", NA,
                                     depress_psych))

#seen GP for nerves/anxiety/depression (2090)
depress_gp <- core_vars$f.2090.0.0
comment(depress_gp)<-c("Datafield = 2090")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, smoking_status, alcohol_intake, health_self_report,
                    medication_number, medication_code, disability_allowance, disability_self_report,
                    depress_psych, depress_gp)

#remove environment variables no longer needed outside of dataframe
rm(smoking_status, alcohol_intake, health_self_report, medication_number,
   medication_code, disability_allowance, disability_self_report, depress_gp, depress_psych)

## Seasonal#############################################

#time spent outdoors in summer (1050)
time_outdoors_summer <- core_vars$f.1050.0.0
comment(time_outdoors_summer)<-c("Datafield = 1050")
# clean time_outdoors_summer
# -10	Less than an hour a day
# -1	Do not know
# -3	Prefer not to answer

time_outdoors_summer <- ifelse(time_outdoors_summer == -10, .5,
                                      ifelse(time_outdoors_summer == -1, NA,
                                             ifelse(time_outdoors_summer == -3, NA,
                                                    time_outdoors_summer)))

#time spent outdoors in winter (1060)
time_outdoors_winter <- core_vars$f.1060.0.0
comment(time_outdoors_winter)<-c("Datafield = 1060")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, time_outdoors_summer, time_outdoors_winter)

#remove environment variables no longer needed outside of dataframe
rm(time_outdoors_summer, time_outdoors_winter)

## Sleep #############################################

#Sleep duration (1160)
sleep_duration<- core_vars$f.1160.0.0
comment(sleep_duration)<-c("Datafield = 1160")

#value -3 (Prefer not to answer)
#value -1 (Do not know)
sleep_duration <- ifelse(sleep_duration == -3, NA,
                                ifelse(sleep_duration == -1, NA,
                                       sleep_duration))
sleep_cat <- ifelse (sleep_duration < 6, "Short" ,
                    ifelse(sleep_duration > 6 & sleep_duration < 9, "Optimal",
                           ifelse(sleep_duration > 9, "Long",   
                           NA)))
table(sleep_cat)

#Getting up in morning	(1170)
getting_up  <- core_vars$f.1170.0.0
comment(getting_up)<-c("Datafield = 1170")

#Morning/evening person (chronotype) (1180)
chronotype <- core_vars$f.1180.0.0
comment(chronotype)<-c("Datafield = 1180")
table(chronotype)
chronotype <- ifelse(chronotype == "Do not know", NA,
                            ifelse(chronotype == "Prefer not to answer", NA,
                                   chronotype))
chronotype <- factor(chronotype, levels = c(3:6), labels = c("Morning","More morning than evening","More evening than morning","Evening"))

#Nap during day (1190)
day_naps <- core_vars$f.1190.0.0
comment(day_naps)<-c("Datafield = 1190")

#Sleeplessness / insomnia (1200)
insomnia <- core_vars$f.1200.0.0
comment(insomnia)<-c("Datafield = 1200")
table(insomnia)
insomnia <- ifelse(insomnia == "Prefer not to answer", NA, insomnia)

#Snoring (1210)
snoring <- core_vars$f.1210.0.0
comment(snoring)<-c("Datafield = 1210")

#Daytime dozing / sleeping (1220)
day_sleepiness <- core_vars$f.1220.0.0
comment(day_sleepiness)<-c("Datafield = 1220")

#Alcohol consumed yesterday (100580)
alcohol_yesterday <- core_vars$f.100580.0.0
comment(alcohol_yesterday)<-c("Datafield = 100580")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, sleep_duration, getting_up, chronotype, day_naps, insomnia,
                    snoring, day_sleepiness, alcohol_yesterday, sleep_cat)

#remove environment variables no longer needed outside of dataframe
rm(sleep_duration, getting_up, chronotype, day_naps, day_sleepiness, insomnia,
   snoring, alcohol_yesterday)


## Physical Measures #############

#BMI (21001)
BMI <- core_vars$f.21001.0.0
comment(BMI)<-c("Datafield = 21001.0.0")

BMI_cat <- cut(BMI, breaks = c(0, 18.5, 24.9, 29.9, Inf), 
                    labels = c("Underweight", "Normal weight", "Overweight", "Obese"))

#add variables to master dataframe
UKB_master <- cbind(UKB_master, BMI, BMI_cat)

#remove environment variables no longer needed outside of dataframe
rm(BMI)

######################################################################################################
#  Extra variables after UKB_master made from core variables.  These must use merge not cbind as order and n could be different

#get townsend index
townsend <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/townsend.tsv")
names(townsend) <- c("eid", "townsend")
comment(townsend$townsend)<-c("Data field = 22189.0.0")
townsend <- townsend[! townsend$eid %in% withdrawn$eid,]
UKB_master <- merge(UKB_master, townsend, by="eid")

#get PA from touchscreen data
PA <- read.delim("./PA.tsv")

names (PA) <- c("eid", "MET_week", "IPAQ","modPA" )
comment(PA$MET_week)<-c("Data field = 22040.0.0")
comment(PA$IPAQ)<-c("Data field = 22032.0.0")
comment(PA$modPA)<-c("Data field = 884.0.0")

# recode modPA
PA$modPA <- ifelse(PA$modPA == -1, NA,PA$modPA)
# Coding	Meaning
# -1	Do not know
# -3	Prefer not to answer

# recode IPAQ
PA$IPAQ <- factor (PA$IPAQ, levels = c(0:2), labels = c("low", "moderate","high"))
# Coding	Meaning
# 0	low
# 1	moderate
# 2	high

UKB_master <- merge(UKB_master, PA, by = "eid")

#childhood obesity
childhood_obesity <- read.delim("./childhood_obesity.tsv")
names (childhood_obesity) <- c("eid", "child_obesity")
comment(childhood_obesity$child_obesity)<-c("Data field = 1687.0.0")
levels_CO <- c(1, 2, 3, -1, -3)
labels_CO <- c("Thinner", "Plumper", "About average","Do not know",	"Prefer not to answer")
childhood_obesity$child_obesity <- factor(childhood_obesity$child_obesity, labels = labels_CO, levels = levels_CO)
childhood_obesity$child_obesity[which(childhood_obesity$child_obesity == "Do not know")] <- NA
childhood_obesity$child_obesity[which(childhood_obesity$child_obesity == "Prefer not to answer")] <- NA
childhood_obesity$child_obesity <- droplevels(childhood_obesity$child_obesity)
table(childhood_obesity$child_obesity, useNA = "always")
UKB_master <- merge (UKB_master, childhood_obesity, by = "eid")

# smoking vars collected during online followup
smoking <- read.delim("./smoking.tsv")
# "eid"        "X20116.0.0" "X3436.0.0"  "X3436.1.0"  "X3436.2.0"  "X3436.3.0"  "X6194.0.0"  "X6194.1.0" 
# "X6194.2.0"  "X6194.3.0"  "X22507.0.0" "X22506.0.0" "X2867.0.0"  "X2867.1.0"  "X2867.2.0"  "X2867.3.0" 

#Data field 3436 started smoking current - function to check and return the value for starting smoking at all instances because not everyone asked this question at assessment centre
get_value <- function(row) {
  if (is.na(row[["X3436.0.0"]])) {
    non_missing_values <- row[c("X3436.1.0", "X3436.2.0", "X3436.3.0")]
    non_missing_values <- non_missing_values[!is.na(non_missing_values)]
    if (length(non_missing_values) > 0) {
      return(non_missing_values[1])
    } else {
      return(NA)
    }
  } else {
    return(row[["X3436.0.0"]])
  }
}

# Apply the function row-wise to the smoking dataframe to extract all values of starting smoking over the instances
smoking$smoke_start_curr <- apply(smoking, 1, get_value)

#set to NA
smoking$smoke_start_curr <- ifelse(smoking$smoke_start_curr == -1, NA,
                                   ifelse(smoking$smoke_start_curr == -3, NA,
                                          smoking$smoke_start_curr))

table(smoking$smoke_start_curr, useNA = "always")

#repeat for previous smokers start age
#Data field 2867 started smoking previous - function to check and return the value for starting smoking at all instances
get_value <- function(row) {
  if (is.na(row[["X2867.0.0"]])) {
    non_missing_values <- row[c("X2867.1.0", "X2867.2.0", "X2867.3.0")]
    non_missing_values <- non_missing_values[!is.na(non_missing_values)]
    if (length(non_missing_values) > 0) {
      return(non_missing_values[1])
    } else {
      return(NA)
    }
  } else {
    return(row[["X2867.0.0"]])
  }
}

# Apply the function row-wise to the smoking dataframe to extract all values of starting smoking over the instances
smoking$smoke_start_prev <- apply(smoking, 1, get_value)

#set to NA
smoking$smoke_start_prev <- ifelse(smoking$smoke_start_prev == -1, NA,
                                   ifelse(smoking$smoke_start_prev == -3, NA,
                                          smoking$smoke_start_prev))
table(smoking$smoke_start_prev, useNA = "always")

#Data-Field 22506 get smoking status now from the 120k - this is different than main touchscreen smoking status variable
smoking$smoker <- smoking$X22506.0.0
comment(smoking$smoker) <- c("Data field = 22506")

smoking$smoker <- factor(smoking$smoker,
labels = c("Smokes on most or all days", "Occasionally","Ex-smoker","Never smoked","Prefer not to answer"),
levels = c(111,112,113,114,-818))           
smoking$smoker[which(smoking$smoker == "Prefer not to answer")] <- NA
smoking$smoker <- droplevels(smoking$smoker)

# Coding	Meaning
# 111	    Smokes on most or all days
# 112	    Occasionally
# 113	    Ex-smoker
# 114	    Never smoked
# -818	  Prefer not to answer

# Create a data frame with the given factor variable
smoking <- smoking %>%
  mutate(smokerYN = case_when(
    smoker %in% c("Smokes on most or all days", "Ex-smoker", "Occasionally") ~ "Smoker",
    smoker == "Never smoked" ~ "Non-Smoker",
    TRUE ~ as.character(smoker)  # Keep other levels unchanged
  ))
table(smoking$smokerYN, useNA = "always")

#Data-Field 22507 age stopped - this is only from 120k
smoking$smoke_stopped <- smoking$X22507.0.0
comment(smoking$smoke_stopped) <- c("Data field = 22507")
describe(smoking$smoke_stopped)
# the started smoking variables are from the assessment centre and other visits.  There were no questions about starting smoking in the online follow up

# calculate mean of stopped smoking prev or current, this allows for people that responded to both
smoking$smoke_start <- rowMeans(smoking[, c("smoke_start_curr", "smoke_start_prev")], na.rm = TRUE)

# rowmeans doesn't work if there is an NA, so if the mean is NA, use the value from smoking prev, if that is NA, return smoking curr, if that is NA return NA.  This variable is now the started smoking age for everyone in the main dataset, the age for the 120k will be extracted from this later
smoking$smoke_start <- ifelse(
  is.na(smoking$smoke_start),
  ifelse(
    !is.na(smoking$smoke_start_prev),
    smoking$smoke_start_prev,
    ifelse(
      !is.na(smoking$smoke_start_curr),
      smoking$smoke_start_curr,
      NA
    )
  ),
  smoking$smoke_start
)
table(smoking$smoke_start, useNA="always")

# now we have (i)   smoking status from 120k   22506
#             (ii)  stopped smoking from 120k  22507
#             (iii) started smoking sifted from all instances from 500k  2867 3436
   

# make variable for early_life_smoking - this is starting smoking at 20 or earlier
smoking$smoke_20yr <- NA
smoking$smoke_20yr <- ifelse(smoking$smoke_start <= 20  & smoking$smoke_start > 1, 1, 
                           ifelse(smoking$smoke_start > 20, 0,
                                      NA))
                               
table(smoking$smoke_20yr, useNA ="always")

describe(smoking$smoke_start_curr)
describe(smoking$smoke_start_prev)

vars <- c("eid",
"smoke_start_curr",
"smoke_start_prev",
"smoker",
"smokerYN",
"smoke_stopped",
"smoke_start",
"smoke_20yr")

subset_smoking <- smoking[vars]

UKB_master <- merge(x = UKB_master, y = subset_smoking, by = "eid")


# add latitude of birth
latitude_birth_lookup <- read.csv("./latitude_birth_lookup.csv")
UKB_master <- merge(UKB_master,latitude_birth_lookup[,c(1:2)], all.x = TRUE, by = "country_birth_nonuk")
UKB_master <- UKB_master[order(UKB_master$eid, decreasing = FALSE), ]

#add UK latitude
UKB_master$birth_latitude <- ifelse(UKB_master$country_birth_uk == "England", 52,
                                    ifelse(UKB_master$country_birth_uk == "Scotland", 56,
                                           ifelse(UKB_master$country_birth_uk == "Wales", 52,
                                                  UKB_master$latitude)))

describe(UKB_master$birth_latitude)

### Save core variables 'UKB_master' as R datafile ######
save(UKB_master,file="UKB_master150224.Rda")

### Save core variables 'UKB_master' as csv ######
write.csv(UKB_master, file="UKB_master150224.csv")

#remove lvl lbl  for clean environment
rm(list=ls(pattern="lvl"))
rm(list=ls(pattern="lbl"))



