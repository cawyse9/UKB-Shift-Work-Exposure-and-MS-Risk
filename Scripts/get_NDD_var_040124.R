## Script name:         get_NDD_var_040124.R

## Purpose of script:   extract data on prevalence of NDD

## Author:              Cathy Wyse

## Date Created:        2024-01-04

## Contact:             cathy.wyse@mu.ie


library(psych)
library(dplyr)

##############################################################################################
# Multiple Sclerosis
##############################################################################################

# process MS data
MS_UKB <- read.delim("./MS_UKB.tsv")

# recode and rename MS source of diagnosis and date of diagnosis
lvl.131043 <- c(20,21,30,31,40,41,50,51)
lbl.131043 <- c("Death register only",
	"Death register and other source(s)",
	"Primary care only",
  "Primary care and other source(s)",
	"Hospital admissions data only",
  "Hospital admissions data and other source(s)",
	"Self-report only",
  "Self-report and other source(s)")

MS_UKB$MS_source <- factor(MS_UKB$X131043.0.0, levels=lvl.131043, labels=lbl.131043) #	Source of report of G35 (multiple sclerosis)

MS_UKB$MS_year <-as.integer(substring(MS_UKB$X131042.0.0,1,4)) #	Date G35 first reported (multiple sclerosis)
#MS_year is the year of diagnosis
#
# Coding	    Meaning
# 1900-01-01	Code has no event date
# 1901-01-01	Code has event date before participant's date of birth
# 1902-02-02	Code has event date matching participant's date of birth
# 1903-03-03	Code has event date after participant's date of birth and falls in the same calendar year as                  date of birth
# 1909-09-09	Code has event date in the future and is presumed to be a place-holder or other system default
# 2037-07-07	Code has event date in the future and is presumed to be a place-holder or other system default

comment(MS_UKB$MS_source) <- "Data field 131043"
comment(MS_UKB$MS_year) <-"Data field 131042"

#MS source is the diagnosis of MS
#MS_year is the year of diagnosis

MS_UKB$MS_YN <- ifelse(!is.na(MS_UKB$MS_source), 1, 0)
table(MS_UKB$MS_YN, useNA = "always")
head(MS_UKB)
MS_pheno <- MS_UKB[,c("eid","eid", "MS_YN")]
names(MS_pheno)<-c("FID","IID","pheno1")

write.csv(MS_pheno, file = "pheno.csv")
getwd()
##############################################################################################
# Parkinsons Disease
##############################################################################################

#  get PD and Dementia data
PD_dementia <- read.delim("./PD_dementia.tsv")

PD_dementia$PD_year <- as.integer(substring(PD_dementia$X42030.0.0,1,4)) #"X42030.0.0" Date of all cause parkinsonism report -> year

comment(PD_dementia$PD_year) <- "Data field 42030"
#1900-01-01	Date is unknown

table(PD_dementia$PD_year)

PD_dementia$PD_source  <- PD_dementia$X42031.0.0 # Source of all cause parkinsonism report
comment(PD_dementia$PD_source) <- "Data field 42031"
table(PD_dementia$X42031.0.0)

# Coding  	Meaning
# 0	        Self-reported only
# 1	        Hospital admission
# 2	        Death only
# 11	      Hospital primary
# 12	      Death primary
# 21	      Hospital secondary
# 22	      Death contributory

PD_dementia$PD_source <- factor(PD_dementia$PD_source,
    levels = c(0,1,2,11,12,21,22),
    labels = c("Self-reported only",
  	          "Hospital admission",
  	          "Death only",
  	          "Hospital primary",
  	          "Death primary",
  	          "Hospital secondary",
  	          "Death contributory"))
table(PD_dementia$PD_source)

# these are unused, data taken from all cause PD
#"X42032.0.0" Date of parkinson's disease report
#"X42033.0.0" Source of parkinson's disease report

PD_dementia$PD_YN <- ifelse(!is.na(PD_dementia$PD_source), 1, 0)


##############################################################################################
# All Cause Dementia
##############################################################################################

PD_dementia$dementia_year <- as.integer(substring(PD_dementia$X42018.0.0,1,4)) #"X42018.0.0" Date of all cause dementia report -> year
comment(PD_dementia$dementia_year) <- "Data field 42018"

# Coding	Meaning
# 1900-01-01	Date is unknown

PD_dementia$dementia_source <- PD_dementia$X42019.0.0  #"X42019.0.0" Source of all cause dementia report
table(PD_dementia$X42019.0.0)
# Coding	Meaning
# 0	      Self-reported only
# 1	      Hospital admission
# 2	      Death only
# 11    	Hospital primary
# 12    	Death primary
# 21    	Hospital secondary
# 22    	Death contributory
PD_dementia$dementia_source <- factor(PD_dementia$dementia_source,
levels = c(0,1,2,11,12,21,22),
labels = c("Self-reported only",
           "Hospital admission",
           "Death only",
           "Hospital primary",
           "Death primary",
           "Hospital secondary",
           "Death contributory"))

PD_dementia$dementia_YN <- ifelse(!is.na(PD_dementia$dementia_source), 1, 0)

table(PD_dementia$dementia_YN)
table(PD_dementia$dementia_source)

##############################################################################################
# Motor neurone disease
##############################################################################################

mnd <- read.delim("./mnd.tsv")

mnd$mnd_year <- as.integer(substring(mnd$X42028.0.0,1,4)) #Date of motor neurone disease report-> year
comment(mnd$mnd_year) <- "Data field 42028"
# Coding	Meaning
# 1900-01-01	Date is unknown

mnd$mnd_source <- mnd$X42029.0.0  #Data-Field 42029 Description:	Source of motor neurone disease report
# Coding	Meaning
# 0	      Self-reported only
# 1	      Hospital admission
# 2	      Death only
# 11	    Hospital primary
# 12	    Death primary
# 21	    Hospital secondary
# 22	    Death contributory
mnd$mnd_source <- factor(mnd$mnd_source,
          levels = c(0,1,2,11,12,21,22),
          labels = c("Self-reported only",
                     "Hospital admission",
                     "Death only",
                     "Hospital primary",
                     "Death primary",
                     "Hospital secondary",
                     "Death contributory"))
mnd$mnd_YN <- ifelse(!is.na(mnd$mnd_source), 1, 0)
table(mnd$mnd_YN)

#########################################################################################################
# merge to UKB Master and remove diagnosis before 20
#########################################################################################################

#merge
UKB_master <- merge (UKB_master, mnd, by="eid")
UKB_master <- merge (UKB_master, PD_dementia, by="eid")
UKB_master <- merge (UKB_master, MS_UKB, by="eid")

#age at diagnosis
UKB_master$MS_age <- UKB_master$MS_year - UKB_master$year_born
UKB_master$PD_age <- UKB_master$PD_year - UKB_master$year_born
UKB_master$dementia_age <- UKB_master$dementia_year - UKB_master$year_born

#check if anyone was diagnosed between age 15-20
remove1 <- UKB_master$eid[!is.na(UKB_master$MS_age) & UKB_master$MS_age < 20 & UKB_master$MS_age >15]
remove2 <- UKB_master$eid[!is.na(UKB_master$PD_age) & UKB_master$PD_age < 20 & UKB_master$PD_age >15]
remove3 <- UKB_master$eid[!is.na(UKB_master$dementia_age) & UKB_master$dementia_age < 20 & UKB_master$dementia_age > 15]

#check if anyone was diagnosed before birth - don't do this, unknown diagnosis dates are before birth.  Unknown or uncertain diagnosis dates are not excluded, but not possible to know if they were diagnosed before 20

#eids to remove
UKB_master <- subset(UKB_master, !(eid %in% remove1 | eid %in% remove2 | eid %in% remove3))


#########################################################################################################
# merge to UKB Master and remove diagnosis after 2015
#########################################################################################################

MS_diag2015 <- MS_UKB$eid[(!is.na(MS_UKB$MS_year) & MS_UKB$MS_year > 2015) ]
PD_diag2015 <- PD_dementia$eid[(!is.na(PD_dementia$PD_year) & PD_dementia$PD_year > 2015) ]
dementia_diag2015 <- PD_dementia$eid[(!is.na(PD_dementia$PD_year) & PD_dementia$PD_year > 2015) ]

diag_2015 <- c (MS_diag2015, PD_diag2015, dementia_diag2015)

# #get a time to event variable - MS_year - age started work, or 2015 -  age started work
# 
# UKB_masterSW_cum$time_to_eventdementia <- UKB_masterSW_cum$dementia_year - (UKB_masterSW_cum$year_born +18) #if ms
# UKB_masterSW_cum$time_to_eventdementia <- ifelse (is.na(UKB_masterSW_cum$dementia_year), (2015 - UKB_masterSW_cum$year_born), UKB_masterSW_cum$time_to_eventdementia)
# table(UKB_masterSW_cum$time_to_eventdementia)
# 
# 
# library(survival)
# UKB_masterSW_cum$SW
# # Create a survival object
# surv_obj <- Surv(UKB_masterSW_cum$time_to_eventdementia, UKB_masterSW_cum$dementia_YN)
# 
# # Fit the Cox proportional hazards model
# cox_model <- coxph(surv_obj ~ dementia_YN + SW + age, data = UKB_masterSW_cum)
# 
# # Display summary of Cox model
# summary(cox_model)




