## Script name:         current_SW_analysis_210624.R

## Purpose of script:   data processing and analysis of baseline shiftwork data

## Author:              Cathy Wyse

## Date Created:        2024-06-21

## Contact:             cathy.wyse@mu.ie

library(survival)
library(lubridate)
library(forestmodel)
library(labelled)
library(finalfit)

library(forestplot)
library(metafor)
# ----------------------  make new dataset of working people only ---------------------------------------

#include
UKB_master$currently_working <- NA
UKB_master$currently_working[UKB_master$employed=="Doing unpaid or voluntary work"] <- 1
UKB_master$currently_working[UKB_master$employed=="In paid employment or self-employed"] <- 1

table(UKB_master$currently_working, useNA="always")
table(UKB_master$employed)

# #exclude
# Full or part-time student 
# Looking after home and/or family 
# None of the above                             
# Prefer not to answer 
# Retired Unable to work because of sickness or disability 
# Unemployed 

UKB_master_work <- UKB_master[!is.na(UKB_master$currently_working),] #remove non-working


# ----------------------  make new dataset of people MS free at baseline   ---------------------------------------
#current_shift_work (nested nightshift work) : reference day workers

MS_diag_and_centre_date <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/MS_diag_and_centre_date.txt")
MS_date <- MS_diag_and_centre_date

MS_date$MS_diag_date <- as.Date(MS_date$X131042.0.0)
MS_date$assess_date <- as.Date(MS_date$X53.0.0)
censor_date <- max(MS_date$MS_diag_date, na.rm=TRUE)

#merge dates into UKB_work dataset
UKB_master_work <-merge(UKB_master_work, MS_date, by = "eid")

#make start_date
UKB_master_work$MS_start_date <- UKB_master_work$assess_date

#make end date as either diagnosis or end of study
UKB_master_work$end_date <- UKB_master_work$MS_diag_date
UKB_master_work$end_date[is.na(UKB_master_work$end_date)] <- censor_date
sum(!is.na(UKB_master_work$MS_diag_date)) # 1098 still includes pre baseline diagnosis

#make follow up time in months
UKB_master_work$follow_up <- time_length(interval(UKB_master_work$MS_start_date, UKB_master_work$end_date), "months")

#does MS diag come before assessment centre - there were 342 people without MS as baseline in working group
UKB_master_work$MS_baseline <- (ifelse(time_length(interval(UKB_master_work$assess_date, UKB_master_work$MS_diag_date), "months") > 0, 1,0))
table(UKB_master_work$MS_baseline) # 342 diagnosis after baseline

v<-UKB_master_work[(!is.na(UKB_master_work$MS_baseline)),]
nrow(v[,c("MS_baseline", "MS_diag_date")]) #1098 in total, 756 before assess, 342 after
table(UKB_master_work$MS_baseline)

# remove people with MS at baseline from UKB_master_work
UKB_master_work$MS_baseline <- factor(UKB_master_work$MS_baseline)

# keep rows where MS_baseline equals 1 ie where the months between diagnosis and baseline assessment were > 0
UKB_master_work <- UKB_master_work[is.na(UKB_master_work$MS_baseline) | UKB_master_work$MS_baseline == 1, ]

table(UKB_master_work$MS_YN)

#-----------------         make sleep category           ------------------------------------------
UKB_master_work$sleep_cat <- NA
UKB_master_work$sleep_cat[UKB_master_work$sleep_duration < 7] <- "Short Sleep"
UKB_master_work$sleep_cat[UKB_master_work$sleep_duration >= 7 & UKB_master_work$sleep_duration < 9] <- "Optimal Sleep"
UKB_master_work$sleep_cat[UKB_master_work$sleep_duration >= 9] <- "Long Sleep"
table(UKB_master_work$sleep_cat, useNA="always")


#-----------------         table options           ------------------------------------------
#make tables
my.render.cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=4), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}

my.render.cat <- function(x) {c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))}

my.render.NP_cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=0), c("","Median (IQR)"=sprintf("%s (&plusmn; %s)", MEDIAN, IQR)))}

#template table <- table1(~  age + sex | eventname ,  data=data, overall=TRUE, render.continuous=my.render.cont, render.categorical=my.render.cat)

# format continuous vars with 95% CI
my.render.cont = function(x) {c("", "mean (95% CI)"=sprintf("%s (%s, %s)",
                                                            round(stats.default(x)$MEAN,2),
                                                            round(stats.default(x)$MEAN-qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2),
                                                            round(stats.default(x)$MEAN+qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2)
))
}

# Table 13   missing
---------------------------------------------------------------------------------------------------------------------
  table1(~ sex + age + townsend + ethnicity_5 + #demography
           BMI_cat + child_obesity  + maternal_smoke + sunburn_cat + 
           chronotype + health_self_report + sleep_duration + sleep_cat +#physiology
           alcohol_intake + time_outdoors_summer +  #lifestyle
           smoking_status + smoke_start + current_shift_work + current_night_shift #smoking
         | factor(MS_YN), data=UKB_master_work, overall=FALSE,  render.continuous=my.render.cont, render.categorical=my.render.cat)

# Table 6   Demography of current SW - stratify by MS diagnosis
---------------------------------------------------------------------------------------------------------------------
  table1(~ sex + age + townsend + ethnicity_5 + birth_latitude + #demography
           BMI_cat + child_obesity  + maternal_smoke + sunburn_cat + 
           chronotype + health_self_report + sleep_duration + sleep_cat +#physiology
           alcohol_intake + time_outdoors_summer +  #lifestyle
           smoking_status + smoke_start + current_shift_work + current_night_shift #smoking
           | factor(MS_YN), data=UKB_master_work, overall=FALSE,  render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' ,  'age' , 'townsend' , 'birth_latitude' , #demography
                                    'BMI_cat' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                    'smoking_status' , 'sunburn_cat', 
                                    "current_shift_work",'current_night_shift', 'maternal_smoke', 'health_self_report'),
                           strata = c('MS_YN'), 
                           data = UKB_master_work, 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)


---------------------------------------------------------------------------------------------------------------------
# Table 7   Demography of current SW - stratify by shiftwork status
---------------------------------------------------------------------------------------------------------------------

#make shiftwork into a factor
table(UKB_master_work$current_shift_work, useNA="always")

UKB_master_work$shiftwork_YN[UKB_master_work$current_shift_work=="Never/rarely"] <- 0
UKB_master_work$shiftwork_YN[UKB_master_work$current_shift_work=="Always"] <- 1
UKB_master_work$shiftwork_YN[UKB_master_work$current_shift_work=="Usually"] <- 1
UKB_master_work$shiftwork_YN[UKB_master_work$current_shift_work=="Sometimes"] <- 1
UKB_master_work$shiftwork_YN[is.na(UKB_master_work$current_shift_work)] <- NA
UKB_master_work$shiftwork_YN <- factor(UKB_master_work$shiftwork_YN)
table(UKB_master_work$shiftwork_YN, useNA="always")

# Remove rows with NA in the factor variable (stay in analysis dataset)
UKB_master_work2 <- UKB_master_work[!is.na(UKB_master_work$shiftwork_YN), ]

table1(~ sex + age + townsend + birth_latitude + ethnicity_5 + #demography
         BMI_cat + child_obesity  + maternal_smoke + sunburn_cat + 
         chronotype + health_self_report + sleep_duration + sleep_cat +#physiology
         alcohol_intake + time_outdoors_summer +  #lifestyle
         smoking_status + smoke_start + current_shift_work + current_night_shift #smoking
       | shiftwork_YN, data=UKB_master_work2, overall=FALSE,  render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                    'health_self_report','BMI_cat' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'health_self_report', 'time_outdoors_summer' ,  'sleep_cat', 'IPAQ', #lifestype
                                    'smoking_status' , 'sunburn_cat', 'maternal_smoke','smoke_start'
                                    ),
                           strata = c("shiftwork_YN"), 
                           data = UKB_master_work[!is.na(UKB_master_work$shiftwork_YN),], 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)



#--------------------------------------------------------------------------------------------------------------------
#
#  get variables for cox regression
#
##--------------------------------------------------------------------------------------------------------------------



#get MS diagnosis date
#-------------------------------------------------------------------------------------------------------

# Data-Field 131042  Description:	Date G35 first reported (multiple sclerosis)

#Date of the first occurrence of any code mapped to 3-character ICD10 G35. The code corresponds to "multiple sclerosis". note that events which were apparently before birth were omitted when constructing this data field.

# Coding 819 defines 6 special values:
#   
# 1900-01-01 represents "Code has no event date"
# 1901-01-01 represents "Code has event date before participant's date of birth"
# 1902-02-02 represents "Code has event date matching participant's date of birth"
# 1903-03-03 represents "Code has event date after participant's date of birth and falls in the same calendar year as date of birth"
# 1909-09-09 represents "Code has event date in the future and is presumed to be a place-holder or other system default"
# 2037-07-07 represents "Code has event date in the future and is presumed to be a place-holder or other system default"  
  

#Data-Field 53  Description:	Date of attending assessment centre

#exclude people that were diagnosed with MS before the assessment centre visit = 342 left.  


# startdate <- assessment centre visit dates for each participant
# enddate <- MS diagnosis, or if NA, then date UKB accessed - date of primary care data update
# follow up time < - end-start date
# MS_status <- MSYN

UKB_master_work$assess_date #baseline date
end_date <- censor_date

UKB_master_work$MS_YN # 'status' is the event indicator (1 for event, 0 for censored)
UKB_master_work$MS_diag_date
UKB_master_work$follow_up # 'time' is the follow-up time

UKB_master_work$shiftwork_YN # variable indicating shift work at baseline 


# time_to_event: follow-up time until MS diagnosis or censoring
UKB_master_work$MS_followup <- time_length(interval(ymd(UKB_master_work$assess_date), ymd(UKB_master_work$MS_diag_date)),"year")
UKB_master_work$MS_followup_censored <- time_length(interval(ymd(UKB_master_work$assess_date), ymd(end_date)),"year")

UKB_master_work$MS_followup_all <- ifelse(is.na(UKB_master_work$MS_followup), UKB_master_work$MS_followup_censored, UKB_master_work$MS_followup) 


# event: 1 if MS diagnosis, 0 if censored MS_YN
# shiftwork_baseline: exposure to shift work at baseline shiftwork_YN


# make a variable that includes nightSW
# Create a combined categorical variable for shift work
UKB_master_work$shiftwork_cat <- with(UKB_master_work, 
                                      ifelse(current_shift_work %in% c("Always", "Sometimes", "Usually"), 
                                             ifelse(current_night_shift %in% c("Always", "Sometimes", "Usually"), 
                                                    "Night Shift Work", 
                                                    "Shift Work"), 
                                             "No Shift Work")
)

# Recode the shiftwork variable
UKB_master_work$shiftwork_all <- ifelse(UKB_master_work$current_shift_work %in% c("Always", "Sometimes", "Usually"), "Yes", "No")

ifelse(UKB_master_work$shiftwork_night==1, UKB_master_work$shiftwork_cat=="night",
       ifelse(UKB_master_work$shiftwork_all==1, UKB_master))

# Recode the nightshift variable
UKB_master_work$nightshift_binary <- ifelse(UKB_master_work$current_night_shift %in% c("Always", "Sometimes", "Usually"), "Yes", "No")

# Convert the combined variable to a factor
UKB_master_work$shiftwork_cat <- factor(UKB_master_work$shiftwork_cat, levels = c("No Shift Work", "Shift Work", "Night Shift Work"))

# Set "No Shift Work" as the reference level
UKB_master_work$shiftwork_cat <- relevel(UKB_master_work$shiftwork_cat, ref = "No Shift Work")

# Set "Never" as the reference level
UKB_master_work$alcohol_intake <- relevel(factor(UKB_master_work$alcohol_intake), ref = "Never")

# Set "Never" as the reference level
UKB_master_work$smoking_status <- relevel(factor(UKB_master_work$smoking_status), ref = "Never")
table(UKB_master_work$smoking_status)

# Set "morning" as the reference level
UKB_master_work$Chronotype <- relevel(factor(UKB_master_work$chronotype), ref = "Morning")
table(UKB_master_work$Chronotype)
UKB_master_work$Chronotype <- relevel(factor(UKB_master_work$Chronotype), ref = "Morning")

# Set "No Shift Work" as the reference level
UKB_master_work$shiftwork_cat <- relevel(factor(UKB_master_work$shiftwork_cat), ref = "No Shift Work")

# Set "optimal" as the reference level
UKB_master_work$sleep_cat <- relevel(factor(UKB_master_work$sleep_cat), ref = "Optimal Sleep")


# Create a Surv object
UKB_master_work$Surv_object <- with(UKB_master_work, Surv(time = MS_followup_all, event = MS_YN))


# Renaming variables within the UKB_master_work dataframe
names(UKB_master_work)[names(UKB_master_work) == "Surv_object"] <- "Surv_object"
names(UKB_master_work)[names(UKB_master_work) == "sex"] <- "Sex"
names(UKB_master_work)[names(UKB_master_work) == "age"] <- "Age"
names(UKB_master_work)[names(UKB_master_work) == "townsend"] <- "Townsend Index"
names(UKB_master_work)[names(UKB_master_work) == "BMI_cat"] <- "BMI"
names(UKB_master_work)[names(UKB_master_work) == "child_obesity"] <- "Childhood Obesity"
names(UKB_master_work)[names(UKB_master_work) == "sleep_duration"] <- "Sleep Duration"
#names(UKB_master_work)[names(UKB_master_work) == "sleep_cat"] <- "Sleep Duration"
names(UKB_master_work)[names(UKB_master_work) == "chronotype"] <- "Chronotype"
names(UKB_master_work)[names(UKB_master_work) == "alcohol_intake"] <- "Alcohol"
names(UKB_master_work)[names(UKB_master_work) == "time_outdoors_summer"] <- "Time Outdoors"
names(UKB_master_work)[names(UKB_master_work) == "smoking_status"] <- "Smoking"
names(UKB_master_work)[names(UKB_master_work) == "shiftwork_cat"] <- "Shiftwork"


# Fit the Cox proportional hazards model
cox_model1 <- coxph(Surv_object ~ Shiftwork , data = UKB_master_work)

cox_model2 <- coxph(Surv_object ~ Sex + Age + `Townsend Index` + 
                      BMI + `Childhood Obesity` +`Sleep Duration` + Chronotype , data = UKB_master_work)

cox_model3 <- coxph(Surv_object ~ Shiftwork + Sex + Age + `Townsend Index` + BMI + `Childhood Obesity` + 
                      `Sleep Duration` + Chronotype + Alcohol + `Time Outdoors` + Smoking  
                      , data = UKB_master_work)

summary(cox_model3)# Extracting the forest model data


pars <- forest_model_format_options(
  colour = "black",
  color = NULL,
  shape = 15,
  text_size = 6, # Set text size to 10
  point_size = 3,
  banded = TRUE
)

panels <- list(
  list(width = 0.02),
  list(width = 0.60, display = ~variable, fontface = "bold", heading = "Variable"),
  list(width = 0.06, display = ~level),
  
  list(width = 0.02, item = "vline", hjust = 0.5),
  list(
    width = 0.40, item = "forest", hjust = 0.5, heading = "HR", linetype = "dashed",
    line_x = 0
  ),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(width = 0.07, display = ~ ifelse(reference, "Reference", sprintf(
    "%0.2f (%0.2f, %0.2f)",
    trans(estimate), trans(conf.low), trans(conf.high)
  )), display_na = NA, heading = "HR (95% CI)"),
  list(
    width = 0.04,
    display = ~ ifelse(reference, "", format.pval(p.value, digits = 1, eps = 0.001)),
    display_na = NA, hjust = 1, heading = "p"
  ),
  list(width = 0.01)
)


p<-forest_model(cox_model3, format_options=pars, panels)
p
# Save the plot as SVG
ggsave("HRforest080525.svg", plot = p, device = "svg", width = 20, height = 20, units = "cm", dpi = 600)

ggsave(
  "HRforest_A4_portrait.jpg",
  plot = p,
  device = "jpg",
  width = 20,  # A4 width in cm
  height = 25,  # A4 height in cm
  units = "cm",
  dpi = 600
)


dat<-p$plot_data$forest_data

p1<-forest_model(cox_model1, format_options=pars, return_data=TRUE, panels)
p2<-forest_model(cox_model2, format_options=pars, return_data=TRUE, panels)
p3<-forest_model(cox_model3, format_options=pars, return_data=TRUE, panels)
dat1<-p1$plot_data$forest_data
dat2<-p2$plot_data$forest_data
dat3<-p3$plot_data$forest_data

write.csv(dat1, "dat1.csv")
write.csv(dat2, "dat2.csv")
write.csv(dat3, "dat3.csv")