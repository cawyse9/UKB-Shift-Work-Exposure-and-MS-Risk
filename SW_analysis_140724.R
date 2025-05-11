# Data Analysis of UKB Lifetime SW and NDD
library(lme4)
library(table1)
library(tableone)
library(finalfit)
library(dplyr)
library(knitr)
library(sjPlot)
library(stringr)
library(glmnet)
library(psych)
library(performance)
library(see)
library(sjmisc)
library(sjlabelled)
library(lubridate)
library(pscl)



setwd("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork")

shiftwork <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork.csv")
UKB_master <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/UKB_master150224.csv")

# add sunburn data
sunburn <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/sunburn.tsv")
names(sunburn) <- c("eid", "sunburn")
sunburn[sunburn$sunburn == "Prefer not to answer",] <- NA #-3	Prefer not to answer

# Ensure sunburn column is treated as character
sunburn$sunburn <- as.character(sunburn$sunburn)

# Initialize the new column with NA
sunburn$sunburn_cat <- NA

# Assign categories based on string comparisons
sunburn$sunburn_cat[sunburn$sunburn == "Do not know"] <- "Do not know"
sunburn$sunburn_cat[sunburn$sunburn == "0"] <- "none"
sunburn$sunburn_cat[sunburn$sunburn == "1" | sunburn$sunburn == "2"] <- "once or twice"
sunburn$sunburn_cat[sunburn$sunburn == "3" | sunburn$sunburn == "4" | sunburn$sunburn == "5"] <- "three to five times"
sunburn$sunburn_cat[as.numeric(sunburn$sunburn) >= 6] <- "more than 5 times"

# Create a frequency table of the categories
table(sunburn$sunburn_cat)

UKB_master <- merge(UKB_master,sunburn,by="eid")
# unit are occasions
# -1	Do not know
# -3	Prefer not to answer

#adding centre to UKB_master
centre <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/centre.tsv")
names(centre) <- c("eid", "centre")
UKB_master <- merge(UKB_master,centre,by="eid")

#adding maternal smoking to UKB_master Data-Field 1787
maternal_smoke <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/maternal_smoke.tsv")
names(maternal_smoke) <- c("eid", "maternal_smoke")
maternal_smoke$maternal_smoke[maternal_smoke$maternal_smoke == ""] <- NA
maternal_smoke$maternal_smoke[maternal_smoke$maternal_smoke == "Prefer not to answer"] <- NA# -3	Prefer not to answer
maternal_smoke$maternal_smoke <- factor(maternal_smoke$maternal_smoke)
UKB_master <- merge(UKB_master,maternal_smoke,by="eid")

#adding early life smoking variable - derived from age started smoking
#problem with age started smoking question in UKB, doesn't include "occasionally" in smoker and does include "once or twice" in never smoked in contrast to the smoking status variable.  If age started smoking is nested in smoking status then the definition of former and current are not the same.  This will not matter because the are all mixed up, but some people that were smokers in status variable were not asked the age question.

#two variables to include = smoking_status with smoke_start as nested

# goto get_NDD_var_040124.R to get NDD data into UKB_master

##########################################################################################################################
#
#  1.  Format shiftwork table to get cumulative hours or dose SW
#
##########################################################################################################################

shiftwork_cum <- shiftwork

# ever did early night shiftwork 15-20
shiftwork_cum$early_NSW_YN <- 0
shiftwork_cum$early_NSW_YN <- ifelse (shiftwork_cum$agebracket=="15-20" &
                                        shiftwork_cum$bracket_total_hr_nightSW > 0 , 1, 
                                      shiftwork_cum$early_NSW_YN)

shiftwork_cum$early_NSW_YN <- ifelse (shiftwork_cum$agebracket=="15-20" &
                                        shiftwork_cum$bracket_total_hr_mixSW > 0 , 1, 
                                      shiftwork_cum$early_NSW_YN)
shiftwork_cum$early_NSW_YN <- factor(shiftwork_cum$early_NSW_YN)

# ever did early any shiftwork 15-20
shiftwork_cum$early_SW_YN <- 0
shiftwork_cum$early_SW_YN <- ifelse(shiftwork_cum$agebracket == "15-20" & 
                     (shiftwork_cum$bracket_total_hr_nightSW > 0 | shiftwork_cum$bracket_total_hr_mixSW > 0 |   
                        shiftwork_cum$bracket_total_hr_daySW > 0 ), 1, shiftwork_cum$early_SW_YN)
shiftwork_cum$early_SW_YN <- factor(shiftwork_cum$early_SW_YN)

shiftwork_cum <- shiftwork_cum [,c(
                                  "eid",              
                                  "agebracket",               
                                  "bracket_total_hr",        
                                  "bracket_total_hr_daySW",   
                                  "bracket_total_hr_nightSW", 
                                  "bracket_total_hr_mixSW",   
                                  "bracket_SW_type",          
                                  "bracket_SW_occupation",   
                                  "bracket_SW_per_work",      
                                  "bracket_nightSW_per_work", 
                                  "bracket_daySW_per_work",   
                                  "bracket_mixSW_per_work", 
                                  "early_NSW_YN",
                                  "early_SW_YN",
                                  "bracket_SW_YN")]  

# Summarize continuous variables by eid
shiftwork_cum <- shiftwork_cum %>%
  group_by(eid) %>%
  summarise(total_hr = sum(bracket_total_hr),
            total_hr_daySW = sum(bracket_total_hr_daySW),
            total_hr_nightSW = sum(bracket_total_hr_nightSW),
            total_hr_mixSW = sum(bracket_total_hr_mixSW),
            early_NSW_YN = max(as.numeric(early_NSW_YN)),
            early_SW_YN = max(as.numeric(early_SW_YN)),
            SW_type = paste(bracket_SW_type, collapse = " "),          
            SW_occupation = paste(bracket_SW_occupation, collapse = " ")
    )

# Remove rows with NA or zero values in 'total_hours' column
shiftwork_cum <- shiftwork_cum %>%
  filter(!is.na(total_hr) & total_hr != 0)

UKB_masterSW_cum <- merge(shiftwork_cum, UKB_master, by="eid")

# Calculate total work hours by summing the values row-wise
UKB_masterSW_cum$SW <- rowSums(UKB_masterSW_cum[, c("total_hr_nightSW", "total_hr_mixSW", "total_hr_daySW")])
UKB_masterSW_cum$NSW <- rowSums(UKB_masterSW_cum[, c("total_hr_nightSW", "total_hr_mixSW")])

UKB_masterSW_cum$NDD <- UKB_masterSW_cum$NDD <- 0
UKB_masterSW_cum$NDD <- ifelse(UKB_masterSW_cum$MS_YN == 1, "MS", 
                               ifelse(UKB_masterSW_cum$PD_YN == 1, "PD",
                                      ifelse(UKB_masterSW_cum$dementia_YN == 1, "Dementia"
                                             ,0)))


##########################################################################################################################
#
#  2.  Merge shiftwork and core variables 
#
##########################################################################################################################

#merge shiftowrk and core variables
UKB_masterSW <- merge(shiftwork, UKB_master, by="eid")

# get dataset of healthy only participants (other than MS)
# Coding	Meaning
# 1	Excellent
# 2	Good
# 3	Fair
# 4	Poor
# -1	Do not know
# -3	Prefer not to answer
UKB_masterSW <- UKB_masterSW %>%
  mutate(healthy = case_when(
    health_self_report == "Prefer not to answer" ~ NA,
    health_self_report == "Excellent" ~ "Healthy",
    health_self_report == "Good" ~ "Healthy",
    health_self_report == "Fair" ~ "Not Healthy",
    health_self_report == "Poor" ~ "Not Healthy",
    health_self_report == "Do not know" ~ "Not Healthy",
    health_self_report == "Prefer not to answer" ~ NA,
    TRUE ~ NA
  ))
UKB_masterSW$healthy <- factor(UKB_masterSW$healthy)


# define the participants that worked but not SW
UKB_masterSW$bracket_SW_type <- ifelse(is.na(UKB_masterSW$bracket_SW_type) & UKB_masterSW$bracket_total_hr > 0, "notSW",UKB_masterSW$bracket_SW_type)

# bracket SW_YN = participant did any type of SW in that bracket
UKB_masterSW$bracket_SW_YN <- ifelse(is.na(UKB_masterSW$bracket_SW_type), NA, 
                                ifelse(UKB_masterSW$bracket_SW_type %in% c("daySW", "mixSW", "nightSW"), 1, 0))
  

# bracket NSW_YN = participant did any NSW in that bracket
UKB_masterSW$bracket_NSW_YN <- ifelse(is.na(UKB_masterSW$bracket_SW_type), NA, 
                                     ifelse(UKB_masterSW$bracket_SW_type %in% c("mixSW", "nightSW"), 1, 0))

# ever did early shiftwork 15-20
UKB_masterSW$early_SW_YN <- 0  #any SW
UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & # getting NSW
                                             UKB_masterSW$bracket_total_hr_nightSW > 0 , 1, 
                                               UKB_masterSW$early_SW_YN)

UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & #getting daySW
                                             UKB_masterSW$bracket_total_hr_daySW > 0 , 1, 
                                               UKB_masterSW$early_SW_YN)
                                    
UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & #getting mixSW
                                             UKB_masterSW$bracket_total_hr_mixSW > 0 , 1, 
                                               UKB_masterSW$early_SW_YN)

UKB_masterSW$early_NSW_YN <- 0  #any NSW
UKB_masterSW$early_NSW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & # getting NSW
                                            UKB_masterSW$bracket_total_hr_nightSW > 0 , 1, 
                                               UKB_masterSW$early_NSW_YN)

UKB_masterSW$early_NSW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & # getting mixSW
                                            UKB_masterSW$bracket_total_hr_mixSW > 0 , 1, 
                                               UKB_masterSW$early_NSW_YN)

UKB_masterSW$early_SW_YN <- factor(UKB_masterSW$early_SW_YN)
UKB_masterSW$early_NSW_YN <- factor(UKB_masterSW$early_NSW_YN)

# make percentage in work variable - note that NA or 0 hours are gaps, not possible to have NA, either work or gap
UKB_masterSW$workingYN <- ifelse(is.na(UKB_masterSW$bracket_total_hr), 0,1)
 
# make variable to stratify by NDD
UKB_masterSW$NDD <- 0
UKB_masterSW$NDD <- ifelse(UKB_masterSW$MS_YN == 1, "MS", 
                            ifelse(UKB_masterSW$PD_YN == 1, "PD",
                                   ifelse(UKB_masterSW$dementia_YN == 1, "Dementia"
                                          ,0)))
 
#########################################################################################################################
# 
# 3,  Format shiftwork table to get years of shiftwork variables
#   
#########################################################################################################################

shiftwork_yr140324 <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork_yr140324.csv") # this has only participants in follow up study and only SW categories

shiftwork_years <- shiftwork_yr140324

# Replace NA with 0
shiftwork_years[is.na(shiftwork_years)] <- 0

# get years of SW for each type
shiftwork_years$yrs_SW  <- rowSums(shiftwork_years[,c("mixSW","nightSW","daySW")]) # any SW
shiftwork_years$yrs_NSW <- rowSums(shiftwork_years[,c("mixSW","nightSW")])   # NSW

# Define the breakpoints for intervals
breaks <- c(-Inf, 0, 4.99, 9.99, Inf)

# Define labels for the intervals
labels <- c("0", "1-5", "6-10", ">10")

# Recode the continuous variable into categories
shiftwork_years$SW_cat <- cut(shiftwork_years$yrs_SW, breaks = breaks, labels = labels, include.lowest = TRUE)
shiftwork_years$NSW_cat <- cut(shiftwork_years$yrs_NSW, breaks = breaks, labels = labels, include.lowest = TRUE)

shiftwork_years <- merge(UKB_masterSW,shiftwork_years, by = "eid")  # merge the new SW categories into main table
table(shiftwork_years$bracket_SW_type)
shiftwork_years$bracket

##############################################################################################################
#
# 4.  SW in early life no age bracket data
#
################################################################################################################

#make a dataset to test early life shiftwork regardless of age bracket
x<-shiftwork_years %>%
  select(eid, sex, age, ethnicity_5, smoking_status, centre, alcohol_intake, time_outdoors_summer, sleep_duration,                chronotype,  BMI_cat, townsend, maternal_smoke, sunburn_cat,  child_obesity, smoker, smoke_start,  smoke_20yr, birth_latitude, MS_YN, NDD, early_SW_YN, early_NSW_YN, bracket_SW_YN, bracket_NSW_YN, NDD) 


# define a function to get max even if all values are missing
safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(max(x, na.rm = TRUE))
  }
}

# Define a function to calculate mode, even if all missing
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


summary_data <- x %>%
  group_by(eid) %>%
  summarise(
    sex = get_mode(sex),
    age = safe_max(age),
    ethnicity_5 = get_mode(ethnicity_5),
    smoking_status = get_mode(smoking_status),
    smoke_start = get_mode(smoke_start),
    alcohol_intake = get_mode(alcohol_intake),
    time_outdoors_summer = safe_max(time_outdoors_summer),
    sleep_duration = safe_max(sleep_duration),
    chronotype = get_mode(chronotype),
    BMI_cat = get_mode(BMI_cat),
    townsend = safe_max(townsend),
    child_obesity = get_mode(child_obesity),
    maternal_smoke = get_mode(maternal_smoke),
    sunburn_cat = get_mode(sunburn_cat),
    smoker = get_mode(smoker),
    smoke_start = safe_max(smoke_start),
    smoke_20yr = safe_max(smoke_20yr),
    birth_latitude = safe_max(birth_latitude),
    MS_YN = get_mode(MS_YN),
    early_SW_YN = safe_max(as.numeric(early_SW_YN)),
    early_NSW_YN = safe_max(as.numeric(early_NSW_YN)),
    bracket_SW_YN = safe_max(as.numeric(bracket_SW_YN)),
    bracket_NSW_YN = safe_max(as.numeric(bracket_NSW_YN))
  )


summary_data$early_SW_YN = factor(summary_data$early_SW_YN)
summary_data$early_NSW_YN = factor(summary_data$early_NSW_YN)
summary_data$bracket_SW_YN = factor(summary_data$bracket_SW_YN)
summary_data$bracket_NSW_YN = factor(summary_data$bracket_NSW_YN)

 #########################################################################################################################
 # 
 # 5.  Make tables of descriptive data
 #   
 #########################################################################################################################
 
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

#check 95% CI
t.test(tabledata[tabledata$MS==1,]$age)$conf.int
sample_mean <- mean(tabledata[tabledata$MS==1,]$age)
sample_sd <- sd(tabledata[tabledata$MS==1,]$age)
sample_size <- length(tabledata[tabledata$MS==1,]$age) 
df <- sample_size - 1
se <- sample_sd / sqrt(sample_size)
margin_of_error <- qt(0.975, df) * se
lower_ci <- sample_mean - margin_of_error
upper_ci <- sample_mean + margin_of_error



# Table 1   Demography stratify by MS
--------------------------------------------------------------------------------------------------------------------
  
sum(is.na((summary_data$bracket_NSW_YN)))
sum(is.na((summary_data$bracket_SW_YN)))
      
# Remove rows with NA in the summary_data$bracket_NSW_YN column
summary_data <- summary_data[!is.na(summary_data$bracket_NSW_YN), ]
summary_data <- summary_data[!is.na(summary_data$bracket_SW_YN), ]


p1<-table1(~ sex + age + townsend + birth_latitude + #demography
         BMI_cat +   chronotype + child_obesity + #physiology
         alcohol_intake + time_outdoors_summer +  #lifestyle
         + maternal_smoke + sunburn_cat +
         smoker + smoke_start + smoke_20yr  +  #smoking
         early_NSW_YN + early_SW_YN + factor(bracket_NSW_YN) + factor(bracket_SW_YN) # shiftwork 15-20
         | factor(MS_YN), data=summary_data, overall=FALSE,  render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                      'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                      'alcohol_intake' , 'time_outdoors_summer' , "sunburn_cat" ,  #lifestype
                                      'smoker' ,"maternal_smoke",
                                    'smoke_start' , 'smoke_20yr'  ,
                                      'early_NSW_YN' , 'early_SW_YN' , 'bracket_NSW_YN' , 'bracket_SW_YN' ),
                           strata = c("MS_YN"), 
                           data = summary_data, 
                           #test = FALSE, 
                           factorVars = c())

p <- print(tableOne, printToggle = FALSE, noSpaces = TRUE)
kable(p, format = "latex")
# Table 2   Demography stratify by SW - ever did SW
---------------------------------------------------------------------------------------------------------------------
  
table1(~ sex + age + townsend + birth_latitude + #demography
           BMI_cat +   chronotype + child_obesity + #physiology
           alcohol_intake + time_outdoors_summer +  #lifestyle
           + maternal_smoke + sunburn_cat +
           smoker + smoke_start + smoke_20yr  +  #smoking
           early_NSW_YN   # shiftwork 15-20
         | factor(bracket_SW_YN ), data=summary_data, overall=FALSE,  render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                    'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                    'smoker' ,
                                    'smoke_start' , 'smoke_20yr'  ,
                                    'early_NSW_YN' , 'early_SW_YN' ),
                           strata = c("bracket_SW_YN"), 
                           data = summary_data, 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)

# Table 2   Demography stratify by SW - ever did NSW
---------------------------------------------------------------------------------------------------------------------
  
  table1(~ sex + age + townsend + birth_latitude + #demography
           BMI_cat +   chronotype + child_obesity + #physiology
           alcohol_intake + time_outdoors_summer +  #lifestyle
           + maternal_smoke + sunburn_cat +
           smoker  + smoke_start + #smoking
           early_NSW_YN +early_SW_YN  # shiftwork 15-20
         | factor(bracket_NSW_YN ), data=summary_data, overall=FALSE,  render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                    'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                    'smoker' , 'sunburn_cat', 'maternal_smoke', "smoke_start",
                                    'early_NSW_YN' , 'early_SW_YN' ),
                           strata = c("bracket_NSW_YN"), 
                           data = summary_data, 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)


# Table 4   Demography stratify ever did early life shiftwork
---------------------------------------------------------------------------------------------------------------------
table1(~ sex + age + townsend + birth_latitude + #demography
           BMI_cat +   chronotype + child_obesity + #physiology
           alcohol_intake + time_outdoors_summer +  #lifestyle
           + maternal_smoke + sunburn_cat +
           smoker  + smoke_start + #smoking
           bracket_SW_YN + bracket_NSW_YN # shiftwork 15-20
         | factor(early_SW_YN), data=summary_data, overall=FALSE,  render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                    'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                    'smoker' , 'sunburn_cat', 'maternal_smoke','smoke_start',
                                     "bracket_NSW_YN",  "bracket_NSW_YN"),
                           strata = c('early_SW_YN'), 
                           data = summary_data, 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)


# Table 5   Shiftwork exposure data 
---------------------------------------------------------------------------------------------------------------------

#table to summarise years of exposure and dose of shiftwork - merge with previous summary_data table
shiftwork_exposure<- merge(shiftwork_cum, summary_data, by="eid")



#get the most common occupation of each shiftworker - where there was a tie, one was selected randommly.  This didn't account for their non-shiftwork jobs
# Define a function to find the most common word(s) in a string

find_most_common_word <- function(string) {
  string <- gsub(",", "", string)
  words <- str_split(string, "\\s+")[[1]]
  words <- words[words != "NA" & words != ""]  # Remove "NA" and empty strings
  
  if (length(words) == 0) {
    return(NA)  # Return NA if no valid words are found
  }

 word_counts <- table(words)
  max_freq <- max(word_counts)
  most_common_words <- names(word_counts[word_counts == max_freq])
  
  if (length(most_common_words) > 1) {
    # Randomly select one of the most common words
    most_common_word <- sample(most_common_words, 1)
  } else {
    most_common_word <- most_common_words
  }
  
  return(most_common_word)
}

# Use rowwise to apply the function row by row and mutate to add a new column
shiftwork_exposure <- shiftwork_exposure %>%
  rowwise() %>%
  mutate(Most_Common_SW = find_most_common_word(SW_occupation)) %>%
  ungroup()  # Remove rowwise grouping

table(shiftwork_exposure$Most_Common_SW)

#replace 0 with NA for exposure to shiftwork - now only applies to those who did SW
shiftwork_exposure$total_hr_daySW[shiftwork_exposure$total_hr_daySW==0] <- NA
shiftwork_exposure$total_hr_nightSW[shiftwork_exposure$total_hr_nightSW==0] <- NA
shiftwork_exposure$total_hr_mixSW[shiftwork_exposure$total_hr_mixSW==0] <- NA
sum(shiftwork_exposure$total_hr_nightSW==0)
sum(is.na(shiftwork_exposure$total_hr_nightSW))


table1(~ total_hr + total_hr_daySW  +  total_hr_nightSW + total_hr_mixSW + early_NSW_YN.x + early_SW_YN.x  + Most_Common_SW       | factor(MS_YN), data=shiftwork_exposure, overall=FALSE, render.missing = NULL, render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(
vars = c('total_hr', 'total_hr_daySW', 'total_hr_nightSW', 'total_hr_mixSW', 
         'early_NSW_YN.x', 'early_SW_YN.x', 'Most_Common_SW'),
strata = c('MS_YN'), 
data = shiftwork_exposure, 
factorVars = c()
)


################################################################################################################
#
# 6.  Regression models
# 
##############################################################################################################


# MODELS FOR PROB OF BEING A SW OR EARLY SW AND MS
# ---------------------------------------------------------------------------------------------------------------------
#   
# #remove post 2015 diagnosis Filter out rows where eid is present in diag2015
# #summary_data_filter <- subset(summary_data, !(eid %in% diag_2015))
# 
# 
# # Specify the reference level using the relevel function
# summary_data$chronotype <- factor(summary_data$chronotype)
# summary_data$chronotype <- relevel(summary_data$chronotype, ref = "Morning")
# table(summary_data$chronotype)
# 
# 
# summary_data$smoking_status <- factor(summary_data$smoking_status)
# summary_data$smoking_status <- relevel(summary_data$smoking_status, ref = "Never")
# table(summary_data$smoking_status)
# 
# summary_data$child_obesity <- factor(summary_data$child_obesity)
# summary_data$child_obesity <- relevel(summary_data$child_obesity, ref = "About average")
# table(summary_data$child_obesity)
# 
# summary_data$alcohol_intake <- factor(summary_data$alcohol_intake)
# summary_data$alcohol_intake <- relevel(summary_data$alcohol_intake, ref = "Never")
# table(summary_data$alcohol_intake)
# 
# 
# # MS           final model looking at numbers that did early SW and any SW
# MS_model1 <- glm(MS_YN ~ age + sex + townsend ,
#                  data = summary_data, family = "binomial")
# MS_model2 <- glm(MS_YN ~ age + sex + townsend  
#                  + child_obesity  + chronotype,
#                  data = summary_data, family = "binomial")
# MS_model3 <- glm(MS_YN ~ age + sex + townsend  
#                 child_obesity  +  early_SW_YN + chronotype
#                  + alcohol_intake + smoking_status + time_outdoors_summer + early_SW_YN + bracket_SW_YN  ,
#                  data = summary_data, family = "binomial")
# summary(MS_model3)
# tab_model(MS_model1, MS_model2, MS_model3 ,show.intercept=FALSE)
# 
# # make categorical vars
# UKB_masterSW_cum$NSW_cat <- ifelse(UKB_masterSW_cum$total_hr_nightSW > 0, 1,0)
# UKB_masterSW_cum$mixSW_cat <- ifelse(UKB_masterSW_cum$total_hr_mixSW > 0, 1,0)
# UKB_masterSW_cum$daySW_cat <- ifelse(UKB_masterSW_cum$total_hr_daySW > 0, 1,0)


# MS           final model looking at numbers that did early SW and any NSW
# MS_model1 <- glm(MS_YN ~ age + sex + townsend ,
#                  data = summary_data, family = "binomial")
# MS_model2 <- glm(MS_YN ~ age + sex + townsend  
#                  + child_obesity + early_SW_YN + chronotype,
#                  data = summary_data, family = "binomial")
# MS_model3 <- glm(MS_YN ~ age + sex + townsend  
#                  + child_obesity + early_SW_YN + chronotype
#                  + alcohol_intake + smoking_status + time_outdoors_summer  + bracket_NSW_YN,
#                  data = summary_data, family = "binomial")
# summary(MS_model3)
# tab_model(MS_model1, MS_model2, MS_model3 ,show.intercept=FALSE)



#  MODELS FOR EXPOSURE TO SW 
---------------------------------------------------------------------------------------------------------------------
  

# Specify the reference level using the relevel function
UKB_masterSW_cum$chronotype <- factor(UKB_masterSW_cum$chronotype)
UKB_masterSW_cum$chronotype <- relevel(UKB_masterSW_cum$chronotype, ref = "Morning")
table(UKB_masterSW_cum$chronotype)

UKB_masterSW_cum$smoking_status <- factor(UKB_masterSW_cum$smoking_status)
UKB_masterSW_cum$smoking_status <- relevel(UKB_masterSW_cum$smoking_status, ref = "Never")
table(UKB_masterSW_cum$smoking_status)

UKB_masterSW_cum$child_obesity <- factor(UKB_masterSW_cum$child_obesity)
UKB_masterSW_cum$child_obesity <- relevel(UKB_masterSW_cum$child_obesity, ref = "About average")
table(UKB_masterSW_cum$child_obesity)

UKB_masterSW_cum$alcohol_intake <- factor(UKB_masterSW_cum$alcohol_intake)
UKB_masterSW_cum$alcohol_intake <- relevel(UKB_masterSW_cum$alcohol_intake, ref = "Never")
table(UKB_masterSW_cum$alcohol_intake)

UKB_masterSW_cum$NSW_normal <- UKB_masterSW_cum$NSW/UKB_masterSW_cum$total_hr
UKB_masterSW_cum$SW_normal <- UKB_masterSW_cum$SW/UKB_masterSW_cum$total_hr


# # MS and total hours of SW, Mix and NSW        
# UKB_masterSW_cum$SW <- UKB_masterSW_cum$total_hr_nightSW + UKB_masterSW_cum$total_hr_mixSW + UKB_masterSW_cum$total_hr_daySW
# 
# 
# 
# MSx_model1 <- glm(MS_YN ~ total_hr_mixSW + total_hr_daySW + total_hr_nightSW + total_hr ,
#                   data = UKB_masterSW_cum, family = "binomial")
# )
# MSx_model2 <- glm(MS_YN ~ total_hr_mixSW + total_hr_daySW + total_hr_nightSW + total_hr +age + sex + townsend + child_obesity  +                    chronotype,data = UKB_masterSW_cum, family = "binomial")
# MSx_model3 <- glm(MS_YN ~total_hr_mixSW + total_hr_daySW + total_hr_nightSW + total_hr + age + sex + townsend + child_obesity   +                   chronotype+ alcohol_intake + smoking_status + time_outdoors_summer +  early_SW_YN  ,
#                   data = UKB_masterSW_cum, family = "binomial")
# 
# 
# tab_model(MSx_model1, MSx_model2, MSx_model3 ,show.intercept=FALSE, transform = NULL)
# tab_model(MSx_model3 )
# 
# #check VIF for early life and ever sw
# library (car)
# vif_values <- car::vif(MSx_model3)
# print(vif_values)


#  zero model
# Create a binary indicator for whether the shift work hours are zero or non-zero
# Calculate the row sums for the specified columns.  This gives total hour of SW zero or not for part 1
UKB_masterSW_cum$total_shiftwork_hours <- rowSums(UKB_masterSW_cum[, c("total_hr_mixSW", "total_hr_nightSW", "total_hr_daySW")])

# for all SW
UKB_masterSW_cum$SW_YN <- ifelse(UKB_masterSW_cum$total_shiftwork_hours > 0& !is.na
                                 (UKB_masterSW_cum$total_shiftwork_hours), 1, 0)  
table(UKB_masterSW_cum$SW_YN)

# for NSW
UKB_masterSW_cum$NSW_YN <- ifelse(UKB_masterSW_cum$total_hr_nightSW > 0 & !is.na
                                 (UKB_masterSW_cum$total_hr_nightSW), 1, 0)  
table(UKB_masterSW_cum$NSW_YN)

# for mix SW
UKB_masterSW_cum$mixSW_YN <- ifelse(UKB_masterSW_cum$total_hr_mixSW > 0 & !is.na
                                 (UKB_masterSW_cum$total_hr_mixSW), 1, 0)  
table(UKB_masterSW_cum$mixSW_YN)

# for day SW
UKB_masterSW_cum$daySW_YN <- ifelse(UKB_masterSW_cum$total_hr_daySW > 0 & !is.na
                                    (UKB_masterSW_cum$total_hr_daySW), 1, 0)  
table(UKB_masterSW_cum$daySW_YN)
UKB_masterSW_cum$total_hr_daySW_std <- as.numeric(scale(UKB_masterSW_cum$total_hr_daySW))
UKB_masterSW_cum$total_hr_nightSW_std <- as.numeric(scale(UKB_masterSW_cum$total_hr_nightSW))
UKB_masterSW_cum$total_hr_mixSW_std <- as.numeric(scale(UKB_masterSW_cum$total_hr_mixSW))
UKB_masterSW_cum$total_hr_std <- as.numeric(scale(UKB_masterSW_cum$total_hr))
UKB_masterSW_cum$total_shiftwork_hours_std <- as.numeric(scale(UKB_masterSW_cum$total_shiftwork_hours))

data$total_shiftwork_hours_scaled <- as.numeric(scale(data$total_shiftwork_hours))

# model part 1 addresses ever doing SW
part1 <- glm(MS_YN ~ SW_YN + total_hr_std +
               age + sex + townsend + child_obesity + early_SW_YN  + 
               alcohol_intake  + time_outdoors_summer+ smoking_status, 
             family = binomial, data = UKB_masterSW_cum)
summary(part1)
tab_model(part1)


# model part 2 addresses total shiftwork hours and tests interaction with early life SW
part2 <- glm(MS_YN ~ total_shiftwork_hours_std*early_SW_YN + total_hr_std +
               age + sex + townsend + child_obesity + early_SW_YN  + 
               alcohol_intake  + time_outdoors_summer+ smoking_status, 
             family = binomial, data = UKB_masterSW_cum)
summary(part2)
tab_model(part2)

# model part 3 addresses total shiftwork hours divided by type
part3 <-  glm(MS_YN ~ total_hr_daySW_std + total_hr_mixSW_std + total_hr_nightSW_std +
                age + sex + townsend + child_obesity + early_SW_YN +  
                alcohol_intake + time_outdoors_summer  + smoking_status + total_hr_std , 
              family = binomial, data = UKB_masterSW_cum)
summary(part3)
tab_model(part3)

# Extract p-values from model
all_p_values <- summary(part3)$coefficients[,4]

# Identify which rows correspond to  shift work variables
shift_work_vars <- c("total_hr_daySW_std", "total_hr_mixSW_std", "total_hr_nightSW_std")
shift_work_indices <- which(names(all_p_values) %in% shift_work_vars)

# Extract only the p-values for shift work variables
shift_work_p_values <- all_p_values[shift_work_indices]

# Apply FDR correction only to these p-values
adjusted_p_values <- p.adjust(shift_work_p_values, method = "BH")

#================================================================================
# sensitivity analysis

# Subset the data to only include shift workers # sensitivity - repeat logistic model to check difference within shiftworkers
shift_workers <- subset(UKB_masterSW_cum, SW_YN == 1)

# part 4 total shiftwork hours only in shiftworkers - check dose with no zeros
part4 <- glm(MS_YN ~ total_shiftwork_hours_std*early_SW_YN+  age + sex + townsend + child_obesity + chronotype    + total_hr_std+
               alcohol_intake + smoking_status  + time_outdoors_summer, family = binomial, data = shift_workers)
summary(part4)
tab_model(part4)

# part 5 total shiftwork hours only in shiftworkers - check dose with no zeros
part5 <- glm(MS_YN ~ (total_hr_daySW_std) + (total_hr_mixSW_std) + (total_hr_nightSW_std)+  age + sex + townsend + child_obesity + chronotype    + total_hr_std + early_SW_YN + alcohol_intake + smoking_status  + time_outdoors_summer,  family = binomial, data = shift_workers)
summary(part5)
tab_model(part5)

#================================================================================
# permuataion analysis


library(boot)

# Function to fit the model and extract coefficients
boot_fn <- function(data, indices) {
  boot_data <- data[indices, ]  # Resample with replacement
  model <- glm(MS_YN ~ total_hr_daySW_std + total_hr_mixSW_std + 
                 total_hr_nightSW_std + age + sex + townsend + child_obesity + 
                 early_SW_YN + alcohol_intake + time_outdoors_summer + smoking_status + 
                 total_hr_std, 
               family = binomial, data = boot_data)
  return(coef(model))  # Extract coefficients
}

# Run bootstrap with 1000 resamples
set.seed(123)
boot_results <- boot(data = UKB_masterSW_cum, statistic = boot_fn, R = 1000)

# Compute confidence intervals
boot_ci <- boot.ci(boot_results, type = "perc")  # Percentile method

# Print results
print(boot_results)
print(boot_ci)
# Compute percentile CIs for all coefficients
boot_ci_all <- lapply(1:length(boot_results$t0), function(i) boot.ci(boot_results, type = "perc", index = i))

# Print the results
boot_ci_all
# Compute empirical p-values
empirical_pvals <- colMeans(boot_results$t >= 0) * 2  # Two-tailed test
print(empirical_pvals)




