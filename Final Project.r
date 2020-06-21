####################################################################################################################################

####################        Does the frame of mind influence how often people go to the doctor?        #############################

####################################################################################################################################

library(dplyr)

setwd("~/Final-Project")

load("Data/easySHARE.rda")

data <- dplyr::filter(easySHARE_rel7_0_0, wave==7, country_mod == "616")

str(data)

hist(data$age)



####################################################################################################################################

#######################################      Choosing potentially useful variables       ###########################################

####################################################################################################################################


# Dependent variable:

# hc002_mod - dependent variable; quantity of doctor's appointment per year, later on it will be converted into a dummy variable 


# Independet variables - deterimnants of people's frame of mind:

# partnerinhh - Living with spouse/partner
# mother_alive - Is natural parent alive: mother
# father_alive - Is natural parent alive: father
# silblings_alive - Number of siblings alive
# ch001_ - Number of children
# ch021_mod - Number of grandchildren
# ch007_hh - At least one child in same household
# ch007_kh - Residential proximity of children
# sp008_ - Given help to others outside the household
# hhsize - Household size
# casp - CASP-12 score (Quality of life)
# br010_mod - Drinking behavior
# ep005_ - Current job situation
# co007_ - Household able to make ends meet
# thinc_m - Househould net income, imputed
# sphus - Self-perceived health (US version)
# maxgrip - Maximum of grip strength measure (from gv_health)
# adlwa - Activities of Daily Living w&h Index
# adla - Activities of Daily Living Index
# iadla - Instrumental Activities of Daily Living Indices
# iadlza - Instrumental Activities of Daily Living Indices
# mobilityind - Mobility Index
# lgmuscle - Large Muscle Index
# grossmotor - Gross Motor Skills Index
# finemotor - Fine Motor Skills Index
# bmi - Body Mass Index
# ep013_mod - Working hours per week
# ac002d1 - Done voluntary or charity work
# ac002d2 - Cared for a sick or disabled adult
# ac002d3 - Provided help to family, friends or neighbors
# ac002d4 - Attended an educational or training course
# ac002d5 - Gone to a sport, social or other kind of club
# ac002d6 - Taken part in a religious organization (church, synagogue, mosque, etc.)
# ac002d7 - Taken part in a political or community-related organization
# chronic_mod - Number of chronic diseases



####################################################################################################################################

#############################################      Dealing with missing values       ###############################################

####################################################################################################################################

sum(is.na(data)) # 0

# Missing codes:

# -3: “implausible value/suspected wrong” -7: “not yet coded”
# -9: “not applicable filtered”
# -10: “SHARELIFE interview” (only in wave 7)
# -11: “regular interview” (only in wave 7)
# -12: “don’t know / refusal”
# -13: “not asked in this wave”
# -14: “not asked in this country”
# -15: “no information”
# -16: “no drop-off (information in drop-off in this wave)”


# Checking percentage of missing data for each variable:

missingDataProp <- function(x){
  1 - sum(x >= 0)/4704
}

data <- data %>% select(hc002_mod,
                        age,
                        partnerinhh,
                        mother_alive,
                        father_alive,
                        siblings_alive,
                        ch001_,
                        ch021_mod,
                        ch007_hh,
                        ch007_km,
                        sp008_,
                        hhsize,
                        casp,
                        br010_mod,
                        ep005_,
                        co007_,
                        thinc_m,
                        sphus,
                        maxgrip,
                        adlwa,
                        adla,
                        iadla,
                        iadlza,
                        mobilityind,
                        lgmuscle,
                        grossmotor,
                        finemotor,
                        bmi,
                        ep013_mod,
                        ac002d1, 
                        ac002d2, 
                        ac002d3, 
                        ac002d4,
                        ac002d5, 
                        ac002d6, 
                        ac002d7, 
                        chronic_mod)

sapply(data, missingDataProp) > 0.5 

# Erasing variables in which more than 50% are missing values


data <- data %>% filter(hc002_mod >= 0,
                        age > 50,
                        partnerinhh >= 0,
                        mother_alive >= 0,
                        father_alive >= 0,
                        siblings_alive >= 0,
                        hhsize >= 0,
                        casp >= 0,
                        ep005_ >= 0,
                        co007_ >= 0,
                        sphus >= 0,
                        maxgrip >= 0,
                        adlwa >= 0,
                        adla >= 0,
                        iadla >= 0,
                        iadlza >= 0,
                        mobilityind >= 0,
                        lgmuscle >= 0,
                        grossmotor >= 0,
                        finemotor >= 0,
                        bmi >= 0,
                        chronic_mod >= 0) %>%
  select(hc002_mod,
         age,
         partnerinhh,
         mother_alive,
         father_alive,
         siblings_alive,
         hhsize,
         casp,
         ep005_,
         co007_,
         sphus,
         maxgrip,
         adlwa,
         adla,
         iadla,
         iadlza,
         mobilityind,
         lgmuscle,
         grossmotor,
         finemotor,
         bmi,
         chronic_mod)



####################################################################################################################################

############################################      Converting dependent variable       ##############################################

####################################################################################################################################

hist(data$hc002_mod)

# We assume that a person that goes to the doctor too frequently has an appointment each month and a half.

if (typeof(data$hc002_mod) == "integer") data$hc002_mod <- 
  (ifelse(data$hc002_mod >= 8, 1, 0))

data$hc002_mod <- as.factor(data$hc002_mod)

plot(data$hc002_mod)
table(data$hc002_mod)
table(data$hc002_mod)/3802

# 32% sees the doctor too many times a year



