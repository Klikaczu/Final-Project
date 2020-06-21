####################        Does the frame of mind influence how often people go to the doctor?        ##############################

setwd("~/Final-Project")

load("Data/easySHARE.rda")

data <- dplyr::filter(easySHARE_rel7_0_0, wave==7, country_mod == "616")

str(data)

hist(data$age)


#######################################      Choosing potentially useful variables       ###########################################

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


















