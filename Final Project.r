####################################################################################################################################

####################        Does the frame of mind influence how often people go to the doctor?        #############################

####################################################################################################################################
install.packages("dplyr")
library(dplyr)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
install.packages("ROCR")
library(ROCR)
install.packages("ipred")
library(ipred)

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

sapply(data, missingDataProp) > 0.75 

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




####################################################################################################################################

#################################################      Developing models       #####################################################

####################################################################################################################################

set.seed(76601)

split <- 0.75

split.index <- (runif(nrow(data)) < split)

train <- data[split.index,]
test <- data[!split.index,]

table(train$hc002_mod)

tree <- rpart(hc002_mod ~., data = train,
              method = "class",
              control = list(maxdepth = 6))


rpart.plot(tree, under=FALSE, tweak=1.3, fallen.leaves = TRUE)


forest <- randomForest(hc002_mod ~., data=train, ntree=300)

varImpPlot(forest)




####################################################################################################################################

###################################################      Testing models       ######################################################

####################################################################################################################################


classif_mx1 <- table(predict(tree, newdata = test, type="class"), test$hc002_mod)

classif_mx2 <- table(predict(forest, new = test, type="class"), test$hc002_mod)

classif_mx1[c(2,1),c(2,1)]

classif_mx2[c(2,1),c(2,1)]

evaluateModel <- function(classif_mx)
{
  true_positive <- classif_mx[1,1]
  true_negative <- classif_mx[2,2]
  false_positive <- classif_mx[1,2]
  false_negative <- classif_mx[2,1]
  condition_positive <- sum(classif_mx[,1])
  condition_negative <- sum(classif_mx[,2])
  predicted_positive <- sum(classif_mx[1,])
  predicted_negative <- sum(classif_mx[2,])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- (false_positive + false_negative) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  F1 <- (2*precision*sensitivity)/(precision+sensitivity)
  FPR <- false_positive / condition_negative
  FNR <- false_negative / condition_positive
  
  return(list(
    accuracy = accuracy,
    MER = MER,
    precision = precision,
    sensitivity = sensitivity,
    specificity = specificity,
    F1 = F1,
    FPR = FPR,
    FNR = FNR
  ))
}

evaluateModel(classif_mx1)
evaluateModel(classif_mx2)


######################################################       ROC       #########################################################

forecast1 <- predict(tree, newdata = test)
forecast1 <- as.vector(forecast1[,2])

pd1 <- prediction(forecast1, test$hc002_mod) #plotting data for the decision tree model

forecast2 <- predict(forest, newdata = test, type = "prob")[,2]

pd2 <- prediction(forecast2, test$hc002_mod) #plotting data for the random forest model

# ROC decision tree
plot(performance(pd1, 'tpr', 'fpr'), main="ROC", lwd=2, col="red")

# ROC random forest
plot(performance(pd2, 'tpr', 'fpr'), main="ROC", lwd=2, col="blue")

# ROC for both
plot(performance(pd1, 'tpr', 'fpr'), main="ROC", lwd=2, col="red")
plot(performance(pd2, 'tpr', 'fpr'), add = TRUE, main="ROC", lwd=2, col="blue")


######################################################       AUC      #########################################################

# AUC decision tree
performance(pd1, 'auc')@y.values

# AUC random forest
performance(pd2, 'auc')@y.values


###############################################       SE/SP trade-off      ####################################################

# SE/SP trade-off decision tree
plot(performance(pd1, "sens", "spec"), main="Sensitivity / Specificity Trade Off",
     lwd = 2, col = "red")

# SE/SP trade-off random forest
plot(performance(pd2, "sens", "spec"), main="Sensitivity / Specificity Trade Off",
     lwd = 2, col = "blue")

# SE/SP trade-off both
plot(performance(pd1, "sens", "spec"), main="Sensitivity / Specificity Trade Off",
     lwd = 2, col = "red")
plot(performance(pd2, "sens", "spec"), add = T, main="Sensitivity / Specificity Trade Off",
     lwd = 2, col = "blue")


##################################################      Lift chart     #######################################################

# Lift chart decision tree
plot(performance(pd1, "lift", "rpp"), main = "Lift Chart", lwd = 2, col = "red")

# Lift chart random forest
plot(performance(pd2, "lift", "rpp"), main = "Lift Chart", lwd = 2, col = "blue")

# Lift both
plot(performance(pd2, "lift", "rpp"), main = "Lift Chart", lwd = 2, col = "blue")
plot(performance(pd1, "lift", "rpp"), add=T, main = "Lift Chart", lwd = 2, col = "red")



################################################     Cross Validation    #####################################################

accs <- rep(0,10)

for (i in 1:10) {
  indices <- (((i-1) * round((1/10)*nrow(data))) + 1):((i*round((1/10) * nrow(data))))
  train2 <- data[-indices,]
  test2 <- data[indices,]
  
  tree2 <- rpart(hc002_mod ~., data = train2,
                 method = "class",
                 control = list(maxdepth = 6))
  
  conf <- table(predict(tree2, newdata=test2, type="class"), test2$hc002_mod)
  
  accs[i] <- sum(diag(conf))/sum(conf)
  
  result <- sum(accs)/10
}

# Accuracy of model with cross validation
result


###################################################     Bootstrap    ########################################################

mod <- bagging(hc002_mod ~ ., data=data, coob=TRUE)
mod

