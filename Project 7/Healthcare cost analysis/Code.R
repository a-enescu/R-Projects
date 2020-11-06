# DESCRIPTION

# Background and Objective:
   
#   A nationwide survey of hospital costs conducted by the US Agency for
# Healthcare consists of hospital records of inpatient samples. The given data
# is restricted to the city of Wisconsin and relates to patients in the age
# group 0-17 years. The agency wants to analyze the data to research on
# healthcare costs and their utilization.
 
# Domain: Healthcare
 
# Dataset Description:
   
#   Here is a detailed description of the given dataset:
   
# Attribute         Description
# Age 	            Age of the patient discharged
# Female 	          A binary variable that indicates if the patient is female
# Los	              Length of stay in days
# Race 	            Race of the patient (specified numerically)
# Totchg	          Hospital discharge costs
# Aprdrg	          All Patient Refined Diagnosis Related Groups

# Analysis to be done: 
   
# 1. To record the patient statistics, the agency wants to find the age
# category of people who frequently visit the hospital and has the maximum
# expenditure.

# 2. In order of severity of the diagnosis and treatments and to find out the
# expensive treatments, the agency wants to find the diagnosis-related group
# that has maximum hospitalization and expenditure.
 
# 3. To make sure that there is no malpractice, the agency needs to analyze if
# the race of the patient is related to the hospitalization costs.

# 4. To properly utilize the costs, the agency has to analyze the severity of
# the hospital costs by age and gender for the proper allocation of resources.

# 5. Since the length of stay is the crucial factor for inpatients, the agency
# wants to find if the length of stay can be predicted from age, gender, and 
# race.
 
# 6. To perform a complete analysis, the agency wants to find the variable that
# mainly affects hospital costs.

library(rio)
library(dplyr)

HospitalDF <- import("1555054100_hospitalcosts.xlsx")

head(HospitalDF)
str(HospitalDF)
summary(HospitalDF)

# Changing Age, Female, Race variables into factors

HospitalDF$AGE <- as.factor(HospitalDF$AGE)
HospitalDF$FEMALE <- as.factor(HospitalDF$FEMALE)
HospitalDF$RACE <- as.factor(HospitalDF$RACE)
HospitalDF$APRDRG <- as.factor(HospitalDF$APRDRG)

str(HospitalDF)

# 1. To record the patient statistics, the agency wants to find the age
# category of people who frequently visit the hospital and has the maximum
# expenditure.

summary(HospitalDF$AGE)

# Infants under 1 year group has the most hospital visits (307)

HospitalDF %>%
  group_by(AGE) %>%
  summarise(Expenditure = sum(TOTCHG)) %>%
  arrange(desc(Expenditure))

# Infants under 1 year group has the most hospital costs (678118).
# Based on the above outputs we can conclude that hospital costs are directly
# proportional to hospital visits


# 2. In order of severity of the diagnosis and treatments and to find out the
# expensive treatments, the agency wants to find the diagnosis-related group
# that has maximum hospitalization and expenditure.

count(HospitalDF, APRDRG, sort = T)

HospitalDF %>%
  group_by(APRDRG) %>%
  summarise(Expenditure = sum(TOTCHG)) %>%
  arrange(desc(Expenditure))

# Diagnosis-related group 640 has the most hospitalizations (267 out of 500)
# and the highest expenditure (437978)


# 3. To make sure that there is no malpractice, the agency needs to analyze if
# the race of the patient is related to the hospitalization costs.

# Remove NA value

HospitalDF <- na.omit(HospitalDF)

fit1 <- aov(HospitalDF$TOTCHG ~ HospitalDF$RACE)
summary(fit1)
summary(HospitalDF$RACE)

# As the p-value is higher than the significance level 0.05, we can conclude
# that there are no significant differences between the groups in the model
# summary. So by accepting the Null hypothesis we can say that there is no
# relationship between race and hospitalization costs.
# Furthermore we can observe that we don't have a normal distributed data for
# Race, where in group 1 we have 484 patients out of 500. we can conclude that
# we don't have enough information to say if tha race is affecting the costs.


# 4. To properly utilize the costs, the agency has to analyze the severity of
# the hospital costs by age and gender for the proper allocation of resources.

fit2 <- glm(TOTCHG ~ AGE + FEMALE, family = gaussian(), HospitalDF)
summary(fit2)
summary(HospitalDF$FEMALE)

# From above analysis we can conclude that the costs are not affected by gender,
# but are significant affected by age.


# 5. Since the length of stay is the crucial factor for inpatients, the agency
# wants to find if the length of stay can be predicted from age, gender, and 
# race.

fit3 <- glm(LOS ~ AGE + FEMALE + RACE, family = gaussian(), HospitalDF)
summary(fit3)

# The p-values for all independent variables are high, so we can say that there
# is no relationship between the variables. We can conclude that based on the
# given data we can not predict the length of stay based on age, gender or race.


# 6. To perform a complete analysis, the agency wants to find the variable that
# mainly affects hospital costs.

fit4 <- lm(TOTCHG ~ ., HospitalDF)
summary(fit4)

# Based on the above analysis we concluded that Total Charge is highly affected
# by: age,length of stay and , Diagnosis-related groups.
# Gender is moderately affecting the Total Charge.
# Race has no impact on Total Charge
