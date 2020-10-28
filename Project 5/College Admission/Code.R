# DESCRIPTION

# Background and Objective: 
#   Every year thousands of applications are being submitted by international
# students for admission in colleges of the USA. It becomes an iterative task 
# for the Education Department to know the total number of applications received
# and then compare that data with the total number of applications successfully 
# accepted and visas processed. Hence to make the entire process easy, the
# education department in the US analyze the factors that influence the
# admission of a student into colleges. The objective of this exercise is to
# analyse the same.
 
# Domain: Education
 
# Dataset Description:
 
#   Attribute	Description
# GRE	            Graduate Record Exam Scores
# GPA	            Grade Point Average
# Rank	          It refers to the prestige of the undergraduate institution.
#                 The variable rank takes on the values 1 through 4. 
#                 Institutions with a rank of 1 have the highest prestige, while
#                 those with a rank of 4 have the lowest.
# Admit	          It is a response variable; admit/don’t admit is a binary
#                 variable where 1 indicates that student is admitted and 0
#                 indicates that student is not admitted. 
# SES	            SES refers to socioeconomic status: 1 - low, 2 - medium, 
#                 3 - high.
# Gender_male	    Gender_male (0, 1) = 0 -> Female, 1 -> Male
# Race	          Race – 1, 2, and 3 represent Hispanic, Asian, and
#                 African-American 

# Analysis Tasks: Analyze the historical data and determine the key drivers for 
#                 admission.

# Predictive: 
   
#   Find the missing values. (if any, perform missing value treatment)
#   Find outliers (if any, then perform outlier treatment)
#   Find the structure of the data set and if required, transform the numeric
# data type to factor and vice-versa.
#   Find whether the data is normally distributed or not. Use the plot to
# determine the same. 
#   Normalize the data if not normally distributed.
#   Use variable reduction techniques to identify significant variables.
#   Run logistic model to determine the factors that influence the admission
# process of a student (Drop insignificant variables) 
#   Calculate the accuracy of the model and run validation techniques.
#   Try other modeling techniques like decision tree and SVM and select a
# champion model 
#   Determine the accuracy rates for each kind of model 
#   Select the most accurate model 
#   Identify other Machine learning or statistical techniques
 

# Descriptive: 
#   Categorize the average of grade point into High, Medium, and Low (with
# admission probability percentages) and plot it on a point chart.  
#   Cross grid for admission variables with GRE Categorization is shown below:
   
#   GRE	        Categorized
#  0-440	          Low
# 440-580	        Medium
#   580+	         High

library(rio)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lattice)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(e1071)
library(class)
library(naivebayes)
library(randomForest)

CollegeDF <- import("College_admission.csv")

## Predictive:
# Find the missing values. (if any, perform missing value treatment)

which(is.na(CollegeDF))

# Find outliers (if any, then perform outlier treatment)

ggplot(CollegeDF,
       aes(y = gre, label = gre)) +
  geom_boxplot() +
  labs(title = "Outliers - Graduate Record Exam Scores") +
  theme(plot.title = element_text(hjust = 0.5))
CollegeDF <- CollegeDF[CollegeDF$gre > 305, ]

ggplot(CollegeDF,
       aes(y = gpa, label = gpa)) +
  geom_boxplot() +
  labs(title = "Outliers - Grade Point Average") +
  theme(plot.title = element_text(hjust = 0.5))
CollegeDF <- CollegeDF[CollegeDF$gpa > 2.5, ]


# Find the structure of the data set and if required, transform the numeric
# data type to factor and vice-versa.

summary(CollegeDF)
str(CollegeDF)

CollegeDF$admit<-as.factor(CollegeDF$admit)
CollegeDF$ses<-as.factor(CollegeDF$ses)
CollegeDF$Gender_Male<-as.factor(CollegeDF$Gender_Male)
CollegeDF$Race<-as.factor(CollegeDF$Race)
CollegeDF$rank<-as.factor(CollegeDF$rank)
str(CollegeDF)


# Find whether the data is normally distributed or not. Use the plot to
# determine the same. 

shapiro.test(CollegeDF$gre)
ggqqplot(CollegeDF$gre)
ggdensity(CollegeDF$gre, 
          main = "Density plot of Graduate Record Exam Scores",
          xlab = "GRE",
          ylab = "Density") +
  theme(plot.title = element_text(hjust = 0.5))


shapiro.test(CollegeDF$gpa)
ggqqplot(CollegeDF$gpa)
ggdensity(CollegeDF$gpa, 
          main = "Density plot of Grade Point Average",
          xlab = "GPA",
          ylab = "Density") + 
  theme(plot.title = element_text(hjust = 0.5))


# Normalize the data if not normally distributed.

CollegeDF$gre <- scale(CollegeDF$gre)
CollegeDF$gpa <- scale(CollegeDF$gpa)


# Use variable reduction techniques to identify significant variables.
# Run logistic model to determine the factors that influence the admission
# process of a student (Drop insignificant variables) 

set.seed(1)
inTrain <-
  createDataPartition(CollegeDF$admit, p = 0.7, list = FALSE)
Training <- CollegeDF[inTrain, ]
Testing <- CollegeDF[-inTrain, ]

#Linear Regression model
fit1 <- glm(admit ~ ., Training,
               family = "binomial")
summary(fit1)

# We keep only the significant variables for a new model
fit2 <- glm(admit ~ gpa + rank + gre, Training,
               family = "binomial")
summary(fit2)

# Calculate the accuracy of the model and run validation techniques.

Pred <- predict(fit2,Testing, type="response")
Testing$Predict.recommend <- ifelse(Pred >= 0.5, 1, 0)

table(Testing$admit,
      Testing$Predict.recommend,
      dnn = list("Actual", "Predicted"))

table(Testing$admit, Testing$Predict.recommend)
Testing$Predict.recommend <- as.factor(Testing$Predict.recommend)
confusionMatrix(Testing$Predict.recommend, Testing$admit)
# Accuracy of the model is 70.09%

#K-Fold Cross Validation

train_control <- trainControl(method = "cv", number = 5)
fit3 <-
  train(
    admit ~ gpa + rank + gre,
    CollegeDF,
    trControl = train_control,
    method = "glm",
    family = "binomial"
  )
fit3$resample
fit3$results

#  We get 70.91% accuracy when we use K-Fold Cross-Validation. K-Fold 
# Cross-Validation addresses the problem of over-fitting of our model and is 
# providing us with the right picture by giving us the correct, more unbiased 
# and real evaluation score of our model.


# Try other modeling techniques like decision tree and SVM and select a
# champion model 
# Determine the accuracy rates for each kind of model 
# Select the most accurate model 

# Decision Tree

fit4 <-
  rpart(
    admit ~ gpa + rank + gre,
    Training,
    method = "class",
    control = rpart.control(minsplit = 30, cp = 0.01)
  )
rpart.plot(fit4)
Pred_DT <- predict(fit4, Testing, type="class")

table(Pred_DT, Testing$admit, dnn = list("Actual", "Predicted"))
confusionMatrix(Pred_DT, Testing$admit)
# Accuracy of the model is 69.23%

# SVM

fit5 <- svm(admit ~ gpa+rank+gre, Training, kernel="linear",
            scale = T)
summary(fit5)
Pred_SVM <- predict(fit5,Testing, type="response")

table(Pred_SVM, Testing$admit, dnn = list("Actual", "Predicted"))
confusionMatrix(Pred_SVM, Testing$admit)
# Accuracy of the model is 69.23%

# The most accurate model is the linear regression with a validated accuracy
# of 70.91%.


# Identify other Machine learning or statistical techniques

# kNN

sqrt(275)
fit6 <-
  knn(Training[, -1], Testing[-c(1, 8)], cl = Training[, 1], k = 16)

table(Testing[, 1], fit6, dnn = list("Actual", "Predicted"))
confusionMatrix(fit6,Testing$admit)
# Accuracy of the model is 70.09%


# Naive Bayes

fit7 <- naive_bayes(admit ~ gpa + rank + gre, Training)
Pred_NV <- predict(fit7, Testing)

table(Pred_NV, Testing$admit, dnn = list("Actual", "Predicted"))
confusionMatrix(Pred_NV, Testing$admit)
# Accuracy of the model is 64.10%

# Random Forest

fit8 <- randomForest(admit ~ gpa+rank+gre, Training)
Pred_RF <- predict(fit8,Testing)

table(Pred_RF, Testing$admit, dnn = list("Actual", "Predicted"))
confusionMatrix(Pred_RF, Testing$admit)
# Accuracy of the model is 70.94%


## Descriptive: 
#   Categorize the average of grade point into High, Medium, and Low (with
# admission probability percentages) and plot it on a point chart.  

College <- import("College_admission.csv")
CollegeGRE <-
  College %>% mutate(Categorized = case_when(
    gre < 440 ~ "Low", 
    gre < 580 ~ "Medium", 
    gre >= 580 ~ "High"))

count(filter(CollegeGRE, Categorized == "Low"))
count(filter(CollegeGRE, Categorized == "Medium"))
count(filter(CollegeGRE, Categorized == "High"))

Admit_GPAlow <- filter(CollegeGRE, Categorized == "Low")
count(filter(CollegeGRE, Categorized == "Low"))
sum(Admit_GPAlow$admit == 1)
(4 / 38) * 100
# For category "Low" the admission probability is 10.53%

Admit_GPAmedium <- filter(CollegeGRE, Categorized == "Medium")
count(filter(CollegeGRE, Categorized == "Medium"))
sum(Admit_GPAmedium$admit == 1)
(39 / 136) * 100
# For category "Medium" the admission probability is 28.68%

Admit_GPAhigh <- filter(CollegeGRE, Categorized == "High")
count(filter(CollegeGRE, Categorized == "High"))
sum(Admit_GPAhigh$admit == 1)
(84 / 226) * 100
# For category "High" the admission probability is 37.17%

ggplot(CollegeGRE,
       aes(x = gpa, y = gre, colour = factor(admit), shape = factor(Categorized))) +
  geom_point() +
  labs(x = "GPA",
       y = "GRE",
       shape = "Categorized",
       colour = "Admited")