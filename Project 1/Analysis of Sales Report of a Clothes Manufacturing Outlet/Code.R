# Background and Objective
# A high-end fashion retail store is looking to expand its products. 
# It wants to understand the market and find the current trends in the industry. 
# It has a database of all products with attributes, such as, style, material, 
# season, and the sales of the products over a period of two months. 

# Domain: Retail

# Dataset Description:
# There are two files provided: Atribute DataSet.xlsx and Dress Sales.xlsx


# Analysis Tasks:
# There are 5 goals for this project


#Import both datasets into R using RIO package

library(plyr)
library(dplyr)
library(rio)
library(raster)
library(data.table)
library(lattice)
library(ggplot2)
library(caret)
install.packages("caTools")
library(caTools)
library(forecast)
install.packages("corrplot")
library(corrplot)


Attrset <- import("Attribute DataSet.xlsx")
Sales.dates <- import("Dress Sales.xlsx")

View(Attrset)
str(Attrset)

#Factoring

Attrset <- mutate_if(Attrset, is.character, as.factor)

#Rename - adjust factor levels

levels(Attrset$Style)
Attrset$Style <-  revalue(Attrset$Style, c("sexy" = "Sexy"))

levels(Attrset$Price)
Attrset$Price <- revalue(Attrset$Price, c("high" = "High"))
Attrset$Price <- revalue(Attrset$Price, c("low" = "Low"))

levels(Attrset$Size)
Attrset$Size <- revalue(Attrset$Size, c("s" = "S"))
Attrset$Size <- revalue(Attrset$Size, c("small" = "S"))

levels(Attrset$Season)
Attrset$Season <- revalue(Attrset$Season, c("Automn" = "Autumn"))
Attrset$Season <- revalue(Attrset$Season, c("spring" = "Spring"))
Attrset$Season <- revalue(Attrset$Season, c("summer" = "Summer"))
Attrset$Season <- revalue(Attrset$Season, c("winter" = "Winter"))

levels(Attrset$NeckLine)
Attrset$NeckLine <-
  revalue(Attrset$NeckLine, c("sweetheart" = "Sweetheart"))

levels(Attrset$SleeveLength)
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("cap-sleeves" = "capsleeves"))
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("half" = "halfsleeve"))
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("sleeevless" = "sleeveless"))
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("sleevless" = "sleeveless"))
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("sleveless" = "sleeveless"))
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("threequater" = "threequarter"))
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("thressqatar" = "threequarter"))
Attrset$SleeveLength <- revalue(Attrset$SleeveLength,
                                c("urndowncollor" = "turndowncollor"))

levels(Attrset$Material)
Attrset$Material <- revalue(Attrset$Material, c("model" = "modal"))

levels(Attrset$FabricType)
Attrset$FabricType <-
  revalue(Attrset$FabricType, c("shiffon" = "chiffon"))
Attrset$FabricType <-
  revalue(Attrset$FabricType, c("flannael" = "flannel"))
Attrset$FabricType <-
  revalue(Attrset$FabricType, c("knitting" = "knitted"))
Attrset$FabricType <-
  revalue(Attrset$FabricType, c("sattin" = "satin"))
Attrset$FabricType <-
  revalue(Attrset$FabricType, c("wollen" = "woolen"))

levels(Attrset$Decoration)
Attrset$Decoration <-
  revalue(Attrset$Decoration, c("none" = "null"))

levels(Attrset$`Pattern Type`)
Attrset$`Pattern Type` <- revalue(Attrset$`Pattern Type`,
                                  c("leapord" = "leopard"))
Attrset$`Pattern Type` <- revalue(Attrset$`Pattern Type`,
                                  c("none" = "null"))

#second dataset

View(Sales.dates)

setnames(
  Sales.dates,
  old = c(
    "41314",
    "41373",
    "41434",
    "41495",
    "41556",
    "41617",
    "41315",
    "41374",
    "41435",
    "40400",
    "41557",
    "41618"
  ),
  new = c(
    "2/9/2013",
    "4/9/2013",
    "6/9/2013",
    "8/9/2013",
    "10/9/2013",
    "12/9/2013",
    "2/10/2013",
    "4/10/2013",
    "6/10/2013",
    "8/10/2013",
    "10/10/2013",
    "12/10/2013"
  )
)


#mean row wise
sapply(Sales.dates, class)

Sales.dates[Sales.dates == "removed"] <- NA
Sales.dates[Sales.dates == "Removed"] <- NA
Sales.dates[Sales.dates == "Orders"] <- NA

Sales.dates[9:14] <-
  mutate_if(Sales.dates[9:14], is.character , as.numeric)

Sales.dates <-
  (is.na(Sales.dates)) * rowMeans(Sales.dates[2:24], na.rm = T)[row(Sales.dates)] +
  replace(Sales.dates, is.na(Sales.dates), 0)

#formating digits
Sales.dates <-
  Sales.dates %>% mutate_at(vars(-Dress_ID), funs(round(., 0)))

##Total sales

Totalsales <- rowSums(Sales.dates[2:24])
TotalSalescol <- colSums(Sales.dates[2:24])
Sales.dates <- data.frame(Sales.dates, Totalsales)

#merge data frames

TotalDF <- merge(Attrset, Sales.dates)


# Question 1
# To automate the process of recommendations, the store needs to analyze
# the given attributes of the product, like the style, season, etc., and come up
# with a model to predict the recommendation of products
# (in binary output – 0 or 1) accordingly.

#Creating a new data frame without the dress_ID and NA rows
Attrib <- Attrset[, -1]
Attrib <- na.omit(Attrib)

#Model for Question 1
fit1 <-
  glm(
    Recommendation ~ Style + Price + Rating + Size + Season +
      NeckLine + SleeveLength + waiseline + Material + FabricType +
      Decoration + `Pattern Type`,
    family = binomial(),
    Attrib
  )
summary(fit1)

#Testing and validating the model
#Spliting data into 80% train and 20% test
set.seed(1)
inTrain <-
  createDataPartition(Attrib$Recommendation, p = 0.8, list = FALSE)
Training <- Attrib[inTrain, ]
Testing <- Attrib[-inTrain, ]

#making a column for prediction of recommendation
Testing$Predict.recommend <- 0

#Logistic model to perform on Training data
fit2 <-
  glm(
    Recommendation ~ Style + Price + Rating + Size + Season + NeckLine +
      SleeveLength + waiseline + Material + FabricType + Decoration +
      `Pattern Type`,
    family = binomial(),
    Training
  )
summary(fit2)

fit2$xlevels[["Style"]] <-
  union(fit2$xlevels[["Style"]], levels(Testing[["Style"]]))
fit2$xlevels[["NeckLine"]] <-
  union(fit2$xlevels[["NeckLine"]], levels(Testing[["NeckLine"]]))
fit2$xlevels[["Decoration"]] <-
  union(fit2$xlevels[["Decoration"]], levels(Testing[["Decoration"]]))
fit2$xlevels[["Pattern Type"]] <-
  union(fit2$xlevels[["Pattern Type"]], levels(Testing[["Pattern Type"]]))

Pred <- predict(fit2, Testing, type = "response")


#Setting threshold as 0.7 for the probability
Testing$Predict.recommend <- ifelse(Pred >= 0.7, 1, 0)
Testing$Predict.recommend

#Testing by confusion matrix
table(Testing$Recommendation,
      Testing$Predict.recommend,
      dnn = list("Actual", "Predicted"))

conf.matrix <-
  table(Testing$Recommendation, Testing$Predict.recommend)

#Accuracy of the model
sum(conf.matrix)
sum(diag(conf.matrix)) / sum(conf.matrix)

#Error of the model
1 - sum(diag(conf.matrix)) / sum(conf.matrix)


# Question 2
# In order to stock the inventory, the store wants to analyze the sales data 
# and predict the trend of total sales for each dress for an extended period 
# of three more alternative days.

is.vector(TotalSalescol)
TotalSalescol
Trend.Pred <- ts(TotalSalescol, start = 1, frequency = 7)
summary(Trend.Pred)

Trend.Pred <- auto.arima(Trend.Pred)
summary(Trend.Pred)
predict(Trend.Pred, 3)

forecast(Trend.Pred, 3)
plot(forecast(Trend.Pred, 3))


# Question 3
# To decide the pricing for various upcoming clothes, they wish to find how the 
# style, season, and material affect the sales of a dress and if the style of 
# the dress is more influential than its price.

ANOVA.Style <- aov(TotalDF$Totalsales ~ TotalDF$Style)
ANOVA.Season <- aov(TotalDF$Totalsales ~ TotalDF$Season)
ANOVA.Material <- aov(TotalDF$Totalsales ~ TotalDF$Material)
summary(ANOVA.Style)
summary(ANOVA.Season)
summary(ANOVA.Material)

fit3 <- lm(Totalsales ~ Style + Season + Material, TotalDF)
summary(fit3)

fit4 <- lm(Totalsales ~ Price + Style, TotalDF)
summary(fit4)


# Question 4
# Also, to increase sales, the management wants to analyze the attributes of
# dresses and find which are the leading factors affecting the sale of a dress.

TotalData <- TotalDF[,-c(1, 15:37)]
fit5 <- lm(Totalsales ~ ., TotalData)
summary(fit5)


# Question 5
# To regularize the rating procedure and find its efficiency, the store wants
# to find if the rating of the dress affects the total sales

corr <- cor.test(TotalDF$Totalsales, TotalDF$Rating)
corr4plot <- cor(TotalDF[, c(4, 38)])

corrplot(corr4plot)
