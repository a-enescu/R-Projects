# DESCRIPTION

# Background of Problem Statement:
#   A UK-based online retail store has captured the sales data for different
# products for the period of one year (Nov 2016 to Dec 2017). The organization
# sells gifts primarily on the online platform. The customers who make a
# purchase consume directly for themselves. There are small businesses that buy
# in bulk and sell to other customers through the retail outlet channel.

# Project Objective:
#   Find significant customers for the business who make high purchases of their
# favourite products. The organization wants to roll out a loyalty program to
# the high-value customers after identification of segments. Use the clustering
# methodology to segment customers into groups:
  
# Domain: E-commerce
 
# Dataset Description:
#   This is a transnational dataset that contains all the transactions occurring
# between Nov-2016 to Dec-2017 for a UK-based online retail store.


# Attribute	     Description
# InvoiceNo	     Invoice number (A 6-digit integral number uniquely assigned to
#                each transaction)
# StockCode	     Product (item) code
# Description 	 Product (item) name
# Quantity 	     The quantities of each product (item) per transaction
# InvoiceDate	   The day when each transaction was generated
# UnitPrice	     Unit price (Product price per unit)
# CustomerID	   Customer number (Unique ID assigned to each customer)
# Country	       Country name (The name of the country where each customer
#                resides)

# Analysis tasks to be performed: 
#   Use the clustering methodology to segment customers into groups:
#   Use the following clustering algorithms:
  
#   K means
#   Hierarchical

# • Identify the right number of customer segments.
# • Provide the number of customers who are highly valued.
# • Identify the clustering algorithm that gives maximum accuracy and explains
#   robust clusters.
# • If the number of observations is loaded in one of the clusters, break down
# that cluster further using the clustering algorithm. [ hint: Here loaded means
# if any cluster has more number of data points as compared to other clusters
# then split that clusters by increasing the number of clusters and observe,
# compare the results with previous results.]

library(rio)
library(DataExplorer)
library(ggplot2)
library(factoextra)
library(NbClust)

EcommDF <- import("Ecommerce.csv")

head(EcommDF)
summary(EcommDF)
str(EcommDF)

# Removing empty column
EcommDF <- EcommDF[, 1:8]

# Checking for NA values
options(scipen = 999)
plot_missing(EcommDF, title = "Missing values in every column")

# Removing NA values
EcommDF <- na.omit(EcommDF)

# Leaving only unique customers

EcommDF <- EcommDF[unique(EcommDF$CustomerID), ]

# Remove Quantity with negative values

EcommDF <- EcommDF[EcommDF$Quantity >= 0,]

# Add a "Total Spent" column

EcommDF$TotalSpent <- EcommDF$Quantity * EcommDF$UnitPrice


## Variables should be numeric

EcommDF_Num <- EcommDF[, -c(3, 5, 8)]
str(EcommDF_Num)

EcommDF_Num$InvoiceNo <- as.numeric(EcommDF_Num$InvoiceNo)
EcommDF_Num$StockCode <- as.numeric(EcommDF_Num$StockCode)
EcommDF_Num$Quantity <- as.numeric(EcommDF_Num$Quantity)
EcommDF_Num$CustomerID <- as.numeric(EcommDF_Num$CustomerID)

summary(EcommDF_Num)

#Outlier removal

Quantity_LT <- mean(EcommDF_Num$Quantity) - 2 * sd(EcommDF_Num$Quantity)
Quantity_UT <- mean(EcommDF_Num$Quantity) + 2 * sd(EcommDF_Num$Quantity)

UnitPrice_LT <- mean(EcommDF_Num$UnitPrice) - 2 * sd(EcommDF_Num$UnitPrice)
UnitPrice_UT <- mean(EcommDF_Num$UnitPrice) + 2 * sd(EcommDF_Num$UnitPrice)

# Threshold

EcommDF_Num$Quantity <-
  ifelse(EcommDF_Num$Quantity > Quantity_UT, Quantity_UT, EcommDF_Num$Quantity)
EcommDF_Num$Quantity <-
  ifelse(EcommDF_Num$Quantity < Quantity_LT, Quantity_LT, EcommDF_Num$Quantity)

EcommDF_Num$UnitPrice <-
  ifelse(EcommDF_Num$UnitPrice > UnitPrice_UT, UnitPrice_UT, EcommDF_Num$UnitPrice)
EcommDF_Num$UnitPrice <-
  ifelse(EcommDF_Num$UnitPrice < UnitPrice_LT, UnitPrice_LT, EcommDF_Num$UnitPrice)

# Remove scaling effect from data
EcommDF_Num <- scale(EcommDF_Num)
summary(EcommDF_Num)

# Removing NA values
EcommDF_Num <- na.omit(EcommDF_Num)

## K-means clustering

# Elbow method
fviz_nbclust(EcommDF_Num, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(EcommDF_Num, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123)
fviz_nbclust(EcommDF_Num, kmeans, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# NbClust 
NC <- NbClust(EcommDF_Num, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")

barplot(table(NC$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by Criteria")

# Cluster plot
KM_res <- kmeans(EcommDF_Num, centers = 2)
fviz_cluster(KM_res, EcommDF_Num, ellipse.type = "norm")

KM_res$centers

## Hierarchical clustering

#calculate distances between observations
Dist.Ecomm <- dist(EcommDF_Num, method = 'euclidean')

HC <- hclust(Dist.Ecomm, method = "single")
plot(HC, hang = -1)
rect.hclust(HC, k = 2, border = 2:5)

## • Identify the right number of customer segments.

# * According to the majority rule, the best number of clusters is  2 

## • Provide the number of customers who are highly valued.

sum(EcommDF$TotalSpent > mean(EcommDF$TotalSpent))

## • Identify the clustering algorithm that gives maximum accuracy and explains
##   robust clusters.

# For the given dataset the maximum accuracy is obtained by using partitioning
# method clustering, more exactly the K-means clustering