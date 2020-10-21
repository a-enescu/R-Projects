# DESCRIPTION
# 
# Background and Objective:
#   The web analytics team of www.datadb.com is interested to understand the 
# web activities of the site, which are the sources used to access the website. 
# They have a database that states the keywords of time in the page, source 
# group, bounces, exits, unique page views, and visits.

# Domain: Web

# Dataset Description:
#   The variables in the dataset are defined here for better understanding:

# Analysis Tasks:
#   The team is targeting the following issues:
#   
# 1. The team wants to analyze each variable of the data collected through data
# summarization to get a basic understanding of the dataset and to prepare for
# further analysis.

# 2. As mentioned earlier, a unique page view represents the number of sessions
# during which that page was viewed one or more times. A visit counts all 
# instances, no matter how many times the same visitor may have been to your 
# site. So the team needs to know whether the unique page view value depends 
# on visits.

# 3. Find out the probable factors from the dataset, which could affect the 
# exits. Exit Page Analysis is usually required to get an idea about why a user
# leaves the website for a session and moves on to another one. Please keep in 
# mind that exits should not be confused with bounces.

# 4. Every site wants to increase the time on page for a visitor. This increases
# the chances of the visitor understanding the site content better and hence 
# there are more chances of a transaction taking place. Find the variables which
# possibly have an effect on the time on page.

# 5. A high bounce rate is a cause of alarm for websites which depend on visitor
# engagement. Help the team in determining the factors that are impacting the
# bounce.

library(rio)

webDF <- import("1555058318_internet_dataset.xlsx")

# 1. The team wants to analyze each variable of the data collected through data
# summarization to get a basic understanding of the dataset and to prepare for
# further analysis.

View(webDF)
str(webDF)
webDF$Continent <- as.factor(webDF$Continent)
webDF$Sourcegroup <- as.factor(webDF$Sourcegroup)
summary(webDF)


#Summary grouped by Continent
table(webDF$Continent)
by(webDF, webDF$Continent, summary)

#Summary grouped by Source group
table(webDF$Sourcegroup)
by(webDF, webDF$Sourcegroup, summary)


# 2. As mentioned earlier, a unique page view represents the number of sessions
# during which that page was viewed one or more times. A visit counts all 
# instances, no matter how many times the same visitor may have been to your 
# site. So the team needs to know whether the unique page view value depends 
# on visits.

# Option 1: ANOVA
UnqPg_View <- aov(Uniquepageviews ~ Visits, webDF)
summary(UnqPg_View)

#Option 2: Correlation function
cor(webDF$Uniquepageviews, webDF$Visits) 


# 3. Find out the probable factors from the dataset, which could affect the 
# exits. Exit Page Analysis is usually required to get an idea about why a user
# leaves the website for a session and moves on to another one. Please keep in 
# mind that exits should not be confused with bounces.

Ext <- aov(Exits ~ ., webDF)
summary(Ext)


# 4. Every site wants to increase the time on page for a visitor. This increases
# the chances of the visitor understanding the site content better and hence 
# there are more chances of a transaction taking place. Find the variables which
# possibly have an effect on the time on page.

Time_Page <- aov(Timeinpage ~ ., webDF)
summary(Time_Page)


# 5. A high bounce rate is a cause of alarm for websites which depend on visitor
# engagement. Help the team in determining the factors that are impacting the
# bounce.

fit1 <- glm(
  BouncesNew ~ Timeinpage + Continent + Exits + Sourcegroup +
    Uniquepageviews + Visits,
  webDF,
  family = "binomial"
)
summary(fit1)