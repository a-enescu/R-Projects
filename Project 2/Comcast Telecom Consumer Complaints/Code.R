# DESCRIPTION
#
# Comcast is an American global telecommunication company. The firm has been
# providing terrible customer service. They continue to fall short despite
# repeated promises to improve. Only last month (October 2016) the authority
# fined them a $2.3 million, after receiving over 1000 consumer complaints.

# The existing database will serve as a repository of public customer complaints
# filed against Comcast.
# It will help to pin down what is wrong with Comcast's customer service.

# Data Dictionary

# Ticket #: Ticket number assigned to each complaintnt
# Date: Date of complaint
# Time: Time of complaint
# Received Via: Mode of communication of the complaint
# City: Customer city
# State: Customer state
# Zipcode: Customer zip
# Status: Status of complaint
# Filing on behalf of someone

# Analysis Task

# 1. Import data into R environment.
# 2. Provide the trend chart for the number of complaints at monthly and daily
# granularity levels.
# 3. Provide a table with the frequency of complaint types.

# 4. Which complaint types are maximum i.e., around internet, network issues, or
#    across any other domains.
#    4.1. Create a new categorical variable with value as Open and Closed.
#         Open & Pending is to be categorized as Open and Closed & Solved
#         is to be categorized as Closed.
#    4.2. Provide state wise status of complaints in a stacked bar chart.
#         Use the categorized variable from Q3. Provide insights on:

# 5. Which state has the maximum complaints
# 6. Which state has the highest percentage of unresolved complaints
#    6.1 Provide the percentage of complaints resolved till date, which were
#        received through the Internet and customer care calls.

# The analysis results to be provided with insights wherever applicable

library(rio)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyselect)
library(wordcloud)
library(plotrix)

# 1. Import data into R environment.

ComcastDF <- import("Comcast Telecom Complaints data.csv")
head(ComcastDF)
str(ComcastDF)

#Checking for NA / missing values
sum(is.na(ComcastDF))


# 2. Provide the trend chart for the number of complaints at monthly and daily
# granularity levels.

head(ComcastDF$Date)
ComcastDF$Date <- dmy(ComcastDF$Date)
head(ComcastDF$Date)
class(ComcastDF$Date)

ComcastDF$Month <- as.Date(cut(ComcastDF$Date, breaks = "month"))
ComcastDF$Day <- as.Date(cut(ComcastDF$Date, breaks = "day"))
ComcastDF["Count"] <- 1

ComcastDF1 <-
  aggregate(ComcastDF[c("Count")],
            by = list(Month = ComcastDF$Month),
            FUN = sum)

ComcastDF2 <-
  aggregate(ComcastDF[c("Count")],
            by = list(Day = ComcastDF$Day),
            FUN = sum)

ggplot(ComcastDF1,
       aes(x = Month, y = Count, label = Count)) +
  geom_point(size = 2) +
  geom_line(size = 1, color = "steelblue") +
  geom_text(vjust = -0.7, fontface = 2) +
  scale_x_continuous(breaks = ComcastDF1$Month,
                     labels = date_format("%Y-%b")) +
  labs(title = "Monthly No. of Complaints",
       x = "Months",
       y = "No. of Complaints") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ComcastDF2,
       aes(x = Day, y = Count, label = Count)) +
  geom_point(size = 2) +
  geom_line(size = 1, color = "steelblue") +
  geom_text(vjust = -0.7, fontface = 2) +
  labs(title = "Daily No. of Complaints",
       x = "Days (with labels per week)",
       y = "No. of Complaints") +
  theme(axis.text.x = element_text(angle = 75, size = 9),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d",
    date_minor_breaks = "1 day"
  )


# 3. Provide a table with the frequency of complaint types.
# 4. Which complaint types are maximum i.e., around internet, network issues, or
#    across any other domains.

wordcloud(ComcastDF$`Customer Complaint`, min.freq = 10, colors = T)

Freq <-
  data.frame(table(unlist(strsplit(
    tolower(ComcastDF$`Customer Complaint`), " "
  ))))

internet_complaints <-
  contains(ComcastDF$`Customer Complaint`,
           match = "internet",
           ignore.case = T)
network_complaints <-
  contains(ComcastDF$`Customer Complaint`,
           match = "network",
           ignore.case = T)
service_complaints <-
  contains(ComcastDF$`Customer Complaint`,
           match = "service",
           ignore.case = T)
billing_complaints <-
  contains(ComcastDF$`Customer Complaint`,
           match = "billing",
           ignore.case = T)
speed_complaints <-
  contains(ComcastDF$`Customer Complaint`,
           match = "speed",
           ignore.case = T)
price_complaints <-
  contains(ComcastDF$`Customer Complaint`,
           match = "pric",
           ignore.case = T)
data.cap_complaints <-
  contains(ComcastDF$`Customer Complaint`,
           match = "data cap",
           ignore.case = T)


ComcastDF$`Complaint Type`[internet_complaints] <- "Internet"
ComcastDF$`Complaint Type`[network_complaints] <- "Network"
ComcastDF$`Complaint Type`[service_complaints] <- "Service"
ComcastDF$`Complaint Type`[billing_complaints] <- "Billing"
ComcastDF$`Complaint Type`[speed_complaints] <- "Speed"
ComcastDF$`Complaint Type`[price_complaints] <- "Price"
ComcastDF$`Complaint Type`[data.cap_complaints] <- "Data cap"

ComcastDF$`Complaint Type`[-c(
  internet_complaints,
  network_complaints,
  service_complaints,
  billing_complaints,
  speed_complaints,
  price_complaints,
  data.cap_complaints
)] <- "Others"

sort.int(table(ComcastDF$`Complaint Type`), decreasing = T)


# 4.1. Create a new categorical variable with value as Open and Closed.
# Open & Pending is to be categorized as Open and Closed & Solved is to be
# categorized as Closed.

ComcastDF <-
  transform(ComcastDF, `Complaint Status` = 
              ifelse((Status == 'Open' |
                      Status == 'Pending'), "Open", "Closed"))
head(ComcastDF$Status, 10)
head(ComcastDF$Complaint.Status, 10)


# 4.2. Provide state wise status of complaints in a stacked bar chart.
#      Use the categorized variable from Q3. Provide insights on:
# 5. Which state has the maximum complaints

state_complaints <-
  data.frame(ComcastDF$State,
             ComcastDF$Complaint.Status,
             ComcastDF$Complaint.Type)

ggplot(state_complaints,
       aes(y = ComcastDF.State)) +
  geom_bar(aes(fill = ComcastDF.Complaint.Status)) +
  labs(title = "Status of Complaints per State",
       x = "No of complaints",
       y = "States",
       fill = "Status of Complaints") +
  scale_fill_manual(values=c("dark green", "red"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(state_complaints,
       aes(y = ComcastDF.State)) +
  geom_bar(aes(fill = ComcastDF.Complaint.Type)) +
  labs(title = "Status of Complaints per State",
       x = "No of complaints",
       y = "States",
       fill = "Types of Complaints") +
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5))

ComcastDF <- group_by(ComcastDF, State, Complaint.Status)

all_complaints <- summarise(ComcastDF, Count = n())
all_complaints[all_complaints$Count == max(all_complaints$Count), c(1, 3)]

open_status <-
  summarise(ComcastDF, Count = n()) %>% filter(Complaint.Status == "Open")
open_status[open_status$Count == max(open_status$Count), c(1, 3)]


# 6. Which state has the highest percentage of unresolved complaints

open_status$Percent <- round(prop.table(open_status$Count)*100,2)
open_status[open_status$Percent == max(open_status$Percent), c(1, 4)]


# 6.1 Provide the percentage of complaints resolved till date, which were
#     received through the Internet and customer care calls.

ComcastDF$Received.Via <- as.factor(ComcastDF$Received.Via)
levels(ComcastDF$Received.Via)

tab <- table(ComcastDF$Received.Via, ComcastDF$Complaint.Status)
round(prop.table(tab, 1)*100,2)