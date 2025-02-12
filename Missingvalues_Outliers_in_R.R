library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)

setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")
df <- read_excel("Student_performance1.xlsx")
view(df)

##Dealing with missing values
library(data.table)
dt <- data.table(df)
class(dt)

##Checking for rows with missing values
dt[,.(sum(is.na((age))))]


#checking for rows without NA
dt[complete.cases(dt)]

##dropping missing values
##This method is not advisabe because we may remove necessary data
dt_clean <- na.omit(dt)
dt_clean

##Dealing with missing values in age
##Visualize to see if the data is normally demostrated

class(dt$age)
hist(dt$age, main = 'Histogram for age',col ='lightblue')

##Since the data is npot normally distributed we shall impute using median
dt[, age:=fifelse(is.na(age),median(age,na.rm = TRUE),age)]

##Checking for missing values in age
dt[,.(sum(is.na((age))))]
dt

##Impute using mode on column gender since its categorical
dt[,.(sum(is.na((gender))))]
dt[, gender:=fifelse(is.na(gender),mode(gender,na.rm=TRUE),gender)
dt[,.(sum(is.na((gender))))]

##Checking for missing values in DSC3108
dt[,.(sum(is.na((DSC3108))))]

##Dealing with missing values in DSC3108 column because its numerical using the  forward fill method
dt[, DSC3108:=nafill(DSC3108,type ="locf")]
##Checking for missing values in DSC3108
dt[,.(sum(is.na((DSC3108))))]

##Dealing with missing values in DSC3114 column because its numerical using the  forward fill method
dt[,.(sum(is.na((DSC3114))))]
dt[,DSC3114:=nafill(DSC3114,type = "nocb")]
dt[,.(sum(is.na((DSC3114))))]

##Dealing with missing values in MTH3108 column because its numerical using the fill method
dt[, MTH3108 := fifelse(is.na(MTH3108),0,MTH3108)]
dt[,.(sum(is.na((MTH3108))))]


dt[,.(sum(is.na((SYE3104))))]
dt[, SYE3104 := fifelse(is.na(SYE3104),0,SYE3104)]
dt[,.(sum(is.na((SYE3104))))]

dt[,.(sum(is.na((MTH3108))))]

##Handling duplicates
dt[duplicated(dt)]

##Handling outliers
ggplot(dt,aes(y=age))+
geom_boxplot(fill="lightblue")+
theme_minimal() + 
ggtitle("Boxplot of A")

##Dealing with outliers
Q1 = dt[, quantile(age,0.25)]
Q3 = dt[, quantile(age,0.75)]

##Calculating the IQR
IQR = Q3 - Q1

##setting the bounds
lower_bound <- Q1 - 1.5*IQR
upper_bound <- Q3 + 1.5*IQR

##Removing the outliers in Age
dt2 <- dt[age >= lower_bound & age <= upper_bound]
boxplot(dt2$age, main = "Boxplot of age", col = "lightblue")

##Removing outliers in DSC3108
boxplot(dt$DSC3108, main = "Boxplot of age", col = "lightblue")
dt3 <- dt[DSC3108 >= lower_bound & DSC3108<= upper_bound]
boxplot(dt3$DSC3108, main = "Boxplot of age", col = "lightblue")

##Removing outliers in DSC3114
boxplot(dt$DSC3114, main = "Boxplot of age", col = "lightblue")
dt4 <- dt[ DSC3114>= lower_bound & DSC3114<= upper_bound]
boxplot(dt4$DSC3114, main = "Boxplot of age", col = "lightblue")

##Removing outliers in MTH3108 ##This particular code didnt work out
boxplot(dt$MTH3108, main = "Boxplot of age", col = "lightblue")
dt5 <- dt[MTH3108>= lower_bound & MTH3108<= upper_bound]
boxplot(dt5$MTH3108, main = "Boxplot of age", col = "lightblue")

boxplot(dt$SYE3104, main = "Boxplot of age", col = "lightblue")
dt6 <- dt[SYE3104>= lower_bound & SYE3104<= upper_bound]
boxplot(dt5$SYE3104, main = "Boxplot of age", col = "lightblue")

boxplot(dt$SYE3104, main = "Boxplot of age", col = "lightblue")
