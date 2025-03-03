##Data.table discussion
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)

library(tidyselect)
library(data.table)
library(dplyr)
library(tidyselect)

setwd("C:/Users/HP PC/Desktop/readrafresh")
##excel files cant be read in data.table but csvs can be read 
##df < read_excel()
##then convert it to data.table
##df <- as.data.table(df)
##you can read a csv in data.table
##df <-fread("",na.string= "")

df<- read_excel("Bike_Sales.xlsx")
df <-as.data.table(df)
class(df)

df2 <- fread("Salaries.csv",na.strings = "")
df2

view(df2)
##selection in data.table
##sele4ction first row in our dataset
df2[1,]

##selecting 
df2[1:10,lgID:salary]

df2[1:10,list(lgID,salary)]

##selecting years greater than 2013
df2[yearID > 2000,][head(yearID,5)]

##Filtering in data.table
##Filters salaries from a specific year eg 2010
df2[yearID==2010,]
df2[lgID=="AL",]

##SELECT WHERE YEARID IS 1990 AND LGID IS AL
df2[yearID==1990 & lgID== "AL",]
df2[yearID==2010 | lgID== "AL",]
df2[teamID=="BAL" & lgID== "AL",][1:4]

##get players earning the most money
df2[order(yearID,salary,decreasing=TRUE)]

##summary statistics
summary(df2$salary)

df2$salary %>% 
  mean()

df2[,mean(salary)]

##avaerage salary for the entire dataset
df2[,table(lgID)]
df2[,sum(salary)]
df2[,max(salary)]

##find the total of salary in lgIDAL
df2[,sum(salary),by="lgID"]
df2[,.(total = sum(salary)), by="lgID"]

##summary statistics based on the team ID
df2[,.(Average=mean(salary),Median= median(salary)),by="teamID"]
df2[,.(Count=.N),by="teamID"]

##Handling missing values
sum(is.na(df))

df3 <- fread("Diabetes Missing Data (1).csv",na.strings = "")
view(df3)
##handling missing values

##Returns a specif column and its rows
df3[is.na(Glucose)]

##Returns columns without missing values
df3[complete.cases(df3)]

##handling missing values in data.table
##removes or deletes all rows with missing values
colSums(is.na(df3))
df4 = na.omit(df3)

##fill in missing values with zeros
df3[,Serum_Insulin:=fifelse(is.na(Serum_Insulin),0,Serum_Insulin)]
colSums(is.na(df3))

##filling in missing values with the mean and median
hist(df3$Glucose)
df3[,Glucose:=fifelse(is.na(Glucose),median(Glucose,na.rm=TRUE),Glucose)]
colSums(is.na(df3))

hist(df3$Diastolic_BP)
df3[,Diastolic_BP:=fifelse(is.na(Diastolic_BP),mean(Diastolic_BP,na.rm=TRUE),Diastolic_BP)]
colSums(is.na(df3))

##Filling in misssing values using backward fill & forward fill
##forwardfill
hist(df3$BMI)
df3[,BMI:= nafill(BMI,type="locf")]
colSums(is.na(df3))

##backwardfill
hist(df3$Skin_Fold)
df3[,Skin_Fold:=nafill(Skin_Fold,type="nocb")]
colSums(is.na(df3))

##Check for duplicates
df3[duplicated(df3)]

##to remove duplicates we run this code
df3 <- unique(df3)

##Handling outliers using data.table
##picking out numerical columns
numerical_data <- df3 %>% 
  select_if(is.numeric)

view(numerical_data)

##visualizing to check for outliers
boxplot(df3$Pregnant)
boxplot(df3$Glucose)
boxplot(df3$Diastolic_BP)
boxplot(df3$Serum_Insulin)
boxplot(df3$BMI)
boxplot(df3$Class)
boxplot(df3$Diabetes_Pedigree)

##summary statistics
q1 = df3[,quantile(Age,0.25)]
q3 = df3[,quantile(Age,0.75)]
q1
q3

##getting IQR
IQR = q3 -q1

##Getting the bounds
lower_bound = q1 - 1.3*IQR
upper_bound = q3 + 1.3*IQR

lower_bound
upper_bound
outlier_removal = df3[,Age < lower_bound | Age > upper_bound]
Remove_outliers = df3[,Age >= lower_bound & Age <= upper_bound]
boxplot(df3$Glucose)

