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
