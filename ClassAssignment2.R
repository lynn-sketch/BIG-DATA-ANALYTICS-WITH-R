library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)

setwd("C:\Users\HP PC\Downloads\R_practical_Exercises (1)")
df <- read_excel("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/PracticeExercises for R/Bike_Sales.xlsx")
df

###Relationship between two continuous variables
view(df)

###Customer age and day
##Plotting continuous using a line graph
df %>% 
  ggplot(aes(x= Day, y=Year)) + geom_line()

#Plotting categorical and continuous
df %>% 
  ggplot(aes(x= Age_Group, y=Day)) + geom_boxplot()
  
