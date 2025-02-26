library(readxl)
library(dplyr)
library(tidyverse)ANALYTICS
library(data.table)
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/Bigdataanalytics Notes For Sem 2 ")

##calling the dataset
df <- read_excel("Bike_Sales_New (1).xlsx")
view(df)

##Changing it to data.table
df = as.data.table(df)
df

##caling a specific column in data.table
df$Country

##Getting a row of a data table
df[1,] ##This only returns one row

df[1:5,]##This only returns rows 1 to 5

##In data.table we cant  call a column using an index this only works in data frames
##therefore we call columns using their names and they appear after the comma
##returning specific columns in datatable
df[,Month]

##Creating a datatable using specific columns
df[,list(Product,Customer_Gender)]
df[,list(Age_Group,Product)]

##selecting rows based on specific columns
df[Customer_Age > 30 & Product ==  "Mountain Tire Tube",]
view(df)

##sort the customers by product in order
df[order(Product),]
df.filter = df[Product=="Mountain Tire Tube" & Customer_Age > 30]
df.filter.sort = df.filter[order(Country)]
df.filter.sort
