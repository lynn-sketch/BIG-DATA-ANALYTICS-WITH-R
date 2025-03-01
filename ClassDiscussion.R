library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(tidyselect)
library(data.table)
library(dplyr)
library(tidyselect)

setwd("C:/Users/HP PC/Desktop/readrafresh")
df <- read_excel("Vegetable_Sales-1.xlsx")
view(df)

##Checking number of rows and columns
dim(df)

##structure of dataset
str(df)
glimpse(df)

##Analyzing a specific column
df$`Unit Selling Price ($/kg)`

##Carrying out summary statistics
summary(df$`Wholesale Price ($/kg)`)

summary(df)

view(summary(df))

##Crating a table
view(table(df$`Item Name`))

##Creating a table of people given a discount and not
DiscountColumn <-view(table(df$`Discount (Yes/No)`))
