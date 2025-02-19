##SETTING WORKING DIRECTORATE
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")


#Preprocessing ----

##Loading libraries----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(GGally)

##Loading data
data <- read.csv("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/class Discussion/class Discussion/Diamond dataset/diamonds.csv")
head(data) ##A review of first  6 columns              
view(data)


#Structure of data
str(data)

#Summarize stats
summary(data)

##Handling missing values----
##Approach 1

missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values

##Approach 2
missing_values_two <- data %>% summarise(numeric_missing = sum(is.na(.)),
                                         categoric_missing = sum(is.na(as.character(.))))
missing_values_two

##Approach three
missing_values_three <- data %>%  summarise_all(funs(sum(is.na(.))))
missing_values_three

##Removing Missing values----

##Approach one
##Dropping methods

new_data = na.omit(data)


##Imputing data
class(data$color)
class(data$depth)

hist(data$depth,col = "blue", main ="Histogram for depth")

##Impute for missing values
impute_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)) ##Normalize numerical data
impute_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE)) ##Skewed numerical data
impute_mode <- function(x){
  modal_value <- as.numeric(names(table(sort(x), decreasing = TRUE[1]))) ##Categorical data
  replace(x, is.na(x),modal_value)
}

##Calling the function to remove missing values
impute_mean(data$depth)
impute_mode(data$color)

##Outliers
##Incosistencies in the data
##Visualizing outliers

numerical_data <- data %>%  select_if(is.numeric)
view(numerical_data)

##Plotting boxplots to show 













###Exploring the data(EDA)

view(df)

##size of dataset
dim(df)

##head of dataset
hd <- head(df,3)
view(hd)

#Piping in R
df %>% 
  head() %>% 
  view()
glimpse(df)

##getting a column of variables
table(df$color)
view(table(df$color))

#Option 2
df %>% 
  select(color) %>% 
  is.na() %>% 
  sum()
duplicated(df)

data()
starwars
