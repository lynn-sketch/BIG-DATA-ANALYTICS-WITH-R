##importing libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(tidyselect)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(caTools)
library(caret)
library(Boruta)
library(cvms)
library(MASS)
library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(caret)
library(ModelMetrics)
library(lmtest)
library(car)
library(olsrr)
library(moments)
library(bestNormalize)
library(magrittr)
library(ggcorrplot)

##SUPERVISED LEARNING: CLASSIFICATION#####
##Suited to a target variable that is categorical
##Various Classification methods including Decision Trees, Random Forest, Support Vector Machines(SVM) etc
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")

data <- read_excel("Bike_Sales_New (1).xlsx")
view(data)

##Splitting the data in training dataset and testing set
set.seed(20)

train = sample(1:nrow(data),nrow(data)*0.7)
view(train)

##viewing the trained dataset
train_data = data[train,]
view(train_data)
str(train_data)
##It has 48,584 rows and 18 columns

##Creating the testing dataset
test <- data[-train,]
str(test)
##20,823 × 18 from the test dataset
view(test)

# Build the decision tree model based on the target variable "Play Golf"
train_data$Age_Group <- as.factor(train_data$Age_Group)
data_tree <- rpart(Age_Group ~ ., data = train_data, method = "class")
sum(is.na(data$Age_Group))
str(train_data$Age_Group)

##checking levels of data
levels(train_data$Age_Group)

##plotting the decision tree
rpart.plot(data_tree, main = "Decision Tree for data")
rpart.plot(data_tree)

##SIMPLE LINEAR REGRESSION
####SUPERVISED LEARNING: REGRESSION ####
#This model assumes a linear relationship between with a target continuous variable.
#Two types exist:
#1. Simple linear regression; when we have only one input variable
#2. Multiple linear regression; when there are multiple input variables

names(data)
##making my categorical data into factors since classification deals with factors
data$Date = as.factor(data$Date)
data$Month = as.factor(data$Month)
data$Age_Group = as.factor(data$Age_Group)
data$Customer_Gender = as.factor(data$Customer_Gender)
data$Country = as.factor(data$Country)
data$State = as.factor(data$State)
data$Sub_Category = as.factor(data$Sub_Category)
data$Product_Category = as.factor(data$Product_Category)
data$Product = as.factor(data$Product)

##subsetting the data into categorical and continuous
categorical = data[c("Date", "Month", "Age_Group", "Customer_Gender", "Sub_Category", "Product", "Product_Category", "Country", "State")]
numerical = data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", "Unit_Price", "Profit", "Cost")]
view(categorical)
view(numerical)
view(data)

##Checking relationships between categorical variable and the target revenue
##we used anova test to see if there is any difference in mean of revenue across diferent dates
one.way <-aov(Revenue ~ Date, data= data)

##summarizing the anova test
summary(one.way)

##The ANOVA test only tells you if there is a significant difference somewhere between the groups, but it does not tell you which specific groups are different.

##The Tukey HSD test helps you compare each pair of groups to determine exactly which pairs are different.
TukeyHSD(one.way)
##the results show the difference of mean in each date

#Relationship between Age_Group and Revenue
one.way <- aov(Revenue ~ Age_Group, data = data)
summary(one.way)
TukeyHSD(one.way)

##creating a function to carry out anova test at once
oneway_anova <- function(data, response, factor) {
  # Run ANOVA
  model <- aov(response ~ factor, data = data)
  
  # Print ANOVA summary
  print(summary(model))
  
  # Run and print Tukey HSD
  print(TukeyHSD(model))
}

# Example usage:
oneway_anova(data,"Customer_Gender","Country"," State","Product_Category","Product","Sub_Category" )


