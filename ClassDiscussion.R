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
DiscountColumn <-(table(df$`Discount (Yes/No)`))
view(DiscountColumn)

##Creating a function called totalprice
df2 <- mutate(
  df,Total_price = df$`Unit Selling Price ($/kg)` * df$`Quantity Sold (kg)`
)
view(df2)

##Creating a table for specific columns based on total price
df %>% 
  select(`Unit Selling Price ($/kg)`, `Quantity Sold (kg)`, Total_price) %>% 
  view()

##Best selling vegetable based on price
Topselling <- df2[which.max(df2$Total_price),c( "Item Name","Quantity Sold (kg)","Sale or Return","Wholesale Price ($/kg)","Total_price","Unit Selling Price ($/kg)")]

##number of minimum
which.min(df2$Total_price)
LeastSellingPrice <- df2[which.min(df2$Total_price),c( "Item Name","Quantity Sold (kg)","Sale or Return","Wholesale Price ($/kg)","Total_price","Unit Selling Price ($/kg)")]
view(LeastSellingPrice)

view(df)

##Filtering items with discount
df3 <-filter(df,df$`Discount (Yes/No)`=="yes")
view(df3)

##hOw many goods didnt have discount
df4 <- filter(df,df$`Discount (Yes/No)`=="No")
view(df4)

##How many goods were sold and how many were returned
sale_returnTable <- table(df$`Sale or Return`)
view(sale_returnTable)

##Find the total revenue made in the sales
sold = filter(df,df$`Sale or Return`=="sale")
Total_revenue = sum(sold$Total_price)
view(Total_revenue)

##Finding the total loss
returned = filter(df,df$`Sale or Return`=="return")
Total_revenue_in_returns = sum(returned$Total_price)
view(Total_revenue_in_returns)

DifferencebtnRevenueAndLoss = Total_revenue - Total_revenue_in_returns
view(DifferencebtnRevenueAndLoss)  

##Dealing with missing values
diabet <- read_csv("Diabetes Missing Data.csv")
view(diabet)

##Checking for missing values
sum(is.na(diabet))

diabet %>% 
  filter(!complete.cases(.)) %>% 
  view()

##Plottting the data to see its distribution
names(daibet)
diabet %>% 
  ggplot(x="Serum_Insulin")
  +geom_bar()

##Creating a function for visualizing

missing <- function(diabet){
  for (col in c("Pregnant","Serum_Insulin","Class","Glucose","BMI","Diastolic_BP","Diabetes_Pedigree","Skin_Fold","Age")){
    if(is.numeric(diabet[[col]])){
      hist(diabet[[col]], main = col)
    }
  }
}

change = function(diabet){
  for (col in c("Pregnant","Serum_Insulin","Class","Glucose","BMI","Diastolic_BP","Diabetes_Pedigree","Skin_Fold","Age")){
    diabet[[col]] <- as.numeric(diabet[col])
    
  }
     
}

change(diabet)

##Plotting to see the distribution so that we can impute
hist(diabet$Skin_Fold)
hist(diabet$Serum_Insulin)
hist(diabet$Glucose)

##impute missing values
diabet$Skin_Fold[is.na(diabet$Skin_Fold)] <- median(diabet$Skin_Fold,na.rm = TRUE)
sum(is.na(diabet$Skin_Fold))

diabet$Serum_Insulin[is.na(diabet$Serum_Insulin)] <- median(diabet$Serum_Insulin,na.rm =TRUE)
sum(is.na(diabet$Serum_Insulin))

sum(is.na(diabet$Glucose))
diabet$Glucose[is.na(diabet$Glucose)] <- median(diabet$Glucose,na.rm =TRUE)

sum(is.na(diabet$Diastolic_BP))
diabet$Diastolic_BP[is.na(diabet$Diastolic_BP)] <- median(diabet$Diastolic_BP,na.rm =TRUE)

sum(is.na(diabet$BMI))
diabet$Diastolic_BP[is.na(diabet$Diastolic_BP)] <- median(diabet$Diastolic_BP,na.rm =TRUE)
