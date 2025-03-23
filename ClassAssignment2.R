library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)

setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/PracticeExercises for R")
BikesDataset <- read_excel("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/PracticeExercises for R/Bike_Sales.xlsx")
BikesDataset

#### Visualizations ######
## 1. continuous variables
#Using a scatter plot
BikesDataset %>%
  ggplot(aes(y=Cost,x=Revenue)) + geom_point(size=2,alpha = 0.5,color = "green") +
  geom_smooth(method = lm,color="red") + #theme_bw("dark6") +
  facet_wrap(~Customer_Gender) +
  labs(x="Cost",
       y="Revenue",
       title = "Relationship between Revenue and Cost By Gender")

#unit cost and unit price
BikesDataset %>%
  ggplot(aes(x=Unit_Price,y=Unit_Cost)) + geom_point(color="blue",shape="*") +
  geom_smooth(method = lm,color="red") +
  theme_bw() +
  labs(x="Unit Cost",
       y="Unit Price",
       title = "Unit Price by Unit cost")

## 2. Continuous and categorical variable

#Using a boxplot
BikesDataset %>%
  ggplot(aes(Age_Group,Revenue)) + geom_boxplot() +
  theme_bw() +
  labs(x="Age_Group",
       y="Revenue",
       title = "Revenue by Age Group based on Customer Gender")

# Revenue by Gender
BikesDataset %>%
  ggplot(aes(Customer_Gender,Revenue)) + geom_boxplot() +
  theme_bw() +
  labs(x="Customer Gender",
       y="Revenue",
       title = "Revenue By Customer Gender")

## 3. Categorical and categorical
##Using a countplot
BikesDataset %>%
  ggplot(aes(Age_Group,Month)) +
  geom_count() + theme_bw() +
  labs(x="Age Group",
       y="Month",
       title = "Country of Month By Age Group")

## Single categorical
BikesDataset %>%
  ggplot(aes(y=Month,fill=Month)) + geom_bar() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x="Count",
       y="Month",
       title = "A Barchat of Month") +
  theme_bw()





