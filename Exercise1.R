
#Setting the working directorate
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/PracticeExercises for R")
#importing the necessary libraries
library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)

#Importing the dataset
df <- read_excel("Student_Performance.xlsx")
view(df)


#Creating my own data
names <- c("Lynn","Emma", "Anita", "Hellen","Ronald","Irwin")
weight <- c(45,55,46,60,79,59)

#Creating a dataframe called class weight
class_weight = data.frame(names,weight)

#Carrying out summary statistics
summary(df)
summary(class_weight)
summary(df)
#The mean is 45.00
#The firstQU is 48.25
#The median is 57.00
#The 3rd QU is 59.75
#The max is 79.00

#Mean of the Dataset
mean(class_weight) #An error because this is a dataset which has qualitative data
mean(class_weight$weight)  #Gives me the mean since its a dataset and a quantitavie variable

#Creating the box plot
boxplot(df $DSC3108, main="Student Marks", boxwex=0.5)

#Display the variable Age visually using a histogram
hist(df $Age, main = "Student Age using a histogram")

sum(is.na(df))

#Viewing the structure of the dataset
str(df)
glimpse(df)

#Showing the names of the variables or columns
names(df)


