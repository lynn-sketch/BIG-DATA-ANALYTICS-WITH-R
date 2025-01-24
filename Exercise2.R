#Dealing with missing values
library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)

#Getting the dataset
df <- read_excel("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/Randomized_Student_Dataset.xlsx")
view(df)

#Finding out how many missing values the dataset has
df%>%
  is.na() %>%
  sum() %>% 
  

#Showing the rows with missing values
df %>%
  select(Access_no,gender,age,course,DSC3108,DSC3114,MTH3108,SYE3104)%>%
  filter(!complete.cases(.))

#Handling  missing categorical data by imputing mode
df %>% 
  select(gender) %>% 
  table() %>% 
  view()

#Replacing the missing values with male or female
df $gender[is.na(df$gender)] <- replace_na("female")

df %>% 
  select(gender) %>% 
  is.na() %>% 
  sum()

#Dealing with missing values in numerical values
#we check if the data is normally distributed if so we impute with mean
#If it is not normally distributed we use median
#We are dealing with each variable singly
#We are dealing with age
#Construct a boxplot

hist(df$age, main="VARIABLE AGE")
df$age[(is.na(df$age))] <-median(df$age,na.rm = TRUE)


df %>% 
  select(DSC3108) %>% 
  is.na() %>% 
  sum()

#Use a histogram to show if variable is normally distributed
hist(df$DSC3108,main = "VARIABLE COURSE_ID")

#Since its not normally distributed we are going to use median to replace the missing values
df$DSC3108[(is.na(df$DSC3108))] <-median(df$DSC3108, na.rm = TRUE)

hist(df$DSC3114,main ="VARIABLE Course_id2")
df$DSC3114[(is.na(df$DSC3114))] <- median(df$DSC3114,na.rm = TRUE)

df %>% 
  select(DSC3114) %>% 
  is.na() %>% 
  sum()

hist(df$MTH3108,main = "VARIABLE COURSE_ID")
boxplot(df$MTH3108)  
df$MTH3108[(is.na(df$MTH3108))] <- mean(df$MTH3108,na.rm = TRUE)

df %>% 
  select(MTH3108) %>% 
  is.na() %>% 
  sum()

hist(df$SYE3104,main = "VARIABLE COURSE_ID")
boxplot(df$SYE3104)  
df$SYE3104[(is.na(df$SYE3104))] <- mean(df$SYE3104,na.rm = TRUE)

df %>% 
  select(SYE3104) %>% 
  is.na() %>% 
  sum()

df %>% 
  is.na() %>% 
  sum()

