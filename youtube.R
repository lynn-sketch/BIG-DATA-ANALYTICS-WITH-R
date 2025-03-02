##INTRODUCTION TO R
##Hashtags are seen as comments

##importing necessary libraries to read the data
library(readxl)
library(tidyverse)

##you have to set the workig directorate
setwd("C:/Users/HP PC/Desktop/readrafresh")

##Reading the data
Data = read_excel("Bike_Sales.xlsx")
##viewing the data
view(Data)

##Generating my own data
name = c("Lynn","emma","liz")
weight = c(56,78,54)

##combining to form a dataframe
classweight = data.frame(name,weight)
view(classweight)


##summary statistics or central tendency
summary(classweight)

##since the mean is 62.67 and median is 56 the data is not normally distributed 
##the mean and median have to be equal for it to be normally distributed

mean(classweight)
##this gives an error because its not numeric so we specify the column which is numeric
mean(classweight$weight)

##visualizing the data
##we use boxplots and histogram to see tendral tendency and distribution
boxplot(classweight$weight, main= "central tendency of weight",boxwex=0.3)


hist(Data$Customer_Age, main= "customerage buying bicycles",boxwex =0.1)


##summary for bicycle dataset in central tendency  
summary(Data$Customer_Age)

##functions
DataIn <- seq(from = 0,to= 100, by= 10) 
DataIn %>% 
  mean()

##dealing with group by functions
view(Data)
##filtering the data
Data1 <- Data[Data$Year== 2017,]

##grouping by the data
summarise(Data1)

##piping by filtering out a specific year,grouping by age then we get the mean
Data %>% 
  filter(Year == 2017) %>% 
  group_by(Customer_Age) %>% 
  summarize(mean=mean(Revenue))

##Incasee we get untidy data we use gather and separate to make it tidy
##eg
##data %>% 
   ##gather(gender_year,income,male_2016:female_2018) %>% 
   ##separate(gender_year,into=c("gender","year"))
