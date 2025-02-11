library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)
install.packages("forecast")

##Importing the dataset and setting the working directorate
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/PracticeExercises for R")
Data <- read.csv("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/ida_credits_to_uganda_02-11-2025.csv")
view(Data)

##Question One 
##Run a time-series analysis on that explains the trend of World Bank fund disbursement to Uganda (hint: use the variable "Disbursed Amount (US$)
##Changing it to data.table
df <- data.table(Data)

## Changing the last date variable to Date
df$Last.Disbursement.Date = as.Date(df$Last.Disbursement.Date)

# Extract the year from 'End of Period'
df[, Year := year(Last.Disbursement.Date)]

# Aggregate the total disbursed amount by year
disbursement <- df %>% 
  group_by(Year) %>% 
  summarise(total_disbursement = sum(Disbursed.Amount..US..),na.rm = TRUE)

# Plot the trend of disbursements using ggplot2
ggplot(disbursement, aes(x = Year, y = total_disbursement)) +
  geom_line(color="red") +
  geom_point(color="blue") +
  labs(title = "Trend of World Bank Fund Disbursement to Uganda",
       x = "Year", y = "Total Disbursed Amount (US$)") +
  theme_minimal()

##Question two
##Describe the overall "Credit Status" of Uganda based on the provided dataset
##Checking distribution of Credit status using a bar graph
df %>% 
  select(Credit.Status) %>% 
  ggplot(aes(Credit.Status)) +geom_bar()

##Checking for distribution of credit status using a table
df %>% 
  select(Credit.Status) %>% 
  table() %>% 
  sort() %>% 
  view()
#Uganda  has 96 fully repaid credit status with it being the highest frequency
##Therefore its credit status is good based on the visual analysis and table format

##Question Three
##Explain the "Original principal amount" Uganda borrowed from the World Bank, what patterns can you deduce from it
##Plotting a histogram to visualize the  trend of the Original Principal amount Uganda borrowed
df %>% 
  ggplot(aes(Original.Principal.Amount..US..)) %>% 
  +geom_histogram()+
  labs(title= 'Visualizing the trend of the original Principle amount')

##According to the above visualization,the amount originally borrowed was low and 
##the amount increased and gradually reduced and has never come up




