library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)

setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/PracticeExercises for R")
WorldBank <- read_csv("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/ida_credits_to_uganda_02-11-2025.csv")
view(WorldBank)
WorldBank2 <- fread("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/ida_credits_to_uganda_02-11-2025.csv",na.strings = '')

##How many loans in IDA are fully repaid
WorldBank2 %>% 
  select(`Credit Status`) %>% 
  filter(`Credit Status` %in% 'Fully Repaid') %>% 
  count()

##Subsetting the first 10 columns and save it as a dataframe
FirstTenColumns <-WorldBank2[,0:10]
view(FirstTenColumns)

##median "Original Principal Amount (US$)" asked for each "Project Name"
WorldBank2 %>%
  select(`Original Principal Amount (US$)`,`Project Name`) %>% 
  group_by(`Project Name`) %>% 
  summarize(median_amount= median(`Original Principal Amount (US$)`, na.rm = TRUE)) %>% 
  view()
