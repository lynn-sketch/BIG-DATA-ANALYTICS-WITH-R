
##AMOIT LYNN
##M23B38/007
##ACCESS NUMBER B20813

install.packages("writexl")
library(tidyverse)
library(tidyselect)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)
library(writexl)

setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/PracticeExercises for R")
WorldBank <- read_csv("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/ida_credits_to_uganda_02-11-2025.csv")
view(WorldBank)
WorldBank2 <- fread("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/ida_credits_to_uganda_02-11-2025.csv",na.strings = '')

##1.How many loans in IDA are fully repaid
WorldBank2 %>% 
  select(`Credit Status`) %>% 
  filter(`Credit Status` %in% 'Fully Repaid') %>% 
  count()
##According to the above code 96 loans of IDA are fully repaid

##2.Subsetting the first 10 columns and save it as a dataframe
FirstTenColumns <-WorldBank2[,0:10]
view(FirstTenColumns)

##3.median "Original Principal Amount (US$)" asked for each "Project Name"
world_data_median <-WorldBank2 %>%
  select(`Original Principal Amount (US$)`,`Project Name`) %>% 
  group_by(`Project Name`) %>% 
  summarize(median_amount= median(`Original Principal Amount (US$)`, na.rm = TRUE))

# Write to Excel file
write_xlsx(world_data_median,"Median_amount.xlsx")
