library(tidyverse)
library(readxl)
library(readr)
library(writexl)
library(data.table)

setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")
##Reading the data

df <- read.csv("escs_trend.csv")
view(df)

##Reading it to data.table
dt <- fread("escs_trend.csv",na.strings ="")##Why isnt this line of code running
df = as.data.table(df)
class(df)

##We are going to subset since the dataset is extremely big
cycle5 = subset(df, cycle%in% ("5"))
studentid1 = subset(df,studentid %in% ("1"))
studentid1

cycle8 <- df[cycle == "5",]
cycle8 %>% 
  view()

##Trying t save the subsetted data as a csv or excel
fwrite(cycle5, file = "cycle5.csv")

##Dealing with the subsetted data
##Reading it as data.table
cycle5 = fread("cycle5.csv",na.strings = "")
view(cycle5)

#Then select only data from country which is called ARE
cycle5[cnt =="ARE",]

##Choosing specific row
cycle5[3:100]

##Choosing specif rows and a given range
view(cycle5)
result3 = cycle5[cnt== "ARE" & paredint_trend == 16][1:10,studentid:paredint_trend]
result3
