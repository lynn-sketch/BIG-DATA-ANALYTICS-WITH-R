##SETTING WORKING DIRECTORATE
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")

##Calling the libraries to be used
library(tidyverse)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyselect)

df <- read.csv("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS/class Discussion/class Discussion/Diamond dataset/diamonds.csv")
###Exploring the data(EDA)

view(df)

##size of dataset
dim(df)

##head of dataset
hd <- head(df,3)
view(hd)

#Piping in R
df %>% 
  head() %>% 
  view()
glimpse(df)

##getting a column of variables
table(df$color)
view(table(df$color))

#Option 2
df %>% 
  select(color) %>% 
  is.na() %>% 
  sum()
duplicated(df)

data()
starwars
