setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")
library(tidyverse)
library(tidyselect)
library(readr)
library(readxl)


df <- read_excel("Student_perfomance.xlsx")
view(df)
dim(df)
head(df)
tail(df)
glimpse(df)
names(df)
 