#Creating working directorate
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")

#importing libraries
library(tidyverse)
library(tidyselect)
library(readxl)
library(ggplot2)
library(readr)

#Reading files in R 

df <- read.csv("mydatasetsforpractice/data_02.csv",col.names = False, col.names = c("FName","Lname","Language","rate"))
view(df)

df2 <- read.csv("mydatasetsforpractice/data_03.csv",sep = "|")
view(df2)

df3 <- read.csv("mydatasetsforpractice/data_04.csv",sep = "\t",skip = 1, comment.char = "#")
view(df3)
dim(df3)
head(df3)
tail(df3)

glimpse(df3)

data()
starwars
view(starwars)
