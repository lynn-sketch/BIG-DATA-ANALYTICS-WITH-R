setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")
#Loading libraries

library(tidyverse)
library(readxl)
library(ggplot2)
library(readr)
 
#Assignment (Trial practice Questions)
#Vectors 
#Creating variable names,marks
names <- c("lynn","Emma","Lorraine")

mark <- c(98,96,97)

view(names)

#Creating dataframes
df <- data.frame(names,mark)
view(df)

class(df)
class(names)
class(mark)

#Creating lists
list1 <- list("Lynn","Emma",90,80)
view(list1)
