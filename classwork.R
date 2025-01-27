proc.time()
system.time()

?proc.time
?system.time()

setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")

start_time = proc.time()
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)

exercise_1 = read.csv("escs_trend.csv")
exercise_1
View(exercise_1)
summary(exercise_1)
end_time = proc.time()
end_time - start_time

system.time({read.csv("escs_trend.csv")})
system.time({fread("escs_trend.csv", na.strings = "")})

