library(tidyr)
library(tidyselect)
library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(data.table)

setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")
df <-read.csv("escs_trend.csv")
df2 <- fread("escs_trend.csv",na.strings = '')
dim(df2)
##using data.table
##Checking for missing values in a specific column
df2[is.na(studentid)]

df[, "studentid" := fifelse(is.na("studentid"), mean("studentid", na.rm = TRUE), "studentid")]

##Plot
boxplot(df2$escs_trend,main='distribution1')
view(df)
summary(df)

sum(is.na(df))

str(df)
boxplot(df$escs_trend)

##Dealing with outliers
summary(df)
q1 = -1.14
q3 = 0.49
IQR = q3-q1
IQR

Tmin <- q1 - 1.5*IQR
Tmax <- q3 + 1.5*IQR

df$escs_trend = df$escs_trend[which(df$escs_trend >= Tmin | df$escs_trend <= Tmax)]
boxplot(df$escs_trend)
