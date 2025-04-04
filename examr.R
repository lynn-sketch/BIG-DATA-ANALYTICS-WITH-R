3*2

##importing libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(tidyselect)
library(ggplot2)

##setting the working directorate----
setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")

##Reading the data----
data <- read_excel("Bike_Sales_New (1).xlsx")
view(data)

##creating your own dataframe
Name = c("Lynn","Amoit","Jacky","Liz")
weight = c(54,56,34,101)

classweight = data.frame(Name,weight)
view(classweight$weight)

##calculating the summary statistics
summary(classweight)

mean(classweight$weight)

##visualizing----
hist(classweight$weight,main="weight of people")
boxplot(classweight$weight,main = "weight boxplot")

##WORK 2
##This just shows what type of data types are in my columns
str(data)

##This line of code below shows the names of my columns
names(data)

##Renaming columns names in r

data_new = names(data)[2] = "Month"
names(data)

data_new = names(data)[4] = "Year"

names(data)[names(data) == "Customer_Age"] = "Age_for_customer"
names(data)

names(data)[names(data)== "Daty"] = "Date"
names(data)

##getting summary statistics for each column
view(summary(data))

##Checking for missing values
sum(is.na(data))

##checking for missing values in a particular column
sum(is.null(data$Age_for_customer))

##Creating a function for mode in r since r doesnt generate mode
getmode = function(v){
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

##we then use the function to calculate mode
v = data$Country
mode_country = getmode(v)
mode_country

##if you have more than 30% missing data we ommit it 
data_omit = na.omit(data)
view(data_omit)

##checking for columns with missing data
listMissingColumns = colnames(data)[apply(data,2,anyNA)]
listMissingColumns

##showimg missing data for each column 
colSums(is.na(data))

##Replacing missing values with zero
data_zero = replace(data,is.na(data),0)
hist(data$Cost)

##if its not normally distributed we use median and mean visav
data_median = data %>% mutate(across(where(is.numeric),replace_na(.,  median(na.rm=TRUE)))))

##dealing with outliers
outliers_customer <- boxplot.stats(data$Age_for_customer)
outliers_customer         

boxplot(data$Age_for_customer, main="customerage boxplot",boxwex =0.9)


##Getting a dataset with missing values
df <- read_excel("Student_performance1.xlsx")
df
sum(is.na(df))
colSums(is.na(df))

##Dealing with missing values
plot_histograms <- function(x) {
  par(mfrow = c(2,2))
  
  for (col in colnames(x)) {
    hist(x[[col]], main = paste("Histogram of", col), col = "lightblue", xlab = col)
  }
}

plot_histograms(df_numeric)

##since its not normally distributed we shall impute missing data with median
sum(is.na(df$age))

data_median_age = df %>%mutate(across(where(is.numeric),~replace_na(., median(., na.rm=TRUE))))

df$age = df %>%mutate(across(age, ~replace_na(., median(., na.rm=TRUE))))
colSums(is.na(df))

df$DSC3108 <- ifelse(is.na(df$DSC3108), median(df$DSC3108, na.rm = TRUE), df$DSC3108)
colSums(is.na(df))

df$MTH3108 <- ifelse(is.na(df$MTH3108), median(df$MTH3108,na.rm = TRUE),df$MTH3108)
colSums(is.na(df))

df <- df %>% mutate(across(where(is.numeric),~replace_na(., median(., na.rm = TRUE))))
colSums(is.na(df))  

sum(is.na(df$gender))
colSums(is.na(df))
 
##Dealing with categorical missing values
##creating a mode function since r has no mode function
##Creating a function for mode in r since r doesnt generate mode
get_mode = function(v){
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

df <- df %>% mutate(across(where(is.character), ~replace_na(., getmode(.))))
sum(is.na(df$gender))




##Dealing with outliers
##creating a function that plots boxplots at ago
df_numeric <- df[, sapply(df,is.numeric)]

boxplots <- function(x){
  ##selecting numeric columns
  par(mfrow = c(1,1))
  boxplot(x, main ="Boxplots for numerc columns",col = "lightblue", boxwex=0.5)
}

boxplots(df_numeric)

##Dealing with one column before i ctreate a function for all at ago
IQR(df$DSC3108)

summary(df$DSC3108)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00   65.00   77.00   80.21   89.00 1000.00 

Tmin = 65.00 - 1.5* 24
Tmax = 1000 + 1.5* 24

df$DSC3108[which(df$DSC3108) < Tmin | df$DSC3108 > Tmax]

df$DSC3108 = df$DSC3108[which(df$DSC3108) > Tmin] & df$DSC3108[which(df$DSC3108) < Tmax]


remove_outliers <- function(x) {
  for (col in names(x)) {
    if (is.numeric(x[[col]])) {
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Calculate lower and upper bounds for outliers
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Remove rows where the value is an outlier
      x <- x[x[[col]] >= lower_bound & x[[col]] <= upper_bound, ]
      
    }
  }
  return(x)
}

remove_outliers(df_numeric)
boxplots(df_numeric)
