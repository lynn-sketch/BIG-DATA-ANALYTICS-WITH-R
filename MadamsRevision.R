library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(tidyselect)
library(data.table)
library(dplyr)
library(tidyselect)

setwd("C:/Users/HP PC/Desktop/readrafresh")
df <- read_csv("Diabetes Missing Data.csv")
view(df)
glimpse(df)

##central tendency
summary(df)
mean(df$BMI)

##checking for missing values
sum(is.na(df$Glucose))

##getting mode of a specific variable
##R doesnt have mode so we generate a mode function

getmode = function(v){
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

##using the function on any variable
v = df 
mode_BMI = getmode(v)
view(mode_BMI)
##this works for categorical data

##deleting missing variables in a specific column
sum(is.na(df$Skin_Fold))
Data_omit = na.omit(df)
sum(is.na(Data_omit))

sum(is.na(df$Glucose))

##listing columns with missing values
##This code shows all columns with missing values greater than 2
listMissingColumns = colnames(df)[apply(df,2,anyNA)]
listMissingColumns

##Showing the number of missing values in each column using colSums
colSums(is.na(df))

##replacing missing values with a zero
Data_Zero = replace(df,is.na(df),0)
colSums(is.na(Data_Zero))
view(Data_Zero)

DataZero2 = replace(df,is.na(df),0)
view(Data_Zero)

colSums(is.na(df))
##replacing missing values with imputation mean or median if not normally distributed

hist(df$Diastolic_BP)
df$Diastolic_BP[is.na(df$Diastolic_BP)] <- mean(df$Diastolic_BP,na.rm = TRUE)
sum(is.na(df$Diastolic_BP))

hist(df$BMI)
df$BMI[is.na(df$BMI)] = median(df$BMI,na.rm = TRUE)

##Handling outliers
outliers_Diastolic_BP <- boxplot.stats(df$Diastolic_BP)$out
outliers_Diastolic_BP

##all outliers
outliers_dataset <- boxplot.stats(df)$out
outliers_dataset

boxplot(df$Diastolic_BP)

##Removing outliers using IQR
summary(df$Diastolic_BP)
IQR(df$Diastolic_BP)

##getting the threshold values
Tmin = 64.00 - (1.6*16)
Tmax =  80.00 + (1.6*16)
Tmin
Tmax
##Removing outliers in Diastolic_BP
outliers_Diastolic_BP <- df$Diastolic_BP[which(df)]
boxplot(df$Diastolic_BP)


##Dealing with nyc flights dataset
library(nycflights13)

##calling the data
nycflights13::flights
?flights
str(flights)
glimpse(flights)

##Filtering in flights picking variables by their values
##selecting all flights on first

jan_1 <- filter(flights,month==1,day==1)
jan_1

sqrt(2)^2 == 8
##instead use the near function
near(sqrt(2)^2,2)

# To select flights that departed in November or December
view(flights)
Nov_Dec_flights = filter(flights,month == 11|month == 12)
Nov_Dec_flights

##or
Nov_Dec_flights2 <- filter(flights, month %in% c(11,12))
Nov_Dec_flights2

# To find flights that were not delayed (on arrival/departure) by more than 2 hours
Undelayedflights <- filter(flights,!(arr_delay >120|dep_delay > 120))
Undelayedflights

##missing values
sum(is.na(flights))
colSums(is.na(flights))

##questions
# Find all flights that;





#7. Departed between midnight and 6am (inclusive)
#8. Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
#9. How many flights have a missing dep_time? What other variables are missing? What might these rows represent?


#1. Had an arrival delay of two or more hours
arrival_delay <- filter(flights,arr_delay >= 120)
arrival_delay
flights3 <- flights %>% 
  filter(arr_delay >= 120) %>% 
  select(all_of(selected_columns))
flights3

#2. Flew to Houston (IAH or HOU)
IAH_HOUFLIGHTS <- filter(flights,dest == "IAH" | dest == "HOU")
IAH_HOUFLIGHTS

selected_columns <- c("arr_delay", "dest", "carrier", "flight")

flights2 <- flights %>% 
  filter(dest == "IAH" | dest == "HOU") %>% 
  select(all_of(selected_columns))
flights2

#3. Were operated by United, American, or Delta
selected_columns2 <- c("carrier","arr_delay", "dest", "carrier", "flight")
flight4 <- flights %>% 
  filter(carrier=="UA"|carrier=="DL") %>% 
  select(all_of(selected_columns2))
flight4

#4. Departed in summer (July, August, and September)
Months <- filter(flights,month==7,month == 8,month==9)
Months
##they dont exist

#5. Arrived more than two hours late, but didn’t leave late
AirDelay <- filter(flights,arr_delay > 120 & dep_delay == 0)
AirDelay


#6. Were delayed by at least an hour, but made up over 30 minutes in flight

##to be continued later
##.3. CHANGING ROWS WITH ARRANGE ()
reordeing <- arrange(flights,year,dep_time,arr_delay)
reordeing
arrange(flights,desc(dep_delay))

#### 3.4. SELECT COLUMNS WITH SELECT #####
select(flights,origin,dep_delay,arr_delay)

##renaming variables
flights <- rename(flights,departureDelay = dep_delay)
view(flights)

###### 3.5. ADD VARIABLES WITH MUTATE() ####### 
#Select columns year-day, delays, distance and air_time and call the output flights_sml
flights_sml <- 
  select(flights,
    year:day,
    ends_with("delay"),
    distance,
    air_time
  )
view(flights_sml)

##Add the columns "gain" (dep_delay-arr_delay) and "speed" (distance/air_time * 60) and name the output an object flights_sml2:
##this is to change the column names 
 mutate(
  flights_sml,
  gain = departureDelay-arr_delay,
  speed = distance/air_time * 60
  
)
 ##to mutate and keep them permanently we use transmute
 transmute(
   flights_sml,
   gain = departureDelay-arr_delay,
   speed = distance/air_time * 60
   
 )
view(flights_sml)

by_day <- group_by(flights, year, month, day)
