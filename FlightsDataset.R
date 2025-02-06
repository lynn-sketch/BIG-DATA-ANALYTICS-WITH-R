setwd("C:/Users/HP PC/Desktop/BSDS 2_2/BIG DATA ANALYTICS")
library(tidyverse)
library(readxl)
library(ggplot2)
library(tidyselect)
library(nycflights13)
library(tidyr)

#Exploring the dataset of nycflights13
?flights

##filtering enables us pick observations by their values
filter(flights,month ==1, day ==1)

Save the result as Jan1
Jan1 = filter(flights,month ==1, day ==1)
view(Jan1)

near(sqrt(2) ^ 2,2)

#Selecting flights that departed in november or december
nov_dec <- filter(flights, month ==11 | month ==12)
view(nov_dec)

#To find out flights that were not delayed (on arrival/departure) by more than 2 hours
delay = filter(flights, !(arr_delay > 120 | dep_delay > 120))
delay = filter(flights, arr_delay <= 120,dep_delay <= 120)
view(delay)

#Checking for missing values
Missingdata <- is.na(flights)
view(Missingdata)

#Questions for practice


#Changing Rows with arrange
arrange(flights,year,month,day)
arrange(flights,desc(dep_delay))

#Selecting particular columns
select(flights, year:day)
rename(flights,tail_num = tailnum)

#Mutation(Addition of columns gain = dep_delay-arr_delay, speed =distance/airtime*60)
flight_sml <- select(flights,
  year:day,
  ends_with("delay"),
  distance,
  air_time)

flight_sml2 <- mutate(flight_sml,gain = dep_delay - arr_delay,
                      speed = distance/air_time *60)
view(flight_sml2)

#To permanently add them to your data
transmute(flight_sml,
           gain = dep_delay - arr_delay,
           speed = distance/air_time *60)

view(flight_sml)
 