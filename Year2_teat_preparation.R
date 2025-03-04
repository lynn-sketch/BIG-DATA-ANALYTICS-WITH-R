### SEETING HE WPRKING DIRECTORATE ###
setwd("H:\\2024-25-R-Analysis\\2025-Analysis\\Practise")

## Loading the libraries ###
library(tidyverse)
library(tidyr)
library(tidyselect)
library(ggplot2)
library(data.table)
library(readxl)
library(readr)

### Loadig the data set ##
df <- read_excel("H:/2024-25-R-Analysis/Data_set/Vegetable_Sales-1.xlsx")
df %>% 
  view()
