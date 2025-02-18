###ANALYSING BIG DATA: DATA ANALYTICS
#USING SPARKLYR PACKAGE
###1. INSTALLATION USING CODE
install.packages("sparklyr")
library(sparklyr)
library(dplyr)
#Then install Spark
spark_install()
spark_install(version = "3.5")
#Check the versions installed
spark_available_versions()
#And set up a local installation of Spark
sc=spark_connect(master = "local")

#2. MANUAL INSTALLATION IN RSTUDIO
#A) Go to the "Connections" tab in RStudio
#B) Click "New Connection"
#C) Select "Spark"
#D) Changes the Spark Connect Parameters
###i) Master: Select "local"
###ii) DB Interface: Select "dplyr"
##iii) Spark Version: Select the latest version 
##iv) Hadoop Version: Select the Default version
##v) Connect From: Select "R console"
#E) Then click OK and wait for the installation to occur. 


##3.TROUBLESHOOTING FOR INSTALLATION
#Warning: In Case the following error is being encountered:
  #ERROR: configuration failed for package 'stringi'
# Install the stringi dependency as follows:
#  install.packages(c(“stringi”),configure.args=c(“-disable-cxxll”),repos=”https://cran.rstudio.com”)
# Install the sparklyr package again as follows:
 # install.packages("sparklyr")
#Note: The default installation paths are ~/spark for macOS and Linux, and %LOCALAPPDATA%/spark for Windows.

##4. IMPORTING DATASET
#Using an online dataset sourced from https://oreil.ly/Uv9r_
download.file(
  "https://github.com/r-spark/okcupid/raw/master/profiles.csv.zip",
  "okcupid.zip")
#Unzip the contents in to a folder called "data" under spark
unzip("okcupid.zip", exdir = "data")
unlink("okcupid.zip")

#Import necessay libraries
library(dbplot)
library(ggplot2)
install.packages("ggmosaic")
install.packages("forcats")
install.packages("FactoMineR")
library(ggmosaic)
library(forcats)
library(FactoMineR)

#ii. Copy the dataset to the local Spark server
#escape = "\"" and options = list(multiline = TRUE) here to accommodate embedded quote characters and newlines in the essay fields
#We also convert the height and income columns to numeric types and recode missing values in the string columns
okc <- spark_read_csv(
  sc, 
  "data/profiles.csv", 
  escape = "\"", 
  memory = FALSE,
  options = list(multiline = TRUE)
) %>%
  mutate(
    height = as.numeric(height),
    income = ifelse(income == "-1", NA, as.numeric(income))
  ) %>%
  mutate(sex = ifelse(is.na(sex), "missing", sex)) %>%
  mutate(drinks = ifelse(is.na(drinks), "missing", drinks)) %>%
  mutate(drugs = ifelse(is.na(drugs), "missing", drugs)) %>%
  mutate(job = ifelse(is.na(job), "missing", job))

#iii. View the dataset using glimpse
glimpse(okc)


##5. Data Transformation to answer questions
#Generate a new variable "not working" to see the people who are unemployed
#People who respond as being "student" "retired" or "unemployed"
okc <- okc %>%
  mutate(
    not_working = ifelse(job %in% c("student", "unemployed", "retired"), 1 , 0)
  )

#Check the number of not_working
okc %>% 
  group_by(not_working) %>% 
  tally()
#Result: 5405 people are not working
# 54541 people are working

#Further explorations using spark functions
#sdf_describe() to get descriptive stats
sdf_describe(okc_train, cols = c("age", "income"))

##6. Data modelling using regression:
#Split data for training and testing the model
#sdf_random_split function splits a spark dataframe
data_splits <- sdf_random_split(okc, training = 0.8, testing = 0.2, seed = 42)
okc_train <- data_splits$training
okc_test <- data_splits$testing


##7. Looking at variable distributions/proportions
#i. Distribution of age in the split data
dbplot_histogram(okc_train, age)

#ii. Proportion of not_working
okc_train %>%
  group_by(not_working) %>%
  tally() %>%
  mutate(frac = n / sum(n))


##8. Load the train and testing data in to spark
# load data to spark
okc_train_spark <- copy_to(sc, okc_train, "okc_train_spark")
okc_test_spark <- copy_to(sc, okc_test, "okc_test_spark")

##9. Fit and test different  models on the training data
#Linear regression
#Boosted trees regressor
#Random forest regressor
#decision trees regressor
#i. Simple linear regression
# specify the linear model
model1 <- not_working ~ age
# fit the model with OLS
fit1 <- lm(model1, okc_train_spark)
# compute t-tests etc.
summary(fit1)

#Alternatively use other engines supported in spark
library(tidymodels)
library(parsnip)
library(tidyverse)
# simple local linear regression example from above
# via tidymodels/parsnip
fit1 <- fit(linear_reg(engine="lm"), model1, data=okc_train_spark)
tidy(fit1)
#i. Machine Learning linear regression
lr = ml_linear_regression(engine = "spark") %>% ml_fit(formula = not_working ~ age, data = okc_train_spark)
#View result of model
lr
#Get validation metrics on test dataset
validation_summary <- ml_evaluate(lr, okc_test_spark)
#View result
validation_summary
#You can access different validation metrics using ml_summary() or $
validation_summary$accuracy()
#Result: 90.78% accurate

##ii. Generalised linear regression
glr <- ml_generalized_linear_regression(
  okc_train_spark, 
  not_working ~ sex + drinks + drugs, 
  family = "binomial")
#Get validation metrics on test dataset
validation_summary2 <- ml_evaluate(glr, okc_test_spark)
validation_summary2$residuals()

#Extract the coefficient estimates in a tidyr object and then plot using ggplot
tidy_glr <- tidy(glr)
tidy_glr %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, 
        ymax = estimate + 1.96 * std.error, width = .1)
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed")

##iii. Decision tree regressor
dtr <- ml_decision_tree_regressor(
  okc_train_spark, 
  not_working ~ sex + drinks + drugs)

#Validation
predictions=ml_predict(dtr, okc_test_spark)


##iv. Random forest regressor
rfr <- ml_random_forest_regressor(
  okc_train_spark, 
  not_working ~ sex + drinks + drugs)
#Validation
predictions2=ml_predict(rfr, okc_test_spark)
predictions2

##vi. Compare the model performances
models <- list(lr, glr, dtr, rfr)



##10. Test classification models
#Using another dataset e.g. mpg
#Target variable is "model" 
#i. Splitting data
data_splits2 <- sdf_random_split(mpg, training = 0.8, testing = 0.2, seed = 42)
mpg_train <- data_splits2$training
mpg_test <- data_splits2$testing


#ii. Load split data to spark
mpg_train_spark <- copy_to(sc, mpg_train, "mpg_train_spark")
mpg_test_spark <- copy_to(sc, mpg_test, "mpg_test_spark")

##iii. Fit models to training data
# models to be used
models2 <- list(logit=logistic_reg(engine="spark", mode = "classification"),
               btree=boost_tree(engine = "spark", mode = "classification"),
               rforest=rand_forest(engine = "spark", mode = "classification"))
# train/fit the models
fits <- lapply(models2, fit, formula=trans~., data=mpg_train_spark)


##iv. Run predictions for models
# run predictions
predictions <- lapply(fits, predict, new_data=mpg_test_spark)
# fetch predictions from Spark, format, add actual outcomes
library(rsample)

pred_outcomes <- 
  lapply(1:length(predictions), function(i){
    x_r <- collect(predictions[[i]]) # load into local R environment
    x_r$pred_class <- as.factor(x_r$pred_class) # format for predictions
    x_r$survived <- as.factor(mpg_test_spark$trans) # add true outcomes
    return(x_r)
    
  })

#v. Compare model accuracies
acc <- lapply(pred_outcomes, accuracy, truth="survived", estimate="pred_class")
acc <- bind_rows(acc)
acc$model <- names(fits)
acc[order(acc$.estimate, decreasing = TRUE),]

#OR use tidy to check individual performance
tidy(fits[["rforest"]])


#Disconnecting from spark
spark_disconnect(sc)
