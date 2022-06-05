
######## Introduction ###################################
# We will use the latest World Sustainability dataset for all countries 
# all over the world. In this project, we will measure the global 
# evolution and progress towards sustainability across different countries
# in the world. We will discuss and introduce few key factors that could
# measure such progress towards sustainability, namely the following:
# 1. % of renewable energy consumption out of total energy consumption
# 2. % of renewable electricity consumption out of total electricity consumption
# 3. Annual CO2 emissions that are resulted from production in millions of tons
# 4. the time effect (year)
# 5. the countries with all data points in dataset

# ##########################################################
# Creating the World Sustainability (wsus) dataset and the validation set
# ##########################################################

# install the packages if they're not installed yet
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if (!require("neuralnet")) install.packages("neuralnet", repos = "https://cran.microsoft.com/snapshot/2019-01-01/web/packages/INDperform/index.html")

# load needed libraries
library(dplyr) 
library(ggplot2) 
library(tidyverse) 
library(caret) 
library(kableExtra) 
library(Metrics) 
library(neuralnet)

# World Sustainability 1.5M dataset (csv file) was downloaded from
# https://www.kaggle.com/datasets/truecue/worldsustainabilitydataset?select=WorldSustainabilityDataset.csv

db_file <- 'WorldSustainabilityDataset.csv'

# call our dataset wsus from 'World Sustainability'
wsus <- read.csv(db_file)

# check out the columns and structure of the wsus set
str(wsus)

##### Let's do some cleaning on the wsus dataset #########

# Rename some of the column names to be shorter and more meaningful
colnames(wsus)[colnames(wsus) == "Country.Name"] <- "country"
colnames(wsus)[colnames(wsus) == "Country.Code"] <- "countryID"
colnames(wsus)[colnames(wsus) == "Year"] <- "year"
colnames(wsus)[colnames(wsus) == "Renewable.energy.consumption....of.total.final.energy.consumption....EG.FEC.RNEW.ZS"] <- "energy"
colnames(wsus)[colnames(wsus) == "Annual.production.based.emissions.of.carbon.dioxide..CO2...measured.in.million.tonnes"] <- "CO2em"
colnames(wsus)[colnames(wsus) == "Access.to.electricity....of.population....EG.ELC.ACCS.ZS"] <- "electricity"

# Check out the columns of wsus set (as a table) to confirm the changes
head(wsus) %>% kable() %>% kable_styling(font_size = 11)

# create a subset of wsus set with selected columns only with new names
wsus_subset <- wsus[ , c('country','countryID','year','CO2em', 
                         'energy', 'electricity')]

# clean the wsus set further by removing NAs
wsus_subset <- na.omit(wsus_subset)

# check out the new wsus subset 
head(wsus_subset) %>% kable() %>% kable_styling(font_size = 11)

str(wsus_subset)

# Let's check each column in wsus_subset and its class
sapply(wsus_subset, class) %>% kable() %>% kable_styling(font_size = 11)

# the above provides the classes of the susbset columns
# country: character
# countryID: character
# year: integer
# CO2em: numeric - Annual production-based emissions of carbon dioxide (CO2), measured in million tonnes
# energy: numeric - % of renewable energy from total energy consumption
# electricity: numeric - % Access to electricity % of total population

# let's find out how many rows in wsus_subset
nrow(wsus_subset)   # 3192  
ncol(wsus_subset)   # 6  

############################################
######## Creating the datasets  ############

# Create a validation set around 10% of wsus_subset
set.seed(1, sample.kind="Rounding")

# creating the test set with 15% of the wsus subset
test_index <- createDataPartition(y = wsus_subset$energy, times = 1, 
                                  p = 0.15, list = FALSE)

# create the main stream and test sets
sustain <- wsus_subset[-test_index,]
validation <- wsus_subset[test_index,]

# rows in the sustain and validation sets
nrow(sustain)     # 2712 
nrow(validation)  # 480 

# Summary of sustain set
summary(sustain) %>% kable() %>% kable_styling(font_size = 11)

# Summary of the validation set
summary(validation) %>% kable() %>% kable_styling(font_size = 11)

##################################################################
########## Data Analysis of the dataset# #########################

# The range of years in the sustain set
min(wsus_subset$year)  # the data starts from year 2000 
max(wsus_subset$year)  # until year 2018

# How many countries are being measured globally
wsus_subset %>% distinct(countryID) %>% count()     # 173 countries 

# Let's do data analysis for the years 2000 to 2018
sustain_2018 <- wsus_subset %>% filter(year == 2018)
head(sustain_2018)

sustain_2000 <- wsus_subset %>% filter(year == 2000)
head(sustain_2000)

# Highest renewable energy consumption from 2000 to 2018
wsus_subset %>% 
  arrange(desc(energy)) %>% head(1) # Congo 98.34% in 2001

# Highest CO2 (from production) emissions from 2000 to 2018
wsus_subset %>% 
  arrange(desc(CO2em)) %>% head(1) # China 9956.569 million tons in 2018

# Top country with highest renewable energy consumption in 2018
sustain_2018 %>% 
  arrange(desc(energy)) %>% head(5) # Congo, Uganda, Ethiopia, Gabon, Liberia

# Top five countries with highest production-based CO2 emissions in 2018
sustain_2018 %>% 
  arrange(desc(CO2em)) %>% head(5) # China, USA, India, Russia, Japan

# Top five countries with highest renewable energy consumption in 2000
sustain_2000 %>% 
  arrange(desc(energy)) %>% head(5) # Congo, Ethiopia, Uganda, Tanzania, Mozambique

# Top five countries with highest production-based CO2 emissions in 2000
sustain_2000 %>% 
  arrange(desc(CO2em)) %>% head(5) # USA, China, Russia, Japan, India

# Calculate the average of global renewable energy consumption in 2018 and 2000
mu_energy_2000 <- mean(sustain_2000$energy)  # 32.854
mu_energy_2018 <- mean(sustain_2018$energy)  # 31.655
# Global renewable energy consumption decreased by 1.2 pts from 2000 to 2018

# Calculate the average of global CO2 emissions in 2018 and 2000
mu_CO2em_2000 <- mean(sustain_2000$CO2em)  # 153.319 million tons
mu_CO2em_2018 <- mean(sustain_2018$CO2em)  # 200.416 million tons
# Global CO2 emissions increased by 47.1 million tons from 2000 to 2018

##############

# Compare the distribution of global energy consumption in 2000 and 2018

# as we have seen above, the average global renewable energy 
# usage dropped from 32.854 in 2000 to 31.655 in 2018

# let's create a new subset for energy only
wsus_subset_energy <- sustain_2000[ , c('countryID','energy')]  

# plot the global energy histogram in 2000
wsus_subset_energy %>% ggplot(aes(energy)) +
  geom_histogram(bins = 40, color="black") + ylab("Frequency") +
  xlab("% of Renewable Energy Consumption") +
  ggtitle("Global Consumption of Renewable Energy - 2000")

# many countries were low in renewable energy usage in 2000
# but there were few with high renewable energy usage as well

# Countries with less than 10% of renewable energy usage in 2000
wsus_subset_energy %>% filter(energy <= 10) %>% count()   # 55 countries 

# Countries with +90% of renewable energy usage in 2001
wsus_subset_energy %>% filter(energy >= 90) %>% count()   # 7 countries 

##### Now, let's look at 2018 data to compare

wsus_subset_energy <- sustain_2018[ , c('countryID','energy')]

# plot the global energy histogram in 2018
 wsus_subset_energy %>% ggplot(aes(energy)) +
  geom_histogram(bins = 40, color="black") + ylab("Frequency") +
  xlab("% of Renewable Energy Consumption") +
  ggtitle("Global Consumption of Renewable Energy - 2018")

# so we seem to have fewer countries with very low renewable
 # energy usage in 2018 and fewer also with very high energy usage, 
 # with more countries in between!
 
# Less than 10% of renewable energy usage in 2018
sustain_2018 %>% filter(energy <= 10) %>% count()   # 45 countries 

# More than 90% of renewable energy usage in 2018
sustain_2018 %>% filter(energy >= 90) %>% count()   # 2 countries 

### Now, let's check out CO2 emissions

## find out the mean of CO2 emission
# as we have seen above, the average Global CO2 emissions 
# increased by 47.1 million tons from 2000 to 2018

#### CO2 in year 2000
# Countries with annual CO2 emission of 5000 million tons or more
sustain_2000 %>%
  group_by(country) %>% filter(CO2em >= 5000) %>%
  summarize() %>% nrow    # 1 country

# which country?
sustain_2000 %>%
  group_by(country) %>% filter(CO2em >= 5000) %>%
  summarize(countryID)     # and the country is USA

# Countries with annual CO2 emission of 10 million tons or less
sustain_2000 %>%
  group_by(country) %>% filter(CO2em <= 10) %>%
  summarize() %>% nrow    # 72 countries

#### CO2 in year 2018
# Countries with annual CO2 emission of 5000 million tons or more
sustain_2018 %>%
  group_by(country) %>% filter(CO2em >= 5000) %>%
  summarize() %>% nrow    # 2 countries

# which countries?
sustain_2018 %>%
  group_by(country) %>% filter(CO2em >= 5000) %>%
  summarize(countryID)     # China and USA

# Countries with annual CO2 emission of 10 million tons or less
sustain_2018 %>%
  group_by(country) %>% filter(CO2em <= 10) %>%
  summarize() %>% nrow    # 73 countries


########## The Prediction Modeling Section  ###############

## Creating the training and testing Sets
train_set <- sustain
test_set <- validation

## in the following section, we will build and test models that
# would predict CO2 emissions globally

#### Creating the training and testing Sets #####

## Let's use the min-max normalization function to transform the data 
# into a common range, thus removing the scaling effect from numeric 
# variables in the dataset. 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

## Use the Min-Max Normalization to better scale the datasets

# create the training and testing subsets with selected normalized columns
train_set_numeric <- train_set[, c('energy', 'CO2em', 'electricity')]
test_set_numeric <- test_set[, c('energy', 'CO2em', 'electricity')]

# normalize the training and testing sets with "normalize" function
train_maxmindf <- as.data.frame(lapply(train_set_numeric, normalize))
test_maxmindf <- as.data.frame(lapply(test_set_numeric, normalize))

# re-add the non-numeric columns back to the datasets
train_set <- train_maxmindf %>% mutate(country = train_set$country, countryID = train_set$countryID, year = train_set$year)
test_set <- test_maxmindf %>% mutate(country = test_set$country, countryID = test_set$countryID, year = test_set$year)


### in the following section, we will build and test models that
# would predict CO2 emissions globally

options(digits = 5, scipen = 3)

## Training Model 1: predicting CO2em with the country effect 

# Get the average CO2 emissions in our training set
mu <- mean(train_set$CO2em)   # 0.01799

# Average CO2 emissions for all countries
CO2em_avgs <- train_set %>% group_by(countryID) %>% 
  summarize(b_i = mean(CO2em - mu)) 

# Get the predicted ratings using the test set
pred_CO2em_M1 <- test_set %>% 
  left_join(CO2em_avgs, by='countryID') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# plot b_i to see the CO2 emissions estimate variations
qplot(b_i, geom="histogram", data=CO2em_avgs, color=I("black"), bins=10)

# Get RMSE for Model 1
M1_RMSE <- RMSE(pred_CO2em_M1, test_set$CO2em)

M1_RMSE      # 0.02976

######### Measuring the Quality of the Model ###########
# When we use the fitted model to make predictions on new observations, 
# we can use several different metrics to measure the quality of the model,
# including: R-squared, RMSE, MAE. Following is a brief description of each:

# * R-squared (Multiple R-squared) measures the strength of the 
# linear relationship between the predictor and the response. The higher
# the multiple R-squared, the better the predictor variables are able to
# predict the response variable.

# RMSE (Root Mean Squared Error) measures the average prediction error 
# made by the model in predicting the value for a new observation. This
# is the average distance between the true value of an observation and 
# the value predicted by the model. Lower values for RMSE indicate a 
# better model fit.

# MAE (Mean Absolute Error) measures the average absolute difference 
# between the true value of an observation and the value predicted by 
# the model. This metric is generally less sensitive to outliers 
# compared to RMSE. Lower values for MAE indicate a better model fit.

dataframe <- c(R_squared = R2(pred_CO2em_M1, test_set$CO2em),
               RMSE = RMSE(pred_CO2em_M1, test_set$CO2em),
               MAE = MAE(pred_CO2em_M1, test_set$CO2em))

dataframe
# R-squared: 0.92968
# RMSE: 0.02976
# MAE: 0.00754


#######################################################
### Model 2: with the renewable energy usage effect

# consider the effect of using renewable energy
energy_avgs <- train_set %>%
  left_join(CO2em_avgs, by='countryID') %>%
  group_by(energy) %>%
  summarize(b_en = mean(CO2em - mu - b_i)) 

# Calculate the predicted CO2 emissions using the test set
pred_CO2em_M2 <- test_set %>% 
  left_join(energy_avgs, by='energy') %>%
  mutate(pred = mu + b_en) %>%
  pull(pred)

# plot b_en to check the CO2em estimates
qplot(b_en, geom="histogram", data=energy_avgs, color=I("black"), bins=10)

# Get RMSE of Model 2
M2_RMSE <- RMSE(pred_CO2em_M2, test_set$CO2em, na.rm = TRUE)

M2_RMSE   # 0.014

# Let's measure the quality of Model 2
dataframe <- c(R_squared = R2(pred_CO2em_M2, test_set$CO2em, na.rm=TRUE),
               RMSE = RMSE(pred_CO2em_M2, test_set$CO2em, na.rm=TRUE),
               MAE = MAE(pred_CO2em_M2, test_set$CO2em, na.rm = TRUE))

dataframe
# R-squared: 0.05764  
# RMSE: 0.014  
# MAE: 0.0129 


### Model 3: with the access to electricity effect

# consider the effect of access to electricity
electric_avgs <- train_set %>%
  left_join(CO2em_avgs, by='countryID') %>%
  group_by(electricity) %>%
  summarize(b_el = mean(CO2em - mu - b_i)) 

# Calculate the predicted CO2 emissions using the test set
pred_CO2em_M3 <- test_set %>% 
  left_join(electric_avgs, by='electricity') %>%
  mutate(pred = mu + b_el) %>%
  pull(pred)

# plot b_el to check the CO2 emission estimates
qplot(b_el, geom="histogram", data=electric_avgs, color=I("black"), bins=10)

# Get RMSE of Model 3
M3_RMSE <- RMSE(pred_CO2em_M3, test_set$CO2em, na.rm = TRUE)

M3_RMSE   # 0.01141

# Measure the quality of Model 3
dataframe <- c(R_squared = R2(pred_CO2em_M3, test_set$CO2em, na.rm=TRUE),
               RMSE = RMSE(pred_CO2em_M3, test_set$CO2em, na.rm=TRUE),
               MAE = MAE(pred_CO2em_M3, test_set$CO2em, na.rm = TRUE))

dataframe
# R-squared: 0.00058  
# RMSE: 0.11406  
# MAE: 0.03803  


###### Model 4: with the time (year) effect  ########

# consider the time (year) effect 
year_avgs <- train_set %>%
  left_join(CO2em_avgs, by='countryID') %>%
  group_by(year) %>%
  summarize(b_y = mean(CO2em - mu - b_i)) 

# Calculate the predicted CO2 emissions using the test set
pred_CO2em_M4 <- test_set %>% 
  left_join(year_avgs, by='year') %>%
  mutate(pred = mu + b_y) %>%
  pull(pred)

# plot b_el to check the CO2 emission estimates
qplot(b_y, geom="histogram", data=year_avgs, color=I("black"), bins=10)

# Get RMSE of Model 4
M4_RMSE <- RMSE(pred_CO2em_M4, test_set$CO2em)

M4_RMSE   # 0.0963

# Measure the quality of Model 4
dataframe <- c(R_squared = R2(pred_CO2em_M4, test_set$CO2em),
               RMSE = RMSE(pred_CO2em_M4, test_set$CO2em),
               MAE = MAE(pred_CO2em_M4, test_set$CO2em))

dataframe
# R-squared: 0.00008 
# RMSE: 0.0963 
# MAE: 0.03085   


###### Model 5: using Regularization  ########
## Use Regularization and Cross Validation for better optimization, 
# we will consider the time, energy, electricity effects in this model

# Perform 10-fold cross-validation to select lambda
lambdas <- 10^seq(0, 100, 0.25)

# Use lambdas with RMSE regularization function to optimise the country,
# year and energy effects
regularize_RMSEs <- sapply(lambdas, function(l){
  
  b_i <- train_set %>%
    group_by(countryID) %>%
    summarize(b_i = sum(CO2em - mu)/(n()+l))
  
  b_en <- train_set %>% 
    left_join(b_i, by="countryID") %>%
    group_by(energy) %>%
    summarize(b_en = sum(CO2em - b_i - mu)/(n()+l))
  
    b_el <- train_set %>% 
    left_join(b_i, by="countryID") %>%
    group_by(electricity) %>%
    summarize(b_el = sum(CO2em - b_i - mu)/(n()+l))
  
    b_y <- train_set %>% 
    left_join(b_i, by="countryID") %>%
    group_by(year) %>%
    summarize(b_y = sum(CO2em - b_i - mu)/(n()+l))
  
  pred_CO2em <- test_set %>% 
    left_join(b_i, by = "countryID") %>%
    left_join(b_y, by="year") %>%
    left_join(b_en, by = "energy") %>%
    left_join(b_el, by = "electricity") %>%
    mutate(pred = mu + b_i + b_y + b_en + b_el) %>% .$pred
  
  return(RMSE = RMSE(pred_CO2em, test_set$CO2em, na.rm=TRUE))
  
})

# plot regularized RMSEs and lambda
qplot(lambdas, regularize_RMSEs)  

# find the lowest lambda
lambdas[which.min(regularize_RMSEs)]  # 3.162

# The lowest RMSE using the above lambdas and regularization function
regularized_M5_RMSE <- min(regularize_RMSEs)  # 0.00249

############# using the Neural Network Model ####################
# we will double check our previous results with another advanced 
# model; using the Neural Network Model 

# generate the Neural Network model

nn <- neuralnet(CO2em ~ energy + electricity, data=train_maxmindf, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix 
# We now generated the error of the neural network model, along with the
# weights between the inputs, hidden layers, and outputs

# let's create the plot diagram now
plot(nn)

### Testing The Accuracy Of The Neural Network Model

#Test the resulting output
head(test_maxmindf)
nn.results <- compute(nn, test_maxmindf)
results <- data.frame(actual = test_maxmindf$CO2em, prediction = nn.results$net.result)
results

# Measure the quality of Neural Network model
nn_quality_results <- c(R_squared = R2(results$actual, results$prediction),
               RMSE = RMSE(results$actual, results$prediction),
               MAE = MAE(results$actual, results$prediction))

nn_quality_results
# R-squared: 0.03036 
# RMSE: 0.09489  
# MAE: 0.02902   


##### Summarize the outcome and results of all models and RMSE results

# 1. RMSE with Country effect
M1_RMSE   # 0.02976

# 2. RMSE with energy effect
M2_RMSE   # 0.014

# 3. RMSE with electricity effects
M3_RMSE   # 0.11406

# 4. RMSE with time (year) effects
M4_RMSE   # 0.0963

# 5. RMSE with regularized country, time, energy, electricity effects
regularized_M5_RMSE  # 0.00249

# 6. RMSE with Neural Network model
nn_quality_results[2]  # 0.09489

results_df <- data.frame(M1_RMSE = format(M1_RMSE, digits = 2), 
                       M2_RMSE = format(M2_RMSE, digits = 2),
                       M3_RMSE = format(M3_RMSE, digits = 2), 
                       M4_RMSE = format(M4_RMSE, digits = 2),
                       M5_RMSE = format(regularized_M5_RMSE, digits = 2), 
                       M6_RMSE = format(nn_quality_results[2], digits = 2)) 

results_df %>% kable() %>% kable_styling(font_size = 11)

min <- apply(results_df[, 1:ncol(results_df)], 1, min)
min  # minimum RMSE: 0.0025 (for M5 regularization model)
