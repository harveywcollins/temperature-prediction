library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(Metrics)
library(randomForest)
library(caret)
library(tensorflow)
library(keras)
library(readxl)
library(patchwork)

#set directory
setwd("C:/Users/Harvey Collins/OneDrive/Documents/MISCADA/Earth & Environmental Sciences/Mini_Project/temperature-prediction/code")

# 1. Data Preparation ---------------------------------------------

message("1/5 | Running Data Preparation...")
source("01_data_preparation.R")

# 2. Exploratory Analysis -----------------------------------------

message("2/5 | Running Exploratory Analysis...")
source("02_exploratory_analysis.R")

# 3. Time Series Modelling ----------------------------------------

message("3/5 | Running Classical Time Series Models...")
source("03_time_series_models.R")

# 4. Machine Learning Models  -------------------------------------

message("4/5 | Running Machine Learning Models...")
source("04_machine_learning_models.R")

# 5. 2020 Forecasts -----------------------------------------------

message("5/5 | Generating 2020 Forecasts...")
source("05_2020_forecasts.R")

# Done ------------------------------------------------------------

message("All steps completed successfully")