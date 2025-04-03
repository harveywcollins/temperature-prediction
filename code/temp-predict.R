# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Load in cleaned data

url <- "https://raw.githubusercontent.com/harveywcollins/temperature-prediction/refs/heads/main/data/cleaned_temperature_data.csv"
temperature_data <- read.csv(url)

head(temperature_data)

# Visulation of Raw Data

ggplot(temperature_data, aes(x = Date, y = Av.temp, group = 1)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily Average Temperature (1901-2019)",
       x = "Date",
       y = "Average Temperature (C)") +
  theme_bw()

