# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Load in the daily temperature

url <- "https://raw.githubusercontent.com/harveywcollins/temperature-prediction/refs/heads/main/data/durhamtemp_1901_2019.csv"
temperature_data <- read.csv(url)

head(temperature_data)
str(temperature_data)
summary(temperature_data)

# Removing rows that have values as NA

temperature_data <- temperature_data[!apply(is.na(temperature_data), 1, all), ]
temperature_data <- na.omit(temperature_data)

# Convert the date column into a date type

temperature_data$Date <- dmy(temperature_data$Date)
str(temperature_data$Date)

# Verify that temperature values are consistent

temperature_data <- temperature_data %>%
  mutate(consistent = ifelse(Av.temp >= Tmin & Av.temp <= Tmax, TRUE, FALSE))

inconsistencies <- sum(!temperature_data$consistent)
cat("Number of inconsistent rows (Av.temp not between Tmin and Tmax):",  inconsistencies, "\n")

# Identify potential outliers

summary_stats <- summary(temperature_data$Av.temp)
print("Summary statistics for daily average temperature:")
print(summary_stats)

boxplot(temperature_data$Av.temp, main = "Boxplot of Daily Average Temperature",
        ylab = "Temperature (C)")

#renamed dataset for clarity
cleaned_data <- temperature_data
cleaned_data <- cleaned_data %>% select(-consistent)

# save as CSV file
write.csv(cleaned_data, "cleaned_temperature_data.csv", row.names = FALSE)
