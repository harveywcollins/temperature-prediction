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

# Verify that temperature values are consistent

temperature_data <- temperature_data %>%
  mutate(consistent = ifelse(Av.temp >= Tmin & Av.temp <= Tmax, TRUE, FALSE))

inconsistencies <- sum(!temperature_data$consistent)
cat("Number of inconsistent rows (Av.temp not between Tmin and Tmax):",  inconsistencies, "\n")

# Removing rows that have values as NA

na_per_col <- sapply(temperature_data, function(x) sum(is.na(x)))
print(na_per_col)

rows_with_na <- which(!complete.cases(temperature_data))
cat("Rows containing at least one NA:\n", rows_with_na, "\n\n")

print( temperature_data[ rows_with_na, ])

temperature_data <- temperature_data[-rows_with_na, ]

sum(!complete.cases(temperature_data))

# Convert the date column into a date type

temperature_data$Date <- dmy(temperature_data$Date)
str(temperature_data$Date)

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
