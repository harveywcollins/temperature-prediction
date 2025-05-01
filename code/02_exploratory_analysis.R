# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Load in raw data

url <- "https://raw.githubusercontent.com/harveywcollins/temperature-prediction/refs/heads/main/data/durhamtemp_1901_2019.csv"
temperature_data <- read.csv(url)

head(temperature_data)

# Visulation of Raw Data

ggplot(temperature_data, aes(x = Date, y = Av.temp, group = 1)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily Average Temperature (1901-2019)",
       x = "Date",
       y = "Average Temperature (C)") +
  theme_bw()

ggplot(temperature_data, aes(x = Date, group = 1)) +
  geom_line(aes(y = Tmin), color = "dodgerblue", alpha = 0.6) +
  geom_line(aes(y = Tmax), color = "tomato", alpha = 0.6) +
  labs(title = "Daily Minimum and Maxmimum Temperatures",
       x = "Date",
       y = "Temperature (C)") +
  theme_bw()

# Aggregate daily data to monthly average temperature

monthly_data <- temperature_data %>%
  mutate(Year = as.integer(Year),
         Month = as.integer(Month)) %>%
  group_by(Year, Month) %>%
  summarise(monthly_avg = mean(Av.temp, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(Date = as.Date(paste(Year, Month, "15", sep = "-"))) %>%
  arrange(Date)

ggplot(monthly_data, aes(x = Date, y = monthly_avg, group = 1)) +
  geom_line(color = "darkgreen") +
  labs(title = "Monthly Average Temperature",
       x = "Date",
       y = "Temperature (C)") +
  theme_bw()

ggplot(monthly_data, aes(x = Date, y = monthly_avg)) +
  geom_line(aes(color = "Observed"), linewidth = 0.8) +
  geom_smooth(aes(colour = "Linear trend"),
              method = "lm",
              se = FALSE,
              linetype = "dashed",
              size = 0.8) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Observed" = "darkgreen",
      "Linear trend" = "blue"
    )
  ) +
  labs(
    title = "Monthly Average Temperature with Linear Trend",
    subtitle = "Durham Observatory (1901-2019)",
    x = "Date",
    y = "Temperature (C)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.background = element_rect(fill = alpha("white", 0.8), colour = "grey80"),
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.4, "cm")
  )

# Generate time series object for monthly data

start_year <- min(monthly_data$Year, na.rm = TRUE)
start_month <- min(monthly_data$Month[monthly_data$Year == start_year], na.rm = TRUE)
ts_monthly <- ts(monthly_data$monthly_avg, start = c(start_year, start_month), frequency = 12)

# Seasonal-Trend decomposition using Loess

ts_monthly <- na.omit(ts_monthly)
stl_decomp <- stl(ts_monthly, s.window = "periodic")
plot(stl_decomp, main = "STL Decomposition of Monthly Average Temperature")

# Summary stats by year

yearly_summary <- temperature_data %>%
  group_by(Year) %>%
  summarise(mean_temp = mean(Av.temp, na.rm = TRUE),
            median_temp = median(Av.temp, na.rm = TRUE),
            sd_temp = sd(Av.temp, na.rm = TRUE),
            n = n())
print("Yearly Summary Statistics:")
print(yearly_summary)

# Plot the yearly mean to see long-term trends
ggplot(yearly_summary, aes(x = Year, y = mean_temp)) +
  geom_line(color = "purple") +
  geom_point() +
  labs(title = "Yearly Mean Temperature",
       x = "Year",
       y = "Mean Temperature (C)") +
  theme_bw()

# Summary stats by month

monthly_summary <- temperature_data %>%
  group_by(Month) %>%
  summarise(mean_temp = mean(Av.temp, na.rm = TRUE),
            median_temp = median(Av.temp, na.rm = TRUE),
            sd_temp = sd(Av.temp, na.rm = TRUE),
            n = n())
print("Monthly Summary Statistics:")
print(monthly_summary)

# Plot the monthly mean to see long-term trends
ggplot(monthly_summary, aes(x = factor(Month), y = mean_temp)) +
  geom_bar(stat = "identity", fill = "coral", color = "black", alpha = 0.7) +
  labs(title = "Monthly Mean Temperature",
       x = "Month",
       y = "Mean Temperature (C)") +
  theme_bw()