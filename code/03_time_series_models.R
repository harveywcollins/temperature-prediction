# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)
library(Metrics)
library(patchwork)

# Load in cleaned data

url <- "https://raw.githubusercontent.com/harveywcollins/temperature-prediction/refs/heads/main/data/cleaned_temperature_data.csv"
temperature_data <- read.csv(url)

# Ensure Data column is in Date format is numeric

temperature_data$Year <- as.numeric(as.character(temperature_data$Year))
temperature_data$Month <- as.numeric(as.character(temperature_data$Month))
temperature_data$Day <- as.numeric(as.character(temperature_data$Day))
temperature_data$Date <- make_date(temperature_data$Year, temperature_data$Month, temperature_data$Day)
temperature_data$doy <- yday(temperature_data$Date)

# Data Partitioning

training_data <- temperature_data %>% filter(Year <= 2015)
validation_data <- temperature_data %>% filter(Year > 2015 & Year <= 2019)

cat("Training rows:", nrow(training_data), "\n")
cat("Validation rows:", nrow(validation_data), "\n")

# Creating a time series object: Note: ignoring leap years in this analysis

start_year <- training_data$Year[1]
start_doy <- yday(training_data$Date[1])

ts_train <- ts(training_data$Av.temp,
               frequency = 365,
               start = c(start_year, start_doy))

# ACF and PACF

acf(ts_train, main = "ACF of Training Data (Daily Average Temperature)")
pacf(ts_train, main = "PACF of Training Data (Daily Average Temperature)")

p_acf <- ggAcf(ts_train, lag.max = 50, main = "Autocorrelation Function") +
  labs(subtitle = NULL, x = "Lag", y = "ACF") +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.subtitle = element_blank(),
    panel.grid = element_blank()
  ) +
  annotate(
    "text", x = Inf, y = Inf, label = "(a)",
    hjust = 1.1, vjust = 1.3, size = 5, fontface = "bold"
  )

p_pacf <- ggPacf(ts_train, lag.max = 50, main = "Partial Autocorrelation Function") +
  labs(subtitle = NULL, x = "Lag", y = "PACF") +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.subtitle = element_blank(),
    panel.grid = element_blank()
  ) +
  annotate(
    "text", x = Inf, y = Inf, label = "(b)",
    hjust = 1.1, vjust = 1.3, size = 5, fontface = "bold"
  )

final_plot <- (p_acf + p_pacf) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  )

print(final_plot)

# Deseasoning via Regression

training_data$Date_numeric <- as.numeric(training_data$Date)
validation_data$Date_numeric <- as.numeric(validation_data$Date)

# Fit cubic regression to capture trend
fit_cubic <- lm(Av.temp ~ poly(Date_numeric, 3), data = training_data)
training_data$trend <- predict(fit_cubic, newdata = training_data)

# Calc residuals (detrended values)
training_data$resid <- training_data$Av.temp - training_data$trend

# Compute seasonal component from training data

daily_median <- training_data %>%
  group_by(doy) %>%
  summarise(median_resid = median(resid, na.rm = TRUE))

training_data <- training_data %>% left_join(daily_median, by = "doy")
training_data$best_resid <- training_data$resid - training_data$median_resid

# Plot deseasoned residuals
ggplot(training_data, aes(x = Date, y = best_resid)) +
  geom_line(color = "purple") +
  labs(title = "Deseasoned Residuals (Training Data)", x = "Date", y = "Residual (C)") +
  theme_minimal()

ts_best_resid <- ts(training_data$best_resid, frequency = 365,
                    start = c(min(training_data$Year), yday(min(training_data$Date))))

# Modelling the Deseasoned Residuals

h_val <- nrow(validation_data)

# Fit ARIMA model 
fit_arima_deseason <- auto.arima(ts_best_resid, seasonal = FALSE)
forecast_best_resid_arima <- forecast(fit_arima_deseason, h = h_val)

# Fit ETS model
fit_ets_deseason <- ets(as.numeric(training_data$best_resid))
forecast_best_resid_ets <- forecast(fit_ets_deseason, h = h_val)

# TBATS on Deseasoned Residuals
fit_tbats_deseason <- tbats(ts_best_resid)
forecast_best_resid_tbats <- forecast(fit_tbats_deseason, h = h_val)

# Reconstruct forecasted temperature for validation period

validation_data$trend <- predict(fit_cubic, newdata = validation_data)
validation_data <- validation_data %>% left_join(daily_median, by = "doy")

# ARIMA-based forecast

forecasted_resid_arima <- as.numeric(forecast_best_resid_arima$mean)
validation_data$pred_temp_arima <- validation_data$trend + validation_data$median_resid + forecasted_resid_arima

# ETS-based forecast

forecasted_resid_ets <- as.numeric(forecast_best_resid_ets$mean)
validation_data$pred_temp_ets <- validation_data$trend + validation_data$median_resid + forecasted_resid_ets

# TBATS-based forecast

forecasted_resid_tbats <- as.numeric(forecast_best_resid_tbats$mean)
validation_data$pred_temp_tbats <- validation_data$trend + validation_data$median_resid + forecasted_resid_tbats

# Compute error metrics

rmse_arima_deseason <- rmse(validation_data$Av.temp, validation_data$pred_temp_arima)
mae_arima_deseason <- mae(validation_data$Av.temp, validation_data$pred_temp_arima)

rmse_ets_deseason <- rmse(validation_data$Av.temp, validation_data$pred_temp_ets)
mae_ets_deseason <- mae(validation_data$Av.temp, validation_data$pred_temp_ets)

rmse_tbats_deseason <- rmse(validation_data$Av.temp, validation_data$pred_temp_tbats)
mae_tbats_deseason <- mae(validation_data$Av.temp, validation_data$pred_temp_tbats)

cat("ARIMA RMSE:", rmse_arima_deseason, "\n")
cat("ARIMA MAE:", mae_arima_deseason, "\n")

cat("ETS RMSE:", rmse_ets_deseason, "\n")
cat("ETS MAE:", mae_ets_deseason, "\n")

cat("TBATS RMSE:", rmse_tbats_deseason, "\n")
cat("TBATS MAE:", mae_tbats_deseason, "\n")

# Plot the Actual vs. Predicted temperatures in the validation period

ggplot(validation_data, aes(x = Date)) +
  geom_line(aes(y = Av.temp, color = "Actual")) +
  geom_line(aes(y = pred_temp_arima, color = "Predicted"), size = 1) +
  labs(title = "Deseasoned ARIMA Forecast Vs. Validation Data",
       x = "Date", y = "Average Temperature (C)") +
  scale_color_manual(name = "Legend", values = c("Actual" = "red", "Predicted" = "darkgreen"))
theme_minimal()

ggplot(validation_data, aes(x = Date)) +
  geom_line(aes(y = Av.temp, color = "Actual")) +
  geom_line(aes(y = pred_temp_ets, color = "Predicted"), size = 1) +
  labs(title = "Deseasoned ETS Forecast Vs. Validation Data",
       x = "Date", y = "Average Temperature (C)") +
  scale_color_manual(name = "Legend", values = c("Actual" = "red", "Predicted" = "darkgreen"))
theme_minimal()

ggplot(validation_data, aes(x = Date)) +
  geom_line(aes(y = Av.temp, color = "Actual")) +
  geom_line(aes(y = pred_temp_tbats, color = "Predicted"), size = 1) +
  labs(title = "Deseasoned TBATS Forecast Vs. Validation Data",
       x = "Date", y = "Average Temperature (C)") +
  scale_color_manual(name = "Legend", values = c("Actual" = "red", "Predicted" = "darkgreen"))
theme_minimal()

# Forecast 2020 Temperatures Using the TBATS Model

dates_2020 <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")

new_data_2020 <- data.frame(
  Year = as.numeric(format(dates_2020, "%Y")),
  Month = as.numeric(format(dates_2020, "%m")),
  Day = as.numeric(format(dates_2020, "%d"))
)

new_data_2020$Date <- dates_2020

new_data_2020$doy <- yday(new_data_2020$Date)
new_data_2020$time_index <- as.numeric(new_data_2020$Date)
new_data_2020$Date_numeric <- as.numeric(new_data_2020$Date)

new_data_2020$trend <- predict(fit_cubic, newdata = new_data_2020)

new_data_2020 <- new_data_2020 %>% left_join(daily_median, by = "doy")

h_2020 <- nrow(new_data_2020)

forecast_tbats_2020 <- forecast(fit_tbats_deseason, h = h_2020)

forecasted_resid_tbats_2020 <- as.numeric(forecast_tbats_2020$mean)

new_data_2020$Predicted_AvTemp <- new_data_2020$trend + new_data_2020$median_resid + forecasted_resid_tbats_2020

ggplot(new_data_2020, aes(x = Date, y = Predicted_AvTemp)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Predicted Daily Average Temperature for 2020 (TBATS on Detrended Data)",
       x = "Date",
       y = "Average Temperature (C)") +
  theme_minimal()

output <- new_data_2020 %>% select(Date, Predicted_AvTemp)
write.csv(output, file = "tbats_predicted_av_temp_2020.csv", row.names = FALSE)


dates_2020 <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")

new_data_2020 <- data.frame(
  Year = as.numeric(format(dates_2020, "%Y")),
  Month = as.numeric(format(dates_2020, "%m")),
  Day = as.numeric(format(dates_2020, "%d"))
)

new_data_2020$Date <- dates_2020

new_data_2020$doy <- yday(new_data_2020$Date)
new_data_2020$time_index <- as.numeric(new_data_2020$Date)
new_data_2020$Date_numeric <- as.numeric(new_data_2020$Date)

new_data_2020$trend <- predict(fit_cubic, newdata = new_data_2020)

new_data_2020 <- new_data_2020 %>% left_join(daily_median, by = "doy")

h_2020 <- nrow(new_data_2020)

forecast_arima_2020 <- forecast(fit_arima_deseason, h = h_2020)

forecasted_resid_arima_2020 <- as.numeric(forecast_arima_2020$mean)

new_data_2020$Predicted_AvTemp <- new_data_2020$trend + new_data_2020$median_resid + forecasted_resid_arima_2020

ggplot(new_data_2020, aes(x = Date, y = Predicted_AvTemp)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Predicted Daily Average Temperature for 2020 (ARIMA on Detrended Data)",
       x = "Date",
       y = "Average Temperature (C)") +
  theme_minimal()

output <- new_data_2020 %>% select(Date, Predicted_AvTemp)
write.csv(output, file = "arima_predicted_av_temp_2020.csv", row.names = FALSE)


