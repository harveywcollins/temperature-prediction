# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(Metrics)
library(patchwork)
library(grid)

# Download the 2020 data and set correct directory

setwd("C:/Users/Harvey Collins/OneDrive/Documents/MISCADA/Earth & Environmental Sciences/Mini_Project/temperature-prediction/data")
#url <- "https://raw.githubusercontent.com/harveywcollins/temperature-prediction/refs/heads/main/data/weather_durham_2020.xlsx"
#download.file(url, "weather_durham_2020.xlsx", mode = "wb")

weather_durham_2020 <- read_excel("weather_durham_2020.xlsx", skip = 0)

print(names(weather_durham_2020))
head(weather_durham_2020)

names(weather_durham_2020)[1:3] <- c("Day", "MaxTemp", "MinTemp")
weather_durham_2020 <- weather_durham_2020 %>%
  mutate(
    Day = as.numeric(Day),
    MaxTemp = as.numeric(MaxTemp),
    MinTemp = as.numeric(MinTemp),
    Date = as.Date("2020-01-01") + Day - 1,
    AvgTemp = (MaxTemp + MinTemp) / 2
  )

# Read in Predicted Data from files

tbats_pred <- read.csv("tbats_predicted_av_temp_2020.csv")
deep_pred <- read.csv("deep_keras_predicted_av_temp_2020.csv")
arima_pred <- read.csv("arima_predicted_av_temp_2020.csv")

deep_pred_v2 <- read.csv("deep_keras_predicted_av_temp_2020_v2.csv")

deep_pred <- deep_pred_v2

tbats_pred$Date <- as.Date(tbats_pred$Date)
deep_pred$Date <- as.Date(deep_pred$Date)
arima_pred$Date <- as.Date(arima_pred$Date)

#deep_pred_v2$Date <- as.Date(deep_pred_v2$Date)

# Merge observed data with predictions

actual_data <- weather_durham_2020 %>% select(Date, AvgTemp)

tbats_merged <- merge(actual_data, tbats_pred, by = "Date")
deep_merged <- merge(actual_data, deep_pred, by = "Date")
arima_merged <- merge(actual_data, arima_pred, by = "Date")

# Compute RMSE 

tbats_complete <- tbats_merged %>% filter(!is.na(AvgTemp))
rmse_tbats <- rmse(tbats_complete$AvgTemp, tbats_complete$Predicted_AvTemp)
mae_tbats <- mae(tbats_complete$AvgTemp, tbats_complete$Predicted_AvTemp)
cat("TBATS RMSE:", rmse_tbats, "\n")
cat("TBATS MAE:", mae_tbats, "\n")
cat("TBATS Combined", rmse_tbats + mae_tbats, "\n")

deep_complete <- deep_merged %>% filter(!is.na(AvgTemp))
rmse_deep <- rmse(deep_complete$AvgTemp, deep_complete$Predicted_AvTemp)
mae_deep <- mae(deep_complete$AvgTemp, deep_complete$Predicted_AvTemp)
cat("Deep Keras RMSE:", rmse_deep, "\n")
cat("Deep Keras MAE:", mae_deep, "\n")
cat("Deep Keras Combined", rmse_deep + mae_deep, "\n")

arima_complete <- arima_merged %>% filter(!is.na(AvgTemp))
rmse_arima <- rmse(arima_complete$AvgTemp, arima_complete$Predicted_AvTemp)
mae_arima <- mae(arima_complete$AvgTemp, arima_complete$Predicted_AvTemp)
cat("ARIMA RMSE:", rmse_arima, "\n")
cat("ARIMA MAE:", mae_arima, "\n")
cat("ARIMA Combined", rmse_arima + mae_arima, "\n")

# Plotting

cols <- c(
  Actual = "black",
  TBATS = "red",
  "Deep Keras" = "blue",
  ARIMA = "green"
)

common_scale <- scale_color_manual(
  name = NULL,
  values = cols,
  breaks = names(cols)
)


p_tbats <- ggplot() +
  geom_line(data = tbats_complete, aes(x = Date, y = AvgTemp, color = "Actual"), linewidth = 1) +
  geom_line(data = tbats_complete, aes(x = Date, y = Predicted_AvTemp, color = "TBATS"), linewidth = 1) +
  common_scale +
  labs(title = "TBATS Model",
       x = "Date", y = "Avg Temperature (C)") +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
  theme_bw() +
  theme(
        panel.grid.major = element_line(color = "grey80", size = 0.3),
        panel.grid.minor = element_line(color = "grey90", size = 0.2),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = c(0.02, 0.02),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.4, "cm")
  ) +
  annotate(
    "text", x=max(tbats_complete$Date), y=max(tbats_complete$AvgTemp), label="(a)",
    hjust=1.0, vjust=0.5, size=5, fontface="bold"
  )

p_deep <- ggplot() +
  geom_line(data = deep_complete, aes(x = Date, y = AvgTemp, color = "Actual"), linewidth = 1) +
  geom_line(data = deep_complete, aes(x = Date, y = Predicted_AvTemp, color = "Deep Keras"), linewidth = 1) +
  common_scale +
  labs(title = "Deep Keras Model",
       x = "Date", y = NULL) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
  theme_bw() +
  theme(
        panel.grid.major = element_line(color = "grey80", size = 0.3),
        panel.grid.minor = element_line(color = "grey90", size = 0.2),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = c(0.02, 0.02),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.4, "cm")
  ) +
  annotate(
    "text", x=max(deep_complete$Date), y=max(deep_complete$AvgTemp), label="(b)",
    hjust=1.0, vjust=0.5, size=5, fontface="bold"
  )

p_arima <- ggplot() +
  geom_line(data = arima_complete, aes(x = Date, y = AvgTemp, color = "Actual"), linewidth = 1) +
  geom_line(data = arima_complete, aes(x = Date, y = Predicted_AvTemp, color = "ARIMA"), linewidth = 1) +
  common_scale +
  labs(title = "ARIMA Model",
       x = "Date", y = NULL) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
  theme_bw() +
  theme(
        panel.grid.major = element_line(color = "grey80", size = 0.3),
        panel.grid.minor = element_line(color = "grey90", size = 0.2),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = c(0.02, 0.02),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.4, "cm")
  ) +
  annotate(
    "text", x=max(arima_complete$Date), y=max(arima_complete$AvgTemp), label="(c)",
    hjust=1.0, vjust=0.5, size=5, fontface="bold"
  )

final_plot <- p_tbats + p_deep + p_arima +
  plot_layout(
    ncol = 3,
    guides = "keep"
  )
  

final_plot <- final_plot +
  plot_annotation(
    title = "Comparison of Model Predictions vs. Actual Average Temperature (2020)",
    theme = theme(plot.title = element_text(size = 20, hjust = 0.5),
                  plot.subtitle = element_text(size = 14, hjust = 0.5)))
print(final_plot)


