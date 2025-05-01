# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)
library(Metrics)
library(randomForest)
library(caret)
library(tensorflow)
library(keras)
library(ggplot2)
library(reticulate)

# Set virtual environment
#use_virtualenv("C:/venvs/r-tensorflow", required = TRUE)

# Set seed

set.seed(123)
tensorflow::set_random_seed(123)

# Load in cleaned data

url <- "https://raw.githubusercontent.com/harveywcollins/temperature-prediction/refs/heads/main/data/cleaned_temperature_data.csv"
temperature_data <- read.csv(url)

# Ensure Data column is in Date format is numeric

temperature_data$Year <- as.numeric(as.character(temperature_data$Year))
temperature_data$Month <- as.numeric(as.character(temperature_data$Month))
temperature_data$Day <- as.numeric(as.character(temperature_data$Day))
temperature_data$Date <- make_date(temperature_data$Year, temperature_data$Month, temperature_data$Day)

temperature_data$doy <- yday(temperature_data$Date)
temperature_data$time_index <- as.numeric(temperature_data$Date)

# Data Partitioning

training_data <- temperature_data %>% filter(Year <= 2015)
validation_data <- temperature_data %>% filter(Year > 2015 & Year <= 2019)

cat("Training rows:", nrow(training_data), "\n")
cat("Validation rows:", nrow(validation_data), "\n")

# Detrending and Deseasoning

training_data$Date_numeric <- as.numeric(training_data$Date)
fit_cubic <- lm(Av.temp ~ poly(Date_numeric, 3), data = training_data)
training_data$trend <- predict(fit_cubic, newdata = training_data)

training_data$resid <- training_data$Av.temp - training_data$trend

daily_median <- training_data %>%
  group_by(doy) %>%
  summarise(median_resid = median(resid, na.rm = TRUE))

training_data <- training_data %>% left_join(daily_median, by = "doy")
training_data$best_resid <- training_data$resid - training_data$median_resid

validation_data$Date_numeric <- as.numeric(validation_data$Date)
validation_data$trend <- predict(fit_cubic, newdata = validation_data)
validation_data$resid <- validation_data$Av.temp - validation_data$trend
validation_data <- validation_data %>% left_join(daily_median, by = "doy")
validation_data$best_resid <- validation_data$resid - validation_data$median_resid

ggplot(training_data, aes(x = Date, y = best_resid)) +
  geom_line(color = "purple") +
  labs(title = "Deseasoned Residuals (Training Data)", x = "Date", y = "Residuals (C)") +
  theme_minimal()

# Predictor features and target

features <- c("Year", "Month", "Day", "doy", "time_index")
target <- "Av.temp"

# Prepare training for validation datasets for ML:

x_train <- training_data %>% select(all_of(features))
y_train <- training_data$best_resid

x_val <- validation_data %>% select(all_of(features))
y_val <- validation_data$best_resid

# Scaling the data

scale_params <- preProcess(x_train, method = c("center", "scale"))
x_train_scaled <- predict(scale_params, x_train)
x_val_scaled <- predict(scale_params, x_val)

# Random Forest Model

rf_model <- randomForest(x = x_train_scaled, y = y_train, ntree = 500)
rf_predictions <- predict(rf_model, newdata = x_val_scaled)

rf_rmse <- rmse(y_val, rf_predictions)
rf_mae <- mae(y_val, rf_predictions)

cat("Baseline Random Forest RMSE:", rf_rmse, "\n")
cat("Baseline Random Forest MAE:", rf_mae, "\n")

{
  repeat {
    ans <- tolower(readline("Run hyperparameter tuning (long run-time)? (y/n): "))
    if (ans %in% c("y", "n")) break
    cat("Please enter 'y' or 'n'\n")
  }
  ntree_values <- seq(200, 500, by = 100)

  if (ans == "y") {
    # Tuning mtry with caret 
    rf_grid <- expand.grid(mtry = 1:ncol(x_train_scaled))
    control <- trainControl(method = "cv", number = 5)
    rf_tuned <- caret::train(x = x_train_scaled,
                             y = y_train,
                             method = "rf",
                             tuneGrid = rf_grid,
                             trControl = control,
                             ntree = 400)
    print(rf_tuned)
    plot(rf_tuned)
  
    best_mtry <- rf_tuned$bestTune$mtry
   
  
    ntree_errors <- sapply(ntree_values, function(ntree) {
      model <- randomForest(x = x_train_scaled, y = y_train, ntree = ntree, mtry = best_mtry)
      preds <- predict(model, newdata = x_val_scaled)
      rmse(y_val, preds)
    })
    
    optimal_ntree <- ntree_values[which.min(ntree_errors)]
    cat("Tuned hyperparameters: mtry =", best_mtry,
        " ntree=", optimal_ntree, "\n\n")
    
    final_rf_model <- randomForest(
      x = x_train_scaled, y = y_train,
      mtry = best_mtry,
      ntree = optimal_ntree,
      nodesize = 10
    )
    final_rf_predictions <- predict(final_rf_model, x_val_scaled)
    final_rf_rmse <- rmse(y_val, final_rf_predictions)
    final_rf_mae <- mae(y_val, final_rf_predictions)
  
  } else {
    cat("Skipping tuning, reusing baseline model\n\n")
    final_rf_model <- rf_model
    final_rf_predictions <- rf_predictions
    final_rf_rmse <- rf_rmse
    final_rf_mae <- rf_mae
  }

}

cat("Final Random Forest RMSE:", final_rf_rmse, "\n")
cat("Final Random Forest MAE:", final_rf_mae, "\n")

rf_rmse <- final_rf_rmse
rf_mae <- final_rf_mae


# Preparation for Deep Learning Model (Keras)

tensorflow::set_random_seed(123)

# Convert data to matrices and numpy to arrays

train_x <- as.matrix(x_train_scaled)
validation_x <- as.matrix(x_val_scaled)
train_y <- matrix(as.numeric(y_train), ncol = 1)
validation_y <- matrix(as.numeric(y_val), ncol = 1)

cat("Dimensions of train_x:", dim(train_x), "\n")
cat("Dimensions of train_y:", dim(train_y), "\n")

# Deep Neural Network Model using Keras

l2_reg <- tf$keras$regularizers$l2(1e-3)

input <- layer_input(shape = c(as.integer(ncol(train_x))))
output <- input %>%
  layer_dense(units = 128, activation = "relu", kernel_regularizer = l2_reg) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = l2_reg) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 32, activation = "relu", kernel_regularizer = l2_reg) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = "linear")

model <- keras_model(inputs = input, outputs = output)

# Compile the model

model$compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mae")
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 5)

history <- model$fit(
  x = train_x,
  y = train_y,
  epochs = as.integer(50),
  batch_size = as.integer(95),
  validation_data = list(validation_x, validation_y),
  callbacks = list(early_stop),
  verbose = 1
)

# Predict on validation data
dnn_predictions <- model$predict(validation_x)
dnn_rmse <- rmse(validation_y, dnn_predictions)
dnn_mae <- mae(validation_y, dnn_predictions)

cat("Deep Neural Network RMSE:", dnn_rmse, "\n")
cat("Deep Neural Network MAE:", dnn_mae, "\n")

# Compare Models

results <- data.frame(
  Model = c("Random Forest", "Deep Neural Network"),
  RMSE = c(rf_rmse, dnn_rmse),
  MAE = c(rf_mae, dnn_mae)
)

print(results)

# Predict 2020 Weather with Deep Keras Model

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

new_x <- new_data_2020 %>% select(all_of(features))
new_x_scaled <- predict(scale_params, new_x)
new_x_matrix <- as.matrix(new_x_scaled)

pred_resid_2020 <- model$predict(new_x_matrix)
new_data_2020$Predicted_AvTemp <- as.numeric(pred_resid_2020) + new_data_2020$trend + new_data_2020$median_resid

ggplot(new_data_2020, aes(x = Date, y = Predicted_AvTemp)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Predicted Daily Average Temperature for 2020",
       x = "Date",
       y = "Average Temperature (C)") +
  theme_minimal()


output <- new_data_2020 %>% select(Date, Predicted_AvTemp)
write.csv(output, file = "deep_keras_predicted_av_temp_2020_v2.csv", row.names = FALSE)
