# Temperature Prediction using Classical and Deep Learning Models

This project aims to forecast daily average temperature for the year 2020 using historical data from the Durham University Observatory (1900-2019).
It compares classical time series models (ARIMA, ETS, TBATS) with machine learning approaches, including a deep neural network implemented using Keras (Deep Keras) and a Random Forest model.

## Dependencies

To reproduce the analysis, the following dependencies are required:

### R Packages

- `library(dplyr)`
- `library(ggplot2)`
- `library(lubridate)`
- `library(forecast)`
- `library(Metrics)`
- `library(randomForest)`
- `library(caret)`
- `library(tensorflow)`
- `library(keras)`
- `library(readxl)`
- `library(patchwork)`
- `library(reticulate)`
- `library(grid)`

### Python
- `tensorflow`
- `keras`

To use TensorFlow within R (via Keras), ensure appropriate Python virtual environment is activated using the `reticulate` package.

Steps: Restart R session and run following lines to link R with your virtual environment:

```r
library(reticulate)
# Set the correct virtual environment path e.g.,
use_virtualenv("C:/vens/r-tensorflow", required = TRUE)
```

To install a virtual environment run the following command in bash:
`pip install tensorflow keras`
