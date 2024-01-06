# Load necessary libraries
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

# Load the dataset
crime_data <- read_csv("crimedata.csv")

# Inspect the dataset structure
head(crime_data)
str(crime_data)
summary(crime_data)
colnames(crime_data)

# Convert date fields to datetime format, ignoring the time part
crime_data$DATE_OCC <- as.Date(crime_data$`DATE OCC`, format="%m/%d/%Y %I:%M:%S %p")

# Check the conversion
head(crime_data$DATE_OCC)

# Aggregating data for daily analysis
crime_data_daily <- crime_data %>%
  group_by(DATE_OCC) %>%
  summarise(Daily_Count = n())

# Check the range of dates in the data
range(crime_data_daily$DATE_OCC)

# Check for missing values in the aggregated data
sum(is.na(crime_data_daily$Daily_Count))

# Convert the daily counts to a time series object with the correct frequency
ts_data <- ts(crime_data_daily$Daily_Count, frequency = 365)

# Differencing the data to achieve stationarity
diff_data <- diff(ts_data)

# Perform Augmented Dickey-Fuller Test to check stationarity of the differenced data
adf_test_diff <- adf.test(diff_data, alternative = "stationary")

# Output the result of the ADF test
adf_test_diff

# ACF and PACF plots of the differenced data
Acf(diff_data, main="ACF of Differenced Data")
Pacf(diff_data, main="PACF of Differenced Data")

# Fit the ARIMA model using the auto.arima function
arima_model <- auto.arima(ts_data, D=1)

# Summary of the model
summary(arima_model)

# Diagnostic checks on the ARIMA model's residuals
checkresiduals(arima_model)

# Apply a Box-Cox transformation to the time series
bc_transformed <- BoxCox(ts_data, lambda = BoxCox.lambda(ts_data))

# Fit an ARIMA model to the Box-Cox transformed data
arima_bc_model <- auto.arima(bc_transformed)

# Summary of the new ARIMA model
summary(arima_bc_model)

# Diagnostic checks on the new ARIMA model's residuals
checkresiduals(arima_bc_model)

# Forecast the next 30 days based on the Box-Cox transformed ARIMA model
forecast_length <- 30  # Adjust this to change the forecast horizon
future_forecast <- forecast(arima_bc_model, h=forecast_length)

# Plot the forecast
plot(future_forecast)

# Apply the inverse Box-Cox transformation to the forecasted values
inverse_forecasts <- InvBoxCox(future_forecast$mean, BoxCox.lambda(ts_data))

# Create a time series object of the forecasts on the original scale
original_scale_forecasts <- ts(inverse_forecasts, start=c(2021, 1), frequency=365)

# Plot the forecasts on the original scale
plot(original_scale_forecasts, type='o', col='blue', xlab='Time', ylab='Crime Counts', main='Forecasted Crime Counts (Original Scale)')

