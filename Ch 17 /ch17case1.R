rm(list = ls()) 


library(readr)
library(ggplot2)
library(forecast)
library(tseries)


vintage<- read.csv("Vintage - Data.csv", header = TRUE)

#1 ----
# Convert Sales column into a time series
sales_ts <- ts(vintage$Sales, start=c(1), frequency=12)

# Plot time series
autoplot(sales_ts) +
  ggtitle("Monthly Food and Beverage Sales") +
  ylab("Sales ($1000s)") +
  xlab("Month")

#Theres an upwards trend. There suggests to be seasonal effects in the later months. 


#2 ----
# Decompose the time series
decomposed <- decompose(sales_ts)

# Plot decomposition
autoplot(decomposed)

# The restaurant sales are growing over time (positive trend).
# There is strong seasonality, meaning some months consistently perform better than others.
# Random fluctuations (remainder) are relatively small indicating a good model fit.



#3 ----
# Deseasonalized time series
deseasonalized_sales <- sales_ts / decomposed$seasonal

# Plot deseasonalized data
autoplot(deseasonalized_sales) +
  ggtitle("Deseasonalized Sales Data") +
  ylab("Sales ($1000s)") +
  xlab("Month") 

# Seasonality is removed
# Sales still show an upward trend, indicating business growth.
# Some fluctuations remain, likely due to external factors like promotions or economic conditions.

#4 ----
# Forecast using decomposition
forecast_decomposed <- forecast(decomposed, h=12)

# Plot forecast
autoplot(forecast_decomposed) +
  ggtitle("Forecasted Sales for Year 4") +
  ylab("Sales ($1000s)") +
  xlab("Month")



 


# Convert months into factor variables
vintage$Month <- factor(rep(1:12, length.out = length(vintage$Sales)))

# Fit regression model with dummy variables
model <- lm(Sales ~ Month, data = vintage)

# Predict Year 4 sales
new_data <- data.frame(Month = factor(1:12))
predicted_sales <- predict(model, newdata = new_data)

# Print forecasted values
print(predicted_sales)


#6 ----
# Create summary table
summary_table <- data.frame(
  Month = 1:12,
  Forecasted_Sales = predicted_sales
)

# Print the summary table
print(summary_table)
