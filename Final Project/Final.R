# Clear the environment
rm(list = ls()) 

getwd()

install.packages("readxl", dependencies = TRUE)
library(readxl)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)

#Look for packages 
search()

KenyaData <- read_excel("Kenya_MFI.xlsx")
# Rename all columns at once
colnames(KenyaData) <- c("MFI", "Year", "Period", "Date", "GrossLoan", 
                         "Cash", "NetLoan", "OtherAssets", "LiabilitiesEquity", 
                         "NetFixedAssets", "TotalAssets", "AvgAssets")


# Convert Fiscal Year to numeric
KenyaData$Year <- as.numeric(KenyaData$Year)


# Ensure data is sorted by year
KenyaData <- KenyaData[order(KenyaData$Year), ]

# Data Variables ----

#Dependent Variable (Target Variable):
#Gross Loan Portfolio → Represents the total value of loans issued by the MFI.

#Independent Variables (Predictor Variables):
#Net Loan Portfolio → Measures the total loan value after deductions like write-offs.
#Total Assets → Represents the overall financial resources of the MFI.
#Number of Active Borrowers → Indicates the total clients currently holding loans.
#Cash and Cash Equivalents → Represents the liquid assets available to the MFI.


# Descriptive Stats----
summary(KenyaData)


# Regression Analysis ---- 

# Single predictor regression
model1 <- lm(GrossLoan ~ NetLoan, data = KenyaData)
summary(model1)  # Check coefficients, R-squared, p-values

# Multiple regression including key financial indicators
model2 <- lm(GrossLoan ~ NetLoan + TotalAssets + LiabilitiesEquity + Cash, data = KenyaData)
summary(model2)  # Check model significance

# Check multicollinearity (Variance Inflation Factor)
library(car)
vif(model2)


# Times Series Analysis and Forecasting ---- 
# 1. Regression-Based Forecasting

# Load required packages

install.packages("caret")
library(caret)

# Build multiple regression model
model_reg <- lm(GrossLoan ~ NetLoan + TotalAssets + LiabilitiesEquity, data = KenyaData)

# Model summary
summary(model_reg)

# Create a new dataset for prediction (replace with actual projected values)
future_data <- data.frame(
  NetLoan = c(5000000, 5200000, 5400000), 
  TotalAssets = c(10000000, 10500000, 11000000), 
  LiabilitiesEquity = c(8000000, 8500000, 9000000)
)

# Predict future Gross Loan Portfolio
future_predictions <- predict(model_reg, newdata = future_data)
print(future_predictions)




# Model Validation and Interpretation ----

# Check residuals of time series model
checkresiduals(auto_arima_model)

# Calculate RMSE for model evaluation
rmse <- sqrt(mean(model2$residuals^2))
print(paste("RMSE:", rmse))

