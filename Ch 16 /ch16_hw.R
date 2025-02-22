# Clear the environment
rm(list = ls()) 

library(tidyverse) 
library(dplyr)
library(ggplot2)
library(car)# For Durbin-Watson test
getwd()

#Start with the linear model as a baseline.
#Use the quadratic model if the relationship is curved.
#Use the logarithmic model if Y increases quickly at low X but levels off at higher X

# Ch16-Q1 ---- 
x <- c(22, 24, 26, 30, 35, 40)
y <- c(12, 21, 33, 35, 40, 36)

#part a: simple linear regression
lm1 <- lm(y ~ x)
summary(lm1)  

#part b hypothesis test 
# since our p value 0.0589 is slight greater than 0.05, we cannot reject null 

#part c: scatter plot 
plot(x, y, main="Scatter Plot of x and y", xlab="x", ylab="y", pch=16, col="blue")
abline(lm1, col="red")  # regression line
# a quadratic term would improve the fit 

#part d: Quadratic regression

x2 <- x^2
lm2 <- lm(y ~ x + x2)
summary(lm2)

# part e: hypothesis test for quadratic relationship 
# since p values for x = 0.0196 and x^2 = 0.0258 and are less than 0.05 
# there is a significant relationship.  

#part f: predict when x=25 
# Predict with quadratic model
new_data <- data.frame(x = 25, x2 = 25^2)
predict(lm2, new_data)


# Ch16-Q2 ---- 

x <- c(9, 32, 18, 15, 26)
y <- c(10, 20, 21, 16, 22)

# part a: simple linear regression 
lm1 <- lm(y ~ x)
summary(lm1)
# 61.36% of variability in y is explained by linear model. The slope 
# is not statistically significant as p value is 0.115 > 0.05. Suggesting   
# linear relationship may be weak/other influences. 


# part b: quadratic regression 
x2 <- x^2
lm2 <- lm(y ~ x + x2)
summary(lm2)
# r squared value means 96% of variability can be explained. p value of 0.03 suggests 
# model is statistically significant. 

# part c : predict y when x = 20 
new_data <- data.frame(x = 20, x2 = 20^2)
predict(lm2, new_data)
# 20.96585 answer 



 
# Ch16-Q3 ---- 
x <- c(2, 3, 4, 5, 7, 7, 7, 8, 9)
y <- c(4, 5, 4, 6, 4, 6, 9, 5, 11)

#part a: Linear relationship 
# Scatter plot
plot(x, y, main="Scatter Plot of x and y", xlab="x", ylab="y", pch=16, col="blue")

# Correlation coefficient
correlation <- cor(x, y)
correlation
# there appears to be a 62% correlation for the relationship. This is non linear 

#part b: linear regression 
# Simple linear regression
lm1 <- lm(y ~ x)
summary(lm1) 
# r squared = 38.46% and p value = 0.07479
# y = 2.32 + 0.6366x 

# part c: plot standardized residuals vs predicted values 
# Standardized residuals
standardized_residuals <- rstandard(lm1)
predicted_values <- fitted(lm1)

# Residual plot
plot(predicted_values, standardized_residuals, main="Standardized Residuals vs Predicted Values", 
     xlab="Predicted Values", ylab="Standardized Residuals", pch=16, col="blue")
abline(h=0, col="red")
# Random scatter, assumptions are satisfied. 

# part d: logarithmic transformation of dependent variable (log(y))
# Logarithmic transformation
log_y <- log(y)

# Regression with log-transformed y
lm2 <- lm(log_y ~ x)
summary(lm2)
# r squared = 38.58% and p value = 0.07421 

# Reciprocal transformation
reciprocal_y <- 1/y

# Regression with reciprocal y
lm3 <- lm(reciprocal_y ~ x)
summary(lm3)
# r squared = 37.42% and p value = 0.08003


 
# Ch16-Q6 ----
facilities <- c(9, 11, 16, 21, 27, 30)
distance <- c(1.66, 1.12, 0.83, 0.62, 0.51, 0.47) 

# part a: Scatter Plot 
plot(facilities, distance, main="Scatter Plot: Facilities vs Distance",
     xlab="Number of Facilities", ylab="Average Distance (miles)", pch=16, col="blue")

#Plot shows a downwards linear relationship 

# Part b: Linear regression 
lm1 <- lm(distance ~ facilities)
summary(lm1)
# R squared = 81.88% of variability accounted for and a p value of 0.01314. 
# Simple linear regression is adequate to show relation. 

# Part c: alt regression models
# Reciprocal transformation
lm2 <- lm(distance ~ I(1/facilities))
summary(lm2)
#higher r square value: 0.9669 and lower p value: 0.0004145

# Logarithmic transformation
lm3 <- lm(distance ~ log(facilities))
summary(lm3)
#higher r square value: 0.9095 and lower p value: 0.003172

 
# Ch16-Q8 ----
classic <- read.csv("CLassicCars - Data.csv", header = TRUE)
colnames(classic) <- c( "Year", "Make", "Model", "Rating", "Price") 

#Part a: Scatter plot
ggplot(classic, aes(x = Rating, y = Price)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot: Rating vs Price",
       x = "Rarity Rating",
       y = "Price ($1000)") +
  theme_minimal()
# The plot shows an upwards trend. 

#part b: linear regression 
# Simple linear regression
lm1 <- lm(Price ~ Rating, data = classic)
summary(lm1)
# Small r square: 0.4836 and low p value: 0.003997   

# Scatter plot with regression line
ggplot(classic, aes(x = Rating, y = Price)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Linear Regression: Rating vs Price",
       x = "Rarity Rating",
       y = "Price ($1000)") +
  theme_minimal()
# Bad line fit. 

#Part c: Multiple regression with Quadratic

# quadratic term
classic <- classic %>% mutate(Rating_squared = Rating^2)

# Multiple regression
lm2 <- lm(Price ~ Rating + Rating_squared, data = classic)
summary(lm2)
#R square is higher at 70.23% of data accounted. p value also smaller at 0.0006962

#Part C: Log transformation
# Logarithmic transformation of Price
classic <- classic %>% mutate(Log_Price = log(Price))

# Linear regression with log-transformed Price
lm3 <- lm(Log_Price ~ Rating, data = classic)
summary(lm3)
# Higher r square at 80.54% thus better model. 

#Part D: model comp
cat("Linear Model R-squared: ", summary(lm1)$r.squared, "\n")
cat("Quadratic Model R-squared: ", summary(lm2)$r.squared, "\n")
cat("Logarithmic Model R-squared: ", summary(lm3)$r.squared, "\n")
#Between the three different models, logarithmic model is best fit. 
 
# Ch16-Q10 ----
n <- 27  # Number of observations
SST_simple <- 1550
SSE_simple <- 520
SSE_full <- 100
alpha <- 0.05

#Part a: Test x1 significance 
# Coefficient for x1 in simple regression
b1 <- 5.5
df_simple <- n - 2

#standard error of b1
SE_b1 <- sqrt(SSE_simple / df_simple)

# t-statistic
t_value <- b1 / SE_b1
t_value

# Critical value for two-tailed t-test at alpha = 0.05
t_critical <- qt(1 - alpha/2, df_simple)
t_critical

# Decision
if (abs(t_value) > t_critical) {
  cat("Reject H0: x1 is significant at alpha = 0.05\n")
} else {
  cat("Fail to reject H0: x1 is NOT significant at alpha = 0.05\n")
}

#Part b: f test for x2 and x3 significance
# Given values the full model
p <- 4  # Intercept + x1 + x2 + x3
df_full <- n - p

# Calculate F-statistic
F_value <- ((SSE_simple - SSE_full) / (p - 1)) / (SSE_full / df_full)
F_value

# Critical value for F-test
F_critical <- qf(1 - alpha, df1 = p - 1, df2 = df_full)
F_critical

# Decision
if (F_value > F_critical) {
  cat("Reject H0: x2 and x3 contribute significantly to the model\n")
} else {
  cat("Fail to reject H0: x2 and x3 do NOT contribute significantly to the model\n")
}

 
# Ch16-Q11 ---- 
# Given data
n <- 30  # Number of observations
SST <- 1805
SSR_full <- 1760
SSR_reduced <- 1705
alpha <- 0.05 

#Part a: overall significance of full model 
# Full model parameters
p_full <- 5  # Intercept + x1 + x2 + x3 + x4
df_full_1 <- p_full - 1
df_full_2 <- n - p_full

# SSE for full model
SSE_full <- SST - SSR_full

# Calculate F-statistic
F_value_full <- (SSR_full / df_full_1) / (SSE_full / df_full_2)
F_value_full

# Critical value for F-test
F_critical_full <- qf(1 - alpha, df1 = df_full_1, df2 = df_full_2)
F_critical_full

# Decision
if (F_value_full > F_critical_full) {
  cat("Reject H0: The overall model is significant.\n")
} else {
  cat("Fail to reject H0: The overall model is NOT significant.\n")
}

#Part b: SSE for full model 
SSE_full <- SST - SSR_full
SSE_full

#Part c: SSE for reduced model 
# SSE for reduced model
SSE_reduced <- SST - SSR_reduced
SSE_reduced

#Part d: f test for significance of x1 and x4 
# Reduced model parameters
p_reduced <- 3  # Intercept + x2 + x3
df_reduced_1 <- p_full - p_reduced
df_reduced_2 <- n - p_full

# F-statistic
F_value_partial <- ((SSE_reduced - SSE_full) / df_reduced_1) / (SSE_full / df_reduced_2)
F_value_partial

# Critical value for F-test
F_critical_partial <- qf(1 - alpha, df1 = df_reduced_1, df2 = df_reduced_2)
F_critical_partial

# Decision
if (F_value_partial > F_critical_partial) {
  cat("Reject H0: x1 and x4 contribute significantly to the model.\n")
} else {
  cat("Fail to reject H0: x1 and x4 do NOT contribute significantly to the model.\n")
}


 
# Ch16-Q15 ---- 
gas_data <- read.csv("NaturalGas - Data.csv", header = TRUE)
colnames(gas_data) <- c( "Average_Bill", "Age", "SqFt","Rooms")

#Part a: Linear regression Bill vs Age
# Simple linear regression
lm1 <- lm(Average_Bill ~ Age, data = gas_data)
summary(lm1)

# Scatter plot with regression line
ggplot(gas_data, aes(x = Age, y = Average_Bill)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Simple Linear Regression: Age vs Gas Bill",
       x = "Age of Home",
       y = "Average Monthly Gas Bill ($)") +
  theme_minimal()
#Very small r square of 0.00274 and very large p value of 0.8265. The model is a bad fit. 

#Part b: Multiple regression Bill vs Age, Sqft, number of rooms
lm2 <- lm(Average_Bill ~ Age + SqFt + Rooms, data = gas_data)
summary(lm2)
#R square improved at 0.8635 and very small p value. The variables are statistically significant. 

#Part c: F test for Sqft and Number of rooms 
# Reduced model: Only Age
lm_reduced <- lm(Average_Bill ~ Age, data = gas_data)

# Compare full and reduced models using an F-test
anova_result <- anova(lm_reduced, lm2)
anova_result
# Since we have very small p value, we reject null and conclude that the Sqft and number of rooms are 
# statistically significant. 
cat("Simple Linear Model R-squared:", summary(lm1)$r.squared, "\n")
cat("Multiple Linear Model R-squared:", summary(lm2)$r.squared, "\n")




 
# Ch16-Q23 ---- 

manufacturer <- c(rep("Manufacturer 1", 4), rep("Manufacturer 2", 4), rep("Manufacturer 3", 4))
time <- c(20, 26, 24, 22, 28, 26, 31, 27, 20, 19, 23, 22)
data <- data.frame(manufacturer, time)

#Part A: Multiple Regression 
# Convert manufacturer to a factor variable
data$manufacturer <- as.factor(data$manufacturer)

# Regression model
lm_model <- lm(time ~ manufacturer, data = data)
summary(lm_model)
# y = 23 + 5x -2x 

#Part b: Coefficient Est
# 23.00 is the mean time for Manufacturer 1 
# Manufacturer 2: 5.00 additional time compared to Man1 
# Man3: -2.0 reduction in time compared to Man1 

#Part c: Hypothesis for Comparing Mean 
# To determine if mean times are the same for all three manufacturers we test: 
# H0 = m1=m2=m3
# H1: At least one manufactures mean time is different from the others. 

# Part d: ANOVA test 
# Perform ANOVA
anova_model <- aov(time ~ manufacturer, data = data)
summary(anova_model)
#Since p value 0.00426 is less than 0.05, we reject null and conclude at least one maufacturers 
# mean time is different. 
 
# Ch16-Q25 ---- 

car_size <- c("Compact", "Intermediate", "FullSize", 
              "Compact", "Intermediate", "FullSize")
analyzer <- c("Computerized", "Computerized", "Computerized", 
              "Electronic", "Electronic", "Electronic")
time <- c(50, 55, 63, 42, 44, 46)

data <- data.frame(car_size, analyzer, time)
summary(data)

#ANOVA two way 
# Convert variables to factors
data$analyzer <- as.factor(data$analyzer)
data$car_size <- as.factor(data$car_size)

# Two-Way ANOVA
anova_model <- aov(time ~ analyzer + car_size + analyzer:car_size, data = data)
summary(anova_model)
#Analyzer has large mean square (216.0) suggesting it likely explains a significant portion of 
# variability in tune up times. 

#Car size factor has mean square of 36.5 indicating that car size also contributes to difference 
# in tune up times 

#Interaction of Analyzer and Car Size has smaller mean square of 10.5 suggesting the diffence in tune
# up times between analyzers does not heavily depend on car size. 

 
# Ch16-Q28 ----

cravens_data <- read.csv("Cravens - Sheet1.csv", header = TRUE)

# Multiple linear regression
lm_model <- lm(Sales ~ Accounts + AdvExp + Poten + Share, data = cravens_data)
summary(lm_model)

# Durbin-Watson test
dw_test <- durbinWatsonTest(lm_model)
dw_test
#Durbin-Watson stat is 1.596 which is slightly below 2, suggesting a small degree of positive 
# autocorrelation. 
#However, since p value is 0.176, which is greater than 0.05, we do not have enough evidence to 
# conclude that positive autocorrelation is statistically significant. 


 
# Ch16-Q29 ---- 

bonds_data <- read.csv("CorporateBonds - Data.csv", header = TRUE)
colnames(bonds_data) <- c( "Company", "Years", "Yield")

#part a: scatter diagram: yield vs years to maturity
# Scatter plot
ggplot(bonds_data, aes(x = Years, y = Yield)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot: Years to Maturity vs Yield",
       x = "Years to Maturity",
       y = "Bond Yield (%)") +
  theme_minimal() 

#Part b: Quadratic Regression: Yield vs Years to maturity (and squared) 
# Add a quadratic term
bonds_data <- bonds_data %>% mutate(Year_Squared = Years^2)

# Quadratic regression
lm_quadratic <- lm(Yield ~ Years + Year_Squared, data = bonds_data)
summary(lm_quadratic)

# Plot quadratic regression curve
ggplot(bonds_data, aes(x = Years, y = Yield)) +
  geom_point(color = "blue", size = 3) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  labs(title = "Quadratic Regression: Years to Maturity vs Yield",
       x = "Years to Maturity",
       y = "Bond Yield (%)") +
  theme_minimal()

#R square is 0.6678 which is decent model fit, could be better. 

#Part C: Logarithmic Regression: Yield vs ln(Years) 
# Logarithmic transformation
bonds_data <- bonds_data %>% mutate(Log_Years = log(Years))

# Logarithmic regression
lm_log <- lm(Yield ~ Log_Years, data = bonds_data)
summary(lm_log)

# Plot logarithmic regression
ggplot(bonds_data, aes(x = Log_Years, y = Yield)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Logarithmic Regression: ln(Years to Maturity) vs Yield",
       x = "ln(Years to Maturity)",
       y = "Bond Yield (%)") +
  theme_minimal()
#Slight improvement to r square of 0.6695. P value is very small as well. 

cat("Linear Model R-squared:", summary(lm(Yield ~ Years, data = bonds_data))$r.squared, "\n")
cat("Quadratic Model R-squared:", summary(lm_quadratic)$r.squared, "\n")
cat("Logarithmic Model R-squared:", summary(lm_log)$r.squared, "\n")

#Best model is logarithmic since its R square at 66.95% is the highest and has best fit. 
# The logarithmic transformation better captures the diminishing effect of Years to Maturity on Yield. 

# Ch16-Q32 ---- 

# Read the dataset
audit_data <- read.csv("Audit - Data.csv", header = TRUE)

#Part a: Multiple Regression 
lm_full <- lm(Delay ~ Industry + Public + Quality + Finished, data = audit_data)
summary(lm_full)
# R square 0.3826 and p-value 0.001666

#Part b: Fit of regression model 
# Residual plot
plot(fitted(lm_full), resid(lm_full), main="Residuals vs Fitted Values",
     xlab="Fitted Values", ylab="Residuals", pch=16, col="red")
abline(h=0, col="black")

#Low r square value. 

#Part c: Scatter Diagram: Delay vs Finished 
# Scatter plot of Delay vs Finished
ggplot(audit_data, aes(x = Finished, y = Delay)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot: Delay vs Finished",
       x = "Finished (1 = All work after year-end, 4 = Most work before year-end)",
       y = "Audit Delay (Days)") +
  theme_minimal()

# Part d: Delay vs Finished, Alt Model 
# Simple linear regression with Finished as the predictor
lm_finished <- lm(Delay ~ Finished, data = audit_data)
summary(lm_finished) 
#Even smaller r square at 0.09931. 

cat("Full Model R-squared:", summary(lm_full)$r.squared, "\n")
cat("Finished-Only Model R-squared:", summary(lm_finished)$r.squared, "\n")


 
# Ch16-Q34 ----

audit_data <- read.csv("Audit - Data.csv", header = TRUE)

#Part a: Multiple regression: Predict Delay using Industry and Quality 

lm_model <- lm(Delay ~ Industry + Quality, data = audit_data)
summary(lm_model)
#R square of 0.2689 and p value of 0.003048 

#Part b: Residual Plot: Residuals vs Order of Data 
# residuals
residuals <- resid(lm_model)

#sequence for plotting
order <- 1:length(residuals)

# Plot residuals vs. order
plot(order, residuals, type = "b", col = "blue", pch = 16,
     main = "Residuals vs Order of Data",
     xlab = "Order of Data", ylab = "Residuals")
abline(h = 0, col = "red")


#Part c: Durbin Watson test for Positive Autocorrelation 
dw_test <- durbinWatsonTest(lm_model)
dw_test
# Our DurbinWatson stat is 1.426 which is less than 2, indicating a positive autocorrelation in residuals. 
# Our p-value is 0.038 which is less than 0.05. We have statistically significant evidence to 
# concluse positive auto correlation. 

 
# Ch16-Q35 ---- 

fuel_data <- read.csv("FuelEconomy2019 - Data.csv", header = TRUE)
colnames(fuel_data) <- c( "Make", "Model", "Class", "Combined_MPG")
# Summary statistics for each class
fuel_data %>%
  group_by(Class) %>%
  summarise(Mean_Combined_MPG = mean(Combined_MPG, na.rm = TRUE),
            SD_Combined_MPG = sd(Combined_MPG, na.rm = TRUE),
            Count = n())

#ANOVA Test
# Convert Class to a factor variable
fuel_data$Class <- as.factor(fuel_data$Class)

#Result ANOVA test
anova_model <- aov(Combined_MPG ~ Class, data = fuel_data)
summary(anova_model)
# At 0.05 significance, we conclude there is significant difference in the mean fuel 
# efficiency (Combined MPG) across the different car classes. 

# Case Problem 2: Rating Wines ---- 
# Load required libraries

wine_data <- read.csv("WineRatings - Data.csv", header = TRUE)

# 1) Number of Wines by Rating and Average Price 
# Count the number of wines and average price by Rating
summary_table <- wine_data %>%
  group_by(Rating) %>%
  summarise(
    Count = n(),
    Avg_Price = mean(Price, na.rm = TRUE)
  )

print(summary_table)
#The classic wines have a very high average score compared to the rest, although only a few rated classic. 
# Not that many "good" wines though they are the cheapest average price. 

# 2) Scatter Plot: Price vs Score 

ggplot(wine_data, aes(x = Price, y = Score)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot: Price vs Wine Score",
       x = "Price ($)", y = "Wine Score") +
  theme_minimal()
#Relationship is not linear. Big cluster of wines in the 0-100 dollar range. 

# 3) Simple Linear Regression: Predict Score from Price 
lm_linear <- lm(Score ~ Price, data = wine_data)
summary(lm_linear)

# Plot regression line
ggplot(wine_data, aes(x = Price, y = Score)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Linear Regression: Price vs Wine Score",
       x = "Price ($)", y = "Wine Score") +
  theme_minimal()

# y = 87.763 + 0.027995x 
# R square of 0.4062 and p value of 1.01e-12. Price is statistically significant 
# but linear model is bad fit 

# 4) Quadratic Regression 
# quadratic term
wine_data <- wine_data %>% mutate(Price_Squared = Price^2)

# Quadratic regression
lm_quadratic <- lm(Score ~ Price + Price_Squared, data = wine_data)
summary(lm_quadratic)

# quadratic regression curve
ggplot(wine_data, aes(x = Price, y = Score)) +
  geom_point(color = "blue", size = 3) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  labs(title = "Quadratic Regression: Price vs Wine Score",
       x = "Price ($)", y = "Wine Score") +
  theme_minimal()
# y = 86.17 + 0.0713x - 0.0001133x^2
# R square of 0.5233 and p value of 2.476e-16. Higher r square value in quadratic, more of variables
# accounted for. 

# 5) Comparison 
cat("Linear Model R-squared:", summary(lm_linear)$r.squared, "\n")
cat("Quadratic Model R-squared:", summary(lm_quadratic)$r.squared, "\n")

# Quadratic model has higher r square (52.33%) than linear model (40.62%) which explains more variability 
# in Wine Spectator score. (52.33% of score can be attributed to price) 

# 6) Logarithmic Model: Using Log of Price 
# Add natural logarithm of Price
wine_data <- wine_data %>% mutate(Log_Price = log(Price))

# Logarithmic regression
lm_log <- lm(Score ~ Log_Price, data = wine_data)
summary(lm_log)

# Plot the logarithmic regression line
ggplot(wine_data, aes(x = Log_Price, y = Score)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Logarithmic Regression: ln(Price) vs Wine Score",
       x = "ln(Price)", y = "Wine Score") +
  theme_minimal()

# R square is 0.5758 and small p value < 2.2e-16

# 7) Paying More = Better Wine? 
# Paying more does not necessarily result in a "better" wine. For starters, taste can be subjective
# and not all would agree. Based on the model, price doesnt directly impact rating because there are
# still various factors unaccounted for. 

# 8) Predicting Wine Score for $30 
predict(lm_quadratic, data.frame(Price = 30, Price_Squared = 30^2))
predict(lm_log, data.frame(Log_Price = log(30)))

#Spending $30 for a bottle of wine has predicted score of 88. This would be 
# considered "very good" wine based on the ratings. Two tiers below the top spot. 




