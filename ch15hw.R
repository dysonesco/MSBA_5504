# Clear the environment
rm(list = ls()) 

library(tidyverse) 

#R-squared values show portion of variance explained in model 
#P-value determines if variables are significant 
#R squared adjusted: adjusts for number of predictors

#Working directory setup
setwd("/Users/dyson/Desktop/MSBA_5504/Ch 15/chapter15_hw")
getwd()

# Ch15-Q5 ----
library(tidyverse)

show <- read.csv("Showtime - Data.csv", header = TRUE)
colnames(show) <- c( "gross", "TvAds", "NewsAds")
head(show) 

# part a: Simple Linear Regression 
ShowModel <- lm(gross ~ TvAds, data = show)

# Display Summary
summary(ShowModel)
# weekly gross revenue increases $1603 per $1000 spent on TV ads


# part b: Multiple Linear Regression 
ShowModel2 <- lm(gross ~ TvAds + NewsAds, data = show)

# Display Summary
summary(ShowModel2)
# weekly gross revenue increases to $2,290 per $1000 spent on TV ads and Newspaper Ads

# part c: Coefficients
coef(ShowModel) # From Simple Regression
coef(ShowModel2) # From Multiple Regression 
#Newspaper ads influences impact of television ads 

# part d: Predict Gross Revenue 

new_data <- data.frame(TvAds = 3.5, NewsAds = 2.3)
predicted_revenue <- predict(ShowModel2, new_data)
predicted_revenue
# Predicted revenue of $94,238 

# Ch15-Q8 ---- 
TheShips <- read.csv("Ships - Data.csv", header = TRUE)
colnames(TheShips) <- c( "ship", "Overall", "Excursions","Food_Dining")

#part a: Simple Linear regression 

ShipsModel <- lm(Overall ~ Excursions, data = TheShips)

# Display Summary
summary(ShipsModel)

# part b: Multiple Linear Regression 
ShipsModel2 <- lm(Overall ~ Excursions + Food_Dining, data = TheShips)

# Display Summary
summary(ShipsModel2) 

coef(ShipsModel) # From Simple Regression
summary(ShipsModel)$r.squared

coef(ShipsModel2) # From Multiple Regression 
summary(ShipsModel2)$r.squared

#part c: Prediction Overall Score 

new_ships <- data.frame(Excursions = 80, Food_Dining = 90)
predicted_score <- predict(ShipsModel2, new_ships)
predicted_score
# predicted score of 87.75

 
# Ch15-Q12 ---- 
# Given values
SST <- 15182.9
SSR <- 14052.2
n <- 10  # Number of observations
p <- 2   # Number of independent variables

# part a: R-squared
R2 <- SSR / SST

# Compute SSE (Sum of Squared Errors)
SSE <- SST - SSR

# part b: Adjusted R-squared
R2_adj <- 1 - ((SSE / (n - p - 1)) / (SST / (n - 1)))

#part c: Interpretation
# results
R2
R2_adj 
# Since r-square is close to 1, the regression equation can explain most of variability in y
# suggesting strong model. 
# Since adjusted-r-square is close to r-square, the the model doesnt have excessive 
# predictors that dont contribute. 
  
# Ch15-Q18 ----
 
pitching <- read.csv("PitchingMLB - Data.csv", header = TRUE)

# part a: R-square for Runs Given Up per Inning

PitchModel <- lm(R.IP ~ SO.IP + HR.IP, data = pitching)
summary(PitchModel) 

# part b: 
#Since r-squared is relatively low at 56.35%, SO/IP and HR/IP 
# do not strongly predict R/IP. 

# part c: 
# Regression with ERA as dependent variable
PitchModel2 <- lm(ERA ~ SO.IP + HR.IP, data = pitching)
summary(PitchModel2)
#Since we have a higher r-squared value here, this suggests ERA 
# is better predicted by SO/IP and HR/IP than R/IP alone. 


 
# Ch15-Q23 ---- 
show <- read.csv("Showtime - Data.csv", header = TRUE)
colnames(show) <- c( "gross", "TvAds", "NewsAds")

# multiple regression model
ShowsModel <- lm(gross ~ TvAds + NewsAds, data = show)
summary(ShowsModel)

# part a: Hypothesis test (F-test) 
anova(ShowsModel)
# since p-value <0.01, we reject null and this means at least one predictor significant

# part b: Test Tv Ads
summary(ShowsModel)$coefficients
# Since p value of TvAds 0.000653 < 0.05, we reject null hypothesis. 
# TvAds are significant. 

# part c: Test significance of Newspaper Ads 
summary(ShowsModel)$coefficients 
#Since p value for NewsAds is also less than 0.05, it is significant and we keep it. 







 
# Ch15-Q26 ---- 

pitching <- read.csv("PitchingMLB - Data.csv", header = TRUE)

# multiple regression model
MLBmodel <- lm(R.IP ~ SO.IP + HR.IP, data = pitching)
summary(MLBmodel)

# part a : F-test overall model significance 
anova(MLBmodel)
# Since p-value is less than 0.05, we reject null and model is statistically significant 

# part b: t-test for Individual Variable 
summary(MLBmodel)$coefficients
# Since p value is less than 0.05 for both S0.IP and HR.IP, these variables are significant 



 
# Ch15-Q31 ---- 
resale <- read.csv("AutoResale - Data.csv", header = TRUE) 

# multiple regression model
ResaleModel <- lm(Price ~ Mileage + Age, data = resale)
summary(ResaleModel)

# part a: estimate price for four year old car with 40k miles 
# new data frame for prediction
est_car <- data.frame(Mileage = 40000, Age = 4)

# Predicted selling price
predicted_price <- predict(ResaleModel, est_car)
predicted_price
# predicted price of $16,144 

# part b: 95% confidence interval for selling price 
conf_interval <- predict(ResaleModel, est_car, interval = "confidence", level = 0.95)
conf_interval
# lower bound = $15,829 
# upper bound = $16,459 
# range for expected mean price  

# part c: 95% prediction interval for individual car 
pred_interval <- predict(ResaleModel, est_car, interval = "prediction", level = 0.95)
pred_interval 
# range where a specific car's price is expected to fall. This interval is larger because 
# it accounts for individual variations. 





 
# Ch15-Q36 ---- 
 
repair <- read.csv("Repair - Data.csv", header = TRUE)
colnames(repair) <- c("Repair_Time", "Months_Since_Service", "Repair_Type", "Repairperson")

# categorical variables to factors
repair$Repair_Type <- as.factor(repair$Repair_Type)
repair$Repairperson <- as.factor(repair$Repairperson) 

# part a : regression model 
# multiple regression model
RepairModel <- lm(Repair_Time ~ Months_Since_Service + Repair_Type + Repairperson, data = repair)
summary(RepairModel)

# part b: overall model significance using f-test 
anova(RepairModel)
# Months since service and repair type are significant. 
# Repair person is not significant at p-value 0.167 which is greater than 0.05. 

# part c: test significance of repair person variable 
summary(RepairModel)$coefficients
# again, repair person does not significantly contribute to repair time 



 
# Ch15-Q38 ---- 
stroke <- read.csv("Stroke - Data.csv", header = TRUE)

# part a: regression model 
# multiple regression model
StrokeModel <- lm(Risk ~ Age + Pressure + Smoker, data = stroke)
summary(StrokeModel) 

# part b: test smoking significance 
summary(StrokeModel)$coefficients
# smoking p value is less than 0.05, it is significant on stroke risk 

# part c: Predict Stroke Prob for 68 yr old smoker with BP 175 
new_patient <- data.frame(Age = 68, Pressure = 175, Smoker = "Yes")

# Predict stroke probability
predicted_risk <- predict(StrokeModel, new_patient)
predicted_risk
# For this patient there is a 34.26% chance of stroke with their current lifestyle.
# May be time to switch habits due to probability. No one wants a 1/3 chance of a stroke. 



 
# Ch15-Q41 ---- 

show <- read.csv("Showtime - Data.csv", header = TRUE)
colnames(show) <- c( "gross", "TvAds", "NewsAds")

# part a: multiple regression model 
ShowModel <- lm(gross ~ TvAds + NewsAds, data = show)
summary(ShowModel)

# part b: Plot standardized residuals 
# Calculate standardized residuals
show$residuals <- rstandard(ShowModel)

# Plot residuals
plot(fitted(ShowModel), show$residuals, main="Standardized Residuals vs. Fitted Values",
     xlab="Fitted Values", ylab="Standardized Residuals")
abline(h=0, col="red", lwd=2) 
# random scatter

# part c: outliers 
outliers <- which(abs(show$residuals) > 2)
show[outliers, ]
# no outliers 

# part d: Cooks distance (textbook) 
# Cook's Distance
cooksd <- cooks.distance(ShowModel)

# Plot Cook's Distance
plot(cooksd, type="h", main="Cook's Distance", xlab="Observation Index", ylab="Cook's Distance")
abline(h=4/length(show$gross), col="red")

# influential observations
influential <- which(cooksd > (4/length(show$gross)))
show[influential, ]
# influential points exist at x index 1,2 and 7. They significantly impact regression model.
# Should be looked for validity.  



 
# Ch15-Q42 ---- 

auto2 <- read.csv("Auto2 - Data.csv", header = TRUE)
colnames(auto2) <- c( "Car", "Price", "Weight", "Horsepower", "Speed") 

#part a: regression model 
# multiple regression model 
AutoModel <- lm(Speed ~ Price + Horsepower + Weight, data = auto2)
summary(AutoModel) 

#part b: plot standardized residuals 
# standardized residuals
auto2$residuals <- rstandard(AutoModel)

# Plot residuals
plot(fitted(AutoModel), auto2$residuals, main="Standardized Residuals vs. Fitted Values",
     xlab="Fitted Values", ylab="Standardized Residuals")
abline(h=0, col="red", lwd=2)

# part c: outliers 
# Identify potential outliers
outliers <- which(abs(auto2$residuals) > 2)
auto2[outliers, ] 
# instance of 11 of x (mitsubishi) is an outlier 

#part d: influential observations 
# Cook's Distance
cooksd <-cooks.distance(AutoModel)

# Plot Cook's Distance
plot(cooksd, type="h", main="Cook's Distance", xlab="Observation Index", ylab="Cook's Distance")
abline(h=4/length(auto2$Speed), col="red")

# Identify influential observations
influential <- which(cooksd > (4/length(auto2$Speed)))
auto2[influential, ]
# Two influential points. Acura NSX and Mitsubishi 3000GT. 




 
# Ch15-Q47 ----  

lakeland <- read.csv("Lakeland - Data.csv", header = TRUE)

#part a : # Fit logistic regression model
LakeModel <- glm(Return ~ GPA + Program, data = lakeland, family = binomial)
summary(LakeModel)

# part b: interpretation 
# Extract coefficients
beta_0 <- coef(LakeModel)[1]
beta_1 <- coef(LakeModel)[2]

# Logit equation when Program = 0
logit_no_program <- beta_0 + beta_1 * mean(lakeland$GPA)

# Convert logit to probability
probability_no_program <- exp(logit_no_program) / (1 + exp(logit_no_program))
probability_no_program

# part d: significance 

anova(LakeModel, test="Chisq") 

# part e: significance of individual variable 
summary(LakeModel)$coefficients 

# part f: Probability for a Student with GPA = 2.5 
# New student data
new_data <- data.frame(GPA = 2.5, Program = c(0,1))

# Predict probabilities
predicted_probabilities <- predict(LakeModel, new_data, type = "response")
predicted_probabilities

#part g:  Odds Ratio for Program
exp(coef(LakeModel)["Program"])



# Ch15 Case Problem 2: Nascar ----

nascar <- read.csv("Nascar - Data.csv", header = TRUE)
colnames(nascar) <- c( "Drivers", "Points", "Poles", "Wins", "Top5", "Top10","Winnings")
nascar$Winnings <- as.numeric(gsub(",", "", nascar$Winnings)) #remove commas in winngings 


# part 1 
#  individual regression models for each predictor
model_points <- lm(Winnings ~ Points, data = nascar)
model_poles <- lm(Winnings ~ Poles, data = nascar)
model_wins <- lm(Winnings ~ Wins, data = nascar)
model_top5 <- lm(Winnings ~ Top5, data = nascar)
model_top10 <- lm(Winnings ~ Top10, data = nascar)

#  R-squared values for comparison
r2_values <- c(summary(model_points)$r.squared,
               summary(model_poles)$r.squared,
               summary(model_wins)$r.squared,
               summary(model_top5)$r.squared,
               summary(model_top10)$r.squared)

names(r2_values) <- c("Points", "Poles", "Wins", "Top5", "Top10")
r2_values


# part 2 
# multiple regression model
NascarModel <- lm(Winnings ~ Poles + Wins + Top5 + Top10, data = nascar)
summary(NascarModel) 

# part 3 New Variables: Top 2–5 and Top 6–10
# independent variables
nascar$Top2_5 <- nascar$Top5 - nascar$Wins
nascar$Top6_10 <- nascar$Top10 - nascar$Top5

# revised regression model
model_revised <- lm(Winnings ~ Poles + Wins + Top2_5 + Top6_10, data = nascar)
summary(model_revised)

# part 4 : best model and interpret 
# final selected model
final_model <- lm(Winnings ~ Wins + Top2_5 + Top6_10, data = nascar)
summary(final_model)
# each coefficient says how much winnings increase per additional win or top finish. 
# wins has highest coefficient, meaning that race wins has greatest impact 



