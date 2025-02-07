library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("BuckeyeCreek - Data.csv") 

head(data) 

summary(data)

#Renaming variables 
colnames(data) <- c( "zip", "population", "pass")

#part 1: Scatter Plot and descriptive stats ----
ggplot(data, aes(x=population, y=pass)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  labs(title="Scatter Plot of Population vs. Season Pass Holders",
       x="Total Population in ZIP Code",
       y="Season Pass Holders")

  # Since the trend is positive, a higher population results in more season pass 
  # holders 

# part 2 and 3: Regression and Significance ----
model <- lm(pass ~ population, data=data)
summary(model) 
anova(data_slr)
  # since p-value is extremely low, almost close to zero, zip code 
  # population strongly influences number of season passes. 


#part 4:regression equation good fit? ---- 
summary(data_slr)$r.squared
  # R-squared:  0.6374 not good fit. 63% of the variation in season passes
  # can be attributed to zip code. There is likely various other factors as well. 


# part 5: residual analysis ----
par(mfrow=c(2,2))
plot(model)
  #residuals vs fitted shows random scatter 
  #normally distributed QQ residuals

# part 6: Target Marketing 
data$Predicted <- predict(model, newdata=data)
data$Difference <- data$pass - data$Predicted

underperforming <- data %>% filter(Difference < 0)
print(underperforming) 

  # By using predicted vs actual season passes, we can see the underperforming 
  # zip codes and target marketing for them. 

# part 7: other data 
# Other data that could be used for prediction might be: 
# Family size, proximity to park, etc
