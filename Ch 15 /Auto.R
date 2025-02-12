# Clear the Environment ----
rm(list = ls()) 

auto <- read.csv("Ch 15 /AutoResale - Data.csv") 


model <- lm( Price ~ Mileage + Age , data=auto) 
summary(model)


options(scipen = 999)
summary(model)


plot(auto)
cor(auto)
