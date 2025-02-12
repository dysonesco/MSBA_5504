# Clear the Environment ----
rm(list = ls()) 

fridge <- read.csv("Ch 15 /Refrigerators - Data.csv") 
colnames(fridge) <- c( "model", "iceWater", "cubicFt", "price")

model <- lm( price ~ iceWater + cubicFt, data=fridge) 
summary(model) 


