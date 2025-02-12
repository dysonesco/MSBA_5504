# Clear the Environment ----
rm(list = ls()) 

library(ggplot2)
library(dplyr) 

# Clear the Environment ----
rm(list = ls())

spring <- read.csv("Ch 15 /SpringHouses - Data.csv") 

head(data) 

# Question 1 
#Renaming variables 
colnames(spring) <- c( "price", "bath", "SqFt", "beds")


plot(spring) 

pairs(spring,lower.panel = panel.smooth)

spring_model 

?mtcars
cor(mtcars)
round(cor(mtcars),2) 


??corrplot
library(corrplot)
corrplot(cor(mtcars))






