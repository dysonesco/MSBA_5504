library(ggplot2)
library(dplyr) 

spring <- read.csv("Ch 15 /SpringHouses - Data.csv") 

head(data) 

# Question 1 
#Renaming variables 
colnames(spring) <- c( "price", "bath", "SqFt", "beds")
