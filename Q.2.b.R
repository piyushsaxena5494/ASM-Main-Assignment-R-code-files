
Data <- read.csv("C:/Users/john/Desktop/MSc Data Science coursework/Semester 2/APPLIED STATISTICAL MODELLING/Assignments/Main Assignment/wine-reviews/wine-data-merged.csv")
library(dplyr)
#Filtering data with country US
Data <- filter(Data, country == 'US')

#Keeping only price and points column
Data <- Data[,c(5,6)] 

#Filtering null values from price and points column
Data <- Data[!(is.na(Data$price)|Data$price==""|is.na(Data$points)|Data$points==""),] 

#Normalizing function
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}

#Taking log and normalizing price column
Data$price <- log(Data$price)
Data$price <- normalize(Data$price)

#Normalizing points column
Data$points <- normalize(Data$points)

plot(Data)

library(mclust)
fit <- Mclust(Data)

print(fit)
summary(fit)

fit$parameters$pro 
fit$parameters$mean 

plot(fit, what = "classification")
plot(fit, what = "uncertainty")
plot(fit, what = "BIC")
fit$BIC

Data$cluster <- fit$classification
write.csv(Data, "C:/Users/john/Desktop/MSc Data Science coursework/Semester 2/APPLIED STATISTICAL MODELLING/Assignments/Main Assignment/wine-reviews/Q2.csv")
