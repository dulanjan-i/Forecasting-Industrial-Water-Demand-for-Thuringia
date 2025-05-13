#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)
library(ggplot2)

# Load the data
annualdata <- read.csv("ind_annual.csv")

# Check the structure and first few rows of the data
str(annualdata)
head(annualdata)

# Convert the 'Year' column to a time series object for the 'Water supply' column
# Frequency is set to 1 
ts_water_supply <- ts(annualdata$`Water.supply`, start=min(annualdata$Year), frequency=1)

#plot 
plot(ts_water_supply, main="Water Supply Over Time", xlab="Year", ylab="Water.Supply")

#Stationary check wirth Augmented Dickey Fuller Test
adf.test<- adf.test(ts_water_supply)
print(adf.test)

#since data is likely stationary (p value < 0.05)

#fit ARIMA Model
fit<- auto.arima(ts_water_supply, d=0)
summary(fit)

#forcasting
forecasted<- forecast(fit, h=5)
plot(forecasted, main= "ARIMA")

accuracy(forecasted)
