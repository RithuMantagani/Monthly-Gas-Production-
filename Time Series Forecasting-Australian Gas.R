install.packages("forecast")
library(forecast)
library(tseries)
library(ggplot2)
library(kableExtra)
library(tseries)
data<-forecast::gas
print(data)
AU.gas=ts(data,start = c(1956,1),frequency = 12)
head(AU.gas)
tail(AU.gas)
class(AU.gas)
##Checking the Missing Values##
any(is.na(AU.gas))
##################
start(AU.gas)
end(AU.gas)
frequency(AU.gas)
summary(AU.gas)
cycle(AU.gas)

AU.gas.qtr=aggregate(AU.gas,nfrequency = 4)
AU.gas.yrly=aggregate(AU.gas,nfrequency = 1)
AU.gas.qtr
AU.gas.yrly

####Plots######
plot.ts(AU.gas,main="Monthly Gas Production 1956-1995",xlab="Year", ylab="Gas Production")
plot.ts(AU.gas.qtr,main="Querterly Gas Production 1956-1995",xlab="Year", ylab="Gas Production")
plot.ts(AU.gas.yrly,main="Yearly Gas Production 1956-1995",xlab="Year", ylab="Gas Production")


####Seasonality Plot###
seasonplot(AU.gas, year.labels = TRUE, year.labels.left = TRUE,col = 1:40, pch=19,main = "Monthly Gas production - Seasonplot")
monthplot(AU.gas,main="Monthly Gas Production - Month Plot",xlab="Month", ylab= "Gas Production")

####Boxplot
boxplot(AU.gas~cycle(AU.gas), main = "Monthly Gas Production - Box Plot", xlab = "Month" , ylab = "Gas Production")

####Additive and Multiplicative model###
decompgas=decompose(AU.gas, type = "multiplicative")
plot(decompgas)
decompgas=decompose(AU.gas, type = "additive")
plot(decompgas)

decompgas
decompgas$seasonal
decompgas$trend
decompgas$random

####Plotting Individual#####
plot(decompgas$seasonal)
plot(decompgas$trend)
plot(decompgas$random)

output=(decompgas$trend+decompgas$random)
output_trend<-
  
  ####Using stl Function###########
  plot(stl(AU.gas, s.window = "periodic"))
AU.gas.ts2=stl(AU.gas, s.window = "periodic")
AU.gas.ts2

deseasongas=(AU.gas.ts2$time.series[,2]+AU.gas.ts2$time.series[,3])
ts.plot(deseasongas,AU.gas,col=c("red","blue"), main="comparison gas production and deseasonalise gas production")



plot(stl(log(AU.gas), s.window = "periodic"))
AU.gas.ts3=stl(log(AU.gas), s.window = "periodic")
AU.gas.ts3$time.series[1:12,1]
AU.gas.ts3.season=exp(AU.gas.ts3$time.series[1:12,1])
plot(AU.gas.ts3.season,type = "l")

#Stationarity#
#Dicky Fuller test for Stationarity#
adf.test(AU.gas)

####Decomposing the Au series##
decomposed = stl(AU.gas, s.window = "periodic")
seasonal = decomposed$time.series[,1]  
trend = decomposed$time.series[,2]    
remainder = decomposed$time.series[,3]  

#Removing the seasonality#
des.data = AU.gas - seasonal
plot(des.data, ylab= "Production units", main = "De-Seasonalized Series") ## plotting the de-seasonlized data

#Splitting dataset into Train and Test data#
train.AUgas = window(AU.gas, start=c(1956,1), end = c(1993,12), frequency=12)
test.AUgas = window(AU.gas, start=c(1994,1), frequency=12)
## Plotting the train and Test set 
autoplot(train.AUgas, series="Train") +
  autolayer(test.AUgas, series="Test") +
  ggtitle("AU gas  Traning and Test data") +
  xlab("Year") + ylab("Production") +
  guides(colour=guide_legend(title="Forecast"))

Augas.diff = diff(AU.gas, differences = 2)
adf.test(Augas.diff)

#Checking ACF and PACF#
par(mfrow=c(1,2))
acf(AU.gas, lag.max = 50)
pacf(AU.gas,lag.max = 50)

par(mfrow=c(1,2))
acf(Augas.diff, lag.max = 50)
pacf(Augas.diff,lag.max = 50)

#Building a manual ARIMA[p,d,q] model with seasonal effects [P,D,Q]#
man.arima = arima(train.AUgas, order = c(1,1,1), seasonal = c(1,1,1), method = 'ML')
man.arima

#Forecasting#
### Plotting the forecast of manual arima for 12 advance periods#
par(mfrow=c(1,1))
plot(forecast(man.arima, h=12), shadecols = "oldstyle")
################################################################
Box.test(man.arima$residuals, type = "Ljung-Box", lag = 350)
##################################################
acf(man.arima$residuals, lag.max = 50)
##############################################
hist(man.arima$residuals) ## checking the normal distribution of residuals 
###########################################

#Auto Arima Model#
auto.fit = auto.arima(train.AUgas, trace = F, seasonal = T)
auto.fit

plot(forecast(auto.fit, h=12), ylab = "Gas Production", xlab = "Year")
##########################################################################

Box.test(auto.fit$residuals, type = "Ljung-Box", lag = 350)
#####################################################################

Vec1<- cbind(test.AUgas ,as.data.frame(forecast(man.arima, h=20))[,1])
ts.plot(Vec1, col=c("blue", "red"), main="Gas Production: Actual vs Forecast")
legend("bottomright", legend=c("Actual", "Forecast"),col=c("blue", "red"), cex=0.8, lty= 1:1)

### Accuracy of the manual arima model###
accuracy(forecast(man.arima, 24), test.AUgas)
#### Accuracy of the Auto arima model###
accuracy(forecast(auto.fit, 24), test.AUgas) 

################################################THANK YOU##################################################################
