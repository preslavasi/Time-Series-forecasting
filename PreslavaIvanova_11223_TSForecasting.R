# Importing data----
setwd("D:\\Preslava\\uni\\Business Analytics\\Boryana Tuesday\\Assignment")
library(quantmod)
jpm=getSymbols("JPM",from = "2009-01-01", to = "2019-06-14", src="yahoo")
y=JPM$JPM.Adjusted # our working variable

# Inspecting data----
sum(is.na(y)) # no missing values
# Plotting initial data
windows()
layout(matrix(c(1:2),1,2,byrow=T))
plot(y,col="blue",type="l",lwd=2) # a clear trend, so we have to transform the data
hist(y) # far from normally distributed
summary(y)
# ADF test for stationarity, just in case
library(tseries)
adf=adf.test(y) #H0: a unit root is present; H1: TS is stationary 
adf$p.value # 0.6072757; p-value>0.05, so we have to transform the data

# Decomposing data----
library(stats)
dec=ts(y,frequency=252,start = 2009-01-01 )
y_dec=decompose(dec) # season
plot(y_dec)

# Transforming data and making the series staionary----
library(astsa)
logreturns=diff(log(y),lag=1) # log transform of the data
windows()
plot(logreturns,col="blue",type="l",lwd=2) # data is now stationary
logreturns=logreturns[-which(is.na(logreturns)),]
adflog=adf.test(logreturns)
adflog$p.value # 0.01 < 0.05, so there is now stationarity in the values

# Spiltting data into train and test set----
library(caTools)
set.seed(700731)
sample=sample.split(logreturns,SplitRatio = 0.80)
train =subset(logreturns,sample ==TRUE)
test=subset(logreturns,sample==FALSE)

# Plotting ACF & PACF----
windows()
layout(matrix(c(1:2),2,1,byrow=T))
acf(logreturns,lag.max=50,col="red") # small spike at lag4: MA(2)
pacf(logreturns,lag.max=50,col="red") # spike at lag4: AR(1)
library(fBasics)
basicStats(logreturns)

# Auto ARIMA modelling----
# Automatically choosing the model
library(forecast)
model=auto.arima(logreturns,stationary=TRUE,ic="aic",trace=TRUE)
model # ARIMA (3,0,1)
# Fitting the model
fit=arima(train,order=c(3,0,1)) 
summary(fit) # RMSE=0.0218; MAE=0.0135
windows()
layout(matrix(c(1:2),1,2))
plot(fit$residuals,col="blue",type="l",lwd=2) #stationary
acf(fit$residuals,lag.max=50,col="red")

# Model forecast----
fc=forecast(fit,h=252,level=c(90,95))
library(ggplot2)
windows()
autoplot(fc, fcol = "red", flwd = 0.5) + ylab("") + theme_bw() # flat forecast; white noise process
f1=predict(fit,n.ahead =length(test))$pred
accuracy(f1,test) # RMSE=0.0202;MAE=0.0128
summary(f1)
