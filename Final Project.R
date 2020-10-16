library(forecast)
library(TSA)
library(fGarch)
library(rugarch)

stock = read.table("C:\\RUTGERS\\565-Applied Time Series Analysis\\Final Project\\m-3m4608.txt", head = T)

# Log transformation
rtn.log =log(stock[,2]+1)

#White noise test
Box.test(rtn.log, lag=6, type="Ljung")
Box.test(rtn.log, lag=12, type="Ljung")

# Determine the order
par(mfrow=c(1,1))
plot(rtn.log,type = 'l', main='log series of returns')
acf(rtn.log)
pacf(rtn.log)

# calculate EACF
TSA::eacf(rtn.log, ar.max = 10)

# ARIMA(3, 0, 3) model
m1=arima(rtn.log, order = c(3,0,3))
m1

# Ljung-Box statistics
# Plot residuals and residuals^2
plot(m1$residuals,type="l")
plot(m1$residuals^2,type="l")

# H0: First m lags of ACF of squared series are 0
# Use Residuals sqaure
Box.test(m1$residuals^2, lag = 6, type = "Ljung")
Box.test(m1$residuals^2, lag = 12, type = "Ljung")


acf(m1$residuals, main = "acf of residual")
pacf(m1$residuals, main = "pacf of residual")
acf(m1$residuals^2,main = "acf of squared residual")
pacf(m1$residuals^2, main = "pacf of squared residual")

# Fit ARCH model
m2 = garchFit(~arma(3, 3) + garch(1, 1), data = rtn.log, trace = F, include.mean= T,cond.dist = 'std')
summary(m2)

m3 = garchFit(~arma(3, 3) +  garch(1, 1), data = rtn.log, trace = F, include.mean= T,cond.dist = 'sstd')
summary(m3)

forecast=ugarchforecast(m3, n.ahead=2, data=rtn.log)

forecast(m3,h=2)
p=predict(m3,2)
p
plot(m3)
pred=predict(m3,n.ahead=2,plot=TRUE)
ts.plot(rtn.log,exp(pred$pred), log = "y", lty = c(1,3))
predict(m3, 2)
