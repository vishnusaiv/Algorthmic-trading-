library(forecast)
library(forecasting)
library(tseries)
library(timeSeries)
library(ggplot2)
bitcoin =read.csv("C:/Users/25769/Desktop/Great lakes/capstone/MyDatalatest.CSV")
head(bitcoin)
# Compute the log returns for the bitcoins
bitcoins = diff(log(bitcoin$Close),lag=1)
bitcoins = bitcoins[!is.na(bitcoins)]
# Plot log returns 
plot(bitcoins,type='l', main='log returns plot')
#  ADF test on log returns series
adf.test(bitcoin$Close)
#acf abd Pacf plot
acf(bitcoin$Close)
pacf(bitcoin$Close)

fit =auto.arima(bitcoin$Close,ic="bic")
fit

plot( as.ts(bitcoin$Close))
lines(fitted(fit),col="red")
fitforecast = forecast(fit)
fitforecast
plot(fitforecast)
