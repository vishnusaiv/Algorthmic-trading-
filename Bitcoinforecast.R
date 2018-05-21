install.packages("anytime")
install.packages("xts")
install.packages("hts")
install.packages("forecast")
library(ggplot2)
library(xts)
library(hts)
library(forecast)
train = read.csv("C:/Users/25769/Desktop/Great lakes/MyData.csv")
attach(train)


head(train)
 
colnames(train)
summary(train)


train$Volume = gsub(',','',train$Volume)
train$Market.Cap = gsub(',','',train$Market.Cap)
train$Volume <- NULL
train$Market.Cap = as.numeric(train$Market.Cap)
head(train)
#convert our data into xts and order by dates
Train = xts(train[, -1], order.by=as.POSIXct(train$Date))
head(Train)
#simple exponential smoothing 
plot(ses(Train[,'Close']))

plot(holt(Train[,'Close']))

plot(thetaf(Train[,'Close']))
plot(forecast(Train[,'Close']))

plot(train)

bitcoin  <- data.frame(close=train$Close,
                       open=log(train$Open+1),
                       high=log(train$High),
                       low=log(train$Low+1),
                       market=log(train$Market.Cap+1))
fit <- step(lm(close ~ open  + high 
               + low + market, data=bitcoin))
summary(fit)
plot(fitted(fit), bitcoin$close,ylab="Closing Price", xlab="Predicted Closing price")

m <- HoltWinters(Train[,'Close'], gamma = FALSE)

plot(m, type="o", ylab="Closing Price of Bitcoin", xlab="Time", fcol="white", plot.conf=FALSE)

plot(forecast(m,h=80))

plot(Train, xlab="Year",main="The Closing value of Bitcoins")
fit = auto.arima(Train[,"Close"])
tsdisplay(arima.errors(fit), main="ARIMA errors")
plot(forecast(fit))
y <- hts(Train,  nodes=list(3, c(3,1,1)))
allf <- forecast(y, h=80)
plot(allf)