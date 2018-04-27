rm(list=ls(all=TRUE))

#scrape YAHOO for google stock price
#install.packages("quantmod")
library(quantmod)
start <- as.Date("2017-01-01")
end <- as.Date("2017-11-01")
getSymbols("GOOGL", src = "yahoo", from = start, to = end)

#this also works fine
#data <- GOOGL["2017-01-01/2017-11-01"]

#for extracting dates use index function(library:zoo)
dates <- as.data.frame(index(GOOGL))
names(dates) <- c('dates')

#write dates as csv file
write.csv(dates,'dates.csv',row.names = F)

#extracting GOOGL.Close price from OHLC
stock <- as.xts(data.frame(GOOGL = GOOGL[,"GOOGL.Close"]))

#change the name of the column
names(stock) <- c('close')

#write stock prices as csv file
write.csv(stock,'stock.csv',row.names = F)

#cbind dates and stock prices
data <- cbind.data.frame(dates,stock)

#write the cbind'ed' data variable into a merged file
write.csv(data,'final.csv',row.names = F)

#read final.csv in another variable
data2 <- read.csv('final.csv',header=T)
str(data2)
#converting data2$dates to dates from factors
data2$dates <- as.Date(data2$dates,format="%Y-%m-%d")

# generating sequence of dates from 01/01/2017 to 01/11/2017
# seq <- data.frame("date_range"=seq(start,end,by="days"))

data2$week <- as.numeric(format(data2$dates, format="%Y.%W"))

# #merge data2 with seq to see the missing values for the dates.
# #all.x <- left join
# #all.y <- right join
# I'm working on this part though for the time being I extracted the
# data manually and performing time series.

data2$Week <- as.numeric(format(data2$dates, format="%Y.%W"))
write.csv(data2,'finaldata.csv',row.names = F)
data2 <- read.csv('finaldata.csv',header = TRUE)

#dividing data as tain and test
train <- data2[which(data2$week <= 2017.43),]
test <- data2[which(data2$week > 2017.43),]

#prepare data2 for time series by applying ts keeping frequency as 0.84/7/52
price <- ts(train$close , frequency = 52)

#plot price
plot(price,type="l",lwd=3,col="blue",xlab="day",ylab="price", main="Time series plot")
#trend can be seen(increasing graph)

#decomposition of the above plot in trend, seasonality and randomness
pricedecomp <- decompose(price)
plot(pricedecomp)

#smoothing
fitsma <- SMA(price,n=7)
length(fitsma)

#plot simple, weighted and exponential moving averages
par(mfrow=c(2,2))
plot(train$close, type="l", col="black")
plot(SMA(price,n=7), type="l", col="red")
plot(WMA(price,n=7), type="l", col="blue")
plot(EMA(price,n=7), type="l", col="brown")

#compiled into one graph
par(mfrow=c(1,1))
plot(train$close, type="l", col="black")
lines(SMA(price,n=5), col="red")
lines(WMA(price,n=5), col="blue")
lines(EMA(price,n=5), col="brown")

#building Holt Winter's model
#with trend 'Beta' as positive
hw_forecast <-  HoltWinters(price,  beta=TRUE, gamma=FALSE)
head(hw_forecast$fitted)

#with both trend and seasonality 'beta' and 'gamma' as positive
hw_forecast2 <-  HoltWinters(price, beta=TRUE, gamma=TRUE,
                             seasonal="additive")

#for forecasting
library("forecast")
hw_price_forecasts = forecast(hw_forecast2,h=2)
test_preds <- data.frame(hw_price_forecasts)$Point.Forecast
test_actuals <- test$close
DMwR::regr.eval(test_actuals,test_preds)

----------------------------------------#-----------------------------------------------

#ARIMA models
par(mfrow=(c(1,2)))
pacf(price)
acf(price)

ndiffs(price) #choose d from here; OR check manually
plot(pacf(diff(price),1, lag.max = 30))#choose p from here
plot(acf(diff(price),1)) #choose q from here

##
model1 <- Arima(price,c(1,1,0))
model1
pred_train = fitted(model1)
pred_test <- data.frame(forecast(model1,h=2))$Point.Forecast

#why test$close is numeric(0) ??
DMwR::regr.eval(train$close, pred_train )
DMwR::regr.eval(test$close, pred_test )

##
model2 <- Arima(price,c(0,1,0))
model2

pred_train = fitted(model2)
pred_test <- data.frame(forecast(model2,h=2))$Point.Forecast

DMwR::regr.eval(train$close, pred_train )
DMwR::regr.eval(test$close, pred_test )


##
model3 <- Arima(price,c(1,1,1))
model3

pred_train = fitted(model3)
pred_test <- data.frame(forecast(model3,h=2))$Point.Forecast

DMwR::regr.eval(train$close, pred_train )
DMwR::regr.eval(test$close, pred_test )


par(mfrow=c(1,3))
plot(model1$residuals,ylim=c(-50,50))
plot(model2$residuals,ylim=c(-50,50))
plot(model3$residuals,ylim=c(-50,50))

#Plot forecasts
par(mfrow=c(1,1))
plot(forecast(model3,h=4))

# Later on will try
# ###Auto-ARIMA
# library("forecast")
# ARIMA_auto <- auto.arima(price, ic='aic')
# summary(ARIMA_auto)
# pred_train <- fitted(ARIMA_auto)
# pred_train
# pred_test <- forecast(ARIMA_auto, h=13)
# pred_test
# DMwR::regr.eval(train$close, pred_train )
# DMwR::regr.eval(test$close, pred_test )
