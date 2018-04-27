install.packages("quantmod")
library(quantmod)
start <- as.Date("2017-01-01")
end <- as.Date("2017-11-01")
getSymbols("GOOGL", src = "yahoo", from = start, to = end)
dim(GOOGL)
head(GOOGL)
names(GOOGL)
str(GOOGL)
tail(GOOGL)

#for extracting dates use index function
dates <- as.data.frame(index(GOOGL))
names(dates) <- c('dates')

#extracting GOOGL.Close price from OHLC
stock <- GOOGL$GOOGL.Close

#change the name of the column
names(stock) <- c('Price')

#cbind dates and stock prices
data <- cbind.data.frame(dates,stock)

#write data as csv file
write.csv(data,'data.csv',row.names = F)

#read data.csv
data2 <- read.csv('data.csv',header=T)

# data2$dates=as.Date(data2$dates,format="%Y-%m-%d")
# 
# # To find the minimum of the dates 
# minDate=min(as.Date(data2$date,format="%Y-%m-%d")) 
# # To find the maximum of the dates 
# maxDate =max(as.Date(data2$date,format="%Y-%m-%d")) 
# # generating the series of dates 
# seq <- data.frame("dateRange"=seq(minDate,maxDate,by="days"))


data2$Week <- as.numeric(format(data2$dates, format="%Y.%W"))

# Now aggregating to weekly data
library(sqldf) 
data_weekly <- sqldf("select Week as Week,min(Price) as Price from data2 group by Week")

#Dividing data as Train & Test 
Train=data_weekly[which(data_weekly$Week<=2017.43),]
Test=data_weekly[which(data_weekly$Week>2017.43),]
Price <- ts(Train$Price, frequency =12)#as we have 44 observation, thus taking 12 as frequency
plot(Price)
pricedecomp <- decompose(Price)
plot(pricedecomp)


###Holt winter's model taking both Trend component and Seasonality (additive)
hw_price_gamma <- HoltWinters(Price, beta=TRUE, gamma=TRUE, seasonal="additive")
library("forecast")
hw_price_forecast = forecast(hw_price_gamma, h=1)
test_pred = data.frame(hw_price_forecast)$Point.Forecast
test_actuals = Test$Price
DMwR::regr.eval(test_actuals,test_pred)

# #Building the Holt winter's model taking only Trend component. 
hw_forecast <-  HoltWinters(Price,  beta=TRUE, gamma=FALSE)
test_pred1 = data.frame(hw_price_forecast)$Point.Forecast
test_actuals1 = Test$Price
DMwR::regr.eval(test_actuals1,test_pred1)

#### ARIMA models
plot(Price)
library("forecast")
par(mfrow=(c(1,2)))
pacf(Price)
acf(Price)

ndiffs(Price) #choose d from here; OR check manually
plot(pacf(diff(Price),1, lag.max = 30))#choose p from here
plot(acf(diff(Price),1))

##
model1 <- Arima(Price,c(0,1,0))
model1
pred_train = fitted(model1)
pred_validation <- data.frame(forecast(model1,h=1))$Point.Forecast

DMwR::regr.eval(Train$Price, pred_train )
DMwR::regr.eval(Test$Price, pred_validation )



##
model2 <- Arima(Price,c(1,1,0))
model2
pred_train2 = fitted(model2)
pred_validation2 <- data.frame(forecast(model2,h=1))$Point.Forecast

DMwR::regr.eval(Train$Price, pred_train2 )
DMwR::regr.eval(Test$Price, pred_validation2 )

##
model3 <- Arima(Price,c(1,1,1))
model3
pred_train3 = fitted(model3)
pred_validation3 <- data.frame(forecast(model3,h=1))$Point.Forecast

DMwR::regr.eval(Train$Price, pred_train3 )
DMwR::regr.eval(Test$Price, pred_validation3 )

###Auto-ARIMA
library("forecast")
ARIMA_auto <- auto.arima(Price, ic='aic')
summary(ARIMA_auto)

pred_train = fitted(ARIMA_auto)
pred_train

pred_test = forecast(ARIMA_auto, h=1)
pred_test

DMwR::regr.eval(Train$Price, pred_train )
DMwR::regr.eval(Test$Price, pred_validation )
