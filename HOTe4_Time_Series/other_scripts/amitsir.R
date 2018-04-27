rm(list = ls(all=TRUE))
setwd("D:/Insofe/Day12/")
#load data
data <- read.csv("GOOGL.csv")
#edit(data)

data <- data[,c(1,5)]
#aggredata the data
data$Date <- as.Date(data$Date, "%m/%d/%Y")
str(data)

minDate <- min(data$Date)
maxDate <-max(data$Date)

#dates are not sequential - create a sequence of dates from min date and max dates, find the missing date

seq <- data.frame("dateRange" =seq(minDate, maxDate, by ="days"))
head(seq)
#do left join on seq(master date list) and data
data =merge(seq,data, by.x = "dateRange", by.y = "Date", all.x=T)
head(data)

# Use the above code to replace the missing values in the Price variable
data$Close=(na.locf(data$Close) + rev(na.locf(rev(data$Close))))/2

head(data)

format(data$dateRange, format="%Y.%W")

data$WEEK <- as.numeric(format(data$dateRange, format="%Y.%W")) 
head(data) # Now aggregating to weekly data 


data <- data
head(data) 
library(sqldf) 
data_weekly <- sqldf("select WEEK as WEEK, max(Close) 
                     as Max_Value from data group by WEEK")

# Let us verify this before we move on. 
head(data)
str(data)

#Dividing data as Train & Test 

#Dividing data as Train & Test 
Train=data_weekly[which(data_weekly$WEEK<=2017.40),] 
Test=data_weekly[which(data_weekly$WEEK>2017.40),]

#build timeseries model
 
Price <- ts(Train$Max_Value, deltat =1.181)
deltat(Price)
frequency(Price)
plot(Price, type= "l", lwd=3, col="blue", xlab= "week", ylab="Price", main = "Time series")

#decompose
pricedecomp <- decompose(Price)

plot(pricedecomp)

# par mfrow is to show 2 graphs side by side. 
#You could change it to say c(2,4) to see 4 graphs in 2 rows. C(1,3) to see the 3 graphs in a single row.

par(mfrow=c(1,2)) 
acf(Price,lag=30)
pacf(Price,lag=30)  

#Price1 <- ts(Train$MIN_PRICE, frequency =1) 
par(mfrow=c(1,2)) 
acf(Price,lag=30)
pacf(Price,lag=30)

par(mfrow=c(1,2))
plot(diff(Price,lag=1),type="l") 
plot(diff(Price,lag=2),type="l")

### Smoothing models
# The library TTR stands for Technical trading rules. l
library(TTR) 
fitsma <- SMA(Price,n=2) 
length(fitsma) 
length(Price)

plot(Price,type="l",lwd=3,col="blue",xlab="week",ylab="Price", main="Time series plot")
fitsma <- SMA(Price,n=5)
length(fitsma)
par(mfrow=c(2,2)) 



plot(Train$Max_Value, type="l", col="black") 
plot(SMA(Price,n=5), type="l", col="red")
plot(WMA(Price,n=5), type="l", col="blue") 
plot(EMA(Price,n=5), type="l", col="brown")

par(mfrow=c(1,1))
plot(SMA(Price,n=5), type="l", col="red")
plot(WMA(Price,n=5), type="l", col="blue") 
plot(EMA(Price,n=5), type="l", col="brown")


#Building the Holt winter's model taking only Trend component. 
holtpriceforecast <- HoltWinters(Price, beta=TRUE, gamma=FALSE) #beta = trend, #GAMMA Seasonality
# Look the fitted or forecasted values 
head(holtpriceforecast$fitted)

priceholtforecast <- HoltWinters(Price, beta=TRUE, gamma=FALSE, seasonal="additive") 
# Look the fitted or forecasted values . Did you notice the 
head(priceholtforecast$fitted)

library(forecast)
# Getting the predictions on Training data & Checking model 
holtforecastTrain <- data.frame(priceholtforecast$fitted) 
holtforecastTrainpredictions <- holtforecastTrain$xhat 
head(holtforecastTrainpredictions) # To get the predictions on Test Data, you can use function "forecast.Holt". "h" indicates the number of future weeks (or whatever be your reference time period, say months, quarters, etc.,) for which you want to get the predictions 


#forecast.HoltWinters(priceholtforecast, h=1)
priceforecast <- forecast(priceholtforecast, h=4)

#get predicted values

test_preds <- data.frame(priceforecast)$Point.Forecast

data.frame(priceforecast)$Point.Forecast
test_preds
test_actuals <- Test$Max_Value
Test$Max_Value
Test
test_actuals
test_preds
DMwR::regr.eval(test_actuals, test_preds)

# Automated functions are available
PriceAutoArima <- auto.arima(Price,ic='aic')
PriceAutoArima
PriceforecastsAutoArima <- forecast(PriceAutoArima, 
                                              h=4)
plot(PriceforecastsAutoArima)
PriceforecastsAutoArima

test_preds <- data.frame(PriceforecastsAutoArima)$Point.Forecast

data.frame(PriceforecastsAutoArima)$Point.Forecast
test_preds
test_actuals <- Test$Max_Value
Test$Max_Value
Test
test_actuals
test_preds
DMwR::regr.eval(test_actuals, test_preds)

