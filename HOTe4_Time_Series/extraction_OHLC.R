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

write.csv(dates,'dates.csv',row.names = F)

#extracting GOOGL.Close price from OHLC
stock <- as.xts(data.frame(GOOGL = GOOGL[,"GOOGL.Close"]))
#change the name of the column
names(stock) <- c('close')
write.csv(stock,'stock.csv',row.names = F)

#merge dates and stock
cbind.data.frame(dates,stock)