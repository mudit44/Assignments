rm(list=ls(all=TRUE))

#read the data
data1 <- read.csv('ProductA.csv',header = T)
data2 <- read.csv('ProductB.csv',header = T)

#adding column for classification
#note if you are giving 'A' and 'B' as product categories in 
#place of 1 and 2 you might have NULL values as a whole column in 
#Tableau.
#choose the values with cross checking in Tableau.
data1$type <- rep(1, nrow(data1))
data2$type <- rep(2, nrow(data2))

#row bind the two data frames
data <- rbind.data.frame(data1,data2)

#convert to factors
data$type <- as.factor(data$type)
data$Target <- as.factor(data$Target)
str(data)

#writing the file
write.csv(data,file = 'blend.csv',row.names = FALSE)

#with the help of range, see if any visualisation can be created
summary(data)
