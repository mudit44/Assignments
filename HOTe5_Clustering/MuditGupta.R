rm(list=ls(all=TRUE))

#read the data
data <- read.csv('Cereals.csv',header = TRUE)

#EDA
head(data)
tail(data)
str(data)
summary(data)

#NA values
sum(is.na(data))

#NA pattern
sapply(data, function(x) sum(is.na(x)))

#rows which have NA's along with columns
which(is.na(data), arr.ind=TRUE)
#check parallel.ipynb for heatmap

#total rows
nrow(data)

#dimension
dim(data)

#impute NA's
names(data)

#check mean
mean(data$carbo, na.rm = T)
mean(data$sugars, na.rm = T)
mean(data$potass, na.rm = T)

#check median
# x = c('carbo','sugars','potass')
# x_func <- function(x) {
#   for (i in x) {
#     y <- median(x[i], na.rm = T)
#     return (y)
#   }
# }
# x_func(x) #not working

median(data$carbo, na.rm = T)
median(data$sugars, na.rm = T)
median(data$potass, na.rm = T)

#mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#check mode
getmode(data$carbo)
getmode(data$sugars)
getmode(data$potass)

#so filling Na's with mean/median
data$carbo[is.na(data$carbo)] <- mean(data$carbo, na.rm = T)
data$sugars[is.na(data$sugars)] <- mean(data$sugars, na.rm = T)
data$potass[is.na(data$potass)] <- mean(data$potass, na.rm = T)
#try for loop

#search for unique values for each column to discriminate whether
#a field is numerical or categorical
sapply(data,function(x)unique(x))

#shelf, vitamins are factors
#convert to factors
data$shelf <- as.factor(data$shelf)
data$vitamins <- as.factor(data$vitamins)

#subset shelf and vitamins as factor dataframe
x_factor <- subset(data, select = c('shelf','vitamins'))
data$shelf <- NULL
data$vitamins <- NULL

#dummify x_factor
library(dummies)
shelf <- dummy(x_factor$shelf)
vitamins <- dummy(x_factor$vitamins)

#changed all the factors to dummies
x_factor <- cbind(shelf,vitamins)
#taking the data$names and cbind to x_factor, this becomes a factor dataframe fully.
name <- data$name
x_factor <- cbind.data.frame(name,x_factor)

#drop name from data
data$name <- NULL

#scale the numerical columns
data <- scale(data)

#cbind data with x_factor
data2 <- cbind(x_factor,data)

#----------------------------------Prepared data for clustering----------------------------#

#getting this error message repeatledly
#Warning message:
#In dist(rbind(x1, y1)) : NAs introduced by coercion
#I think it's because dummy creates n dummies for n number of factors 
#but only n-1 factors can explain the last one
#so stored the shelf3 and vitamin100 in other variable
#and droping them from data2

#tried the above thing, its not helping

#maybe its because names are included in the dataset
#so I'll try dropping name column from it

#but before dropping I'm putting them as rownames so as to 
#know which all products are clustered together
row.names(data2) <- data2$name

data2$name <- NULL
#the error got removed

#distance matrix (type:euclidean)
distance <- dist(data2,method = "euclidean")

#WARD is a min variance method to find compact clusters
fit <- hclust(distance, method="ward.D")

#display dendogram
plot(fit)
fit$merge
fit$dist.method

#although it's subjective but I'm taking 6 clusters into account
groups <- cutree(fit, k=6) #cut tree into 6 clusters
groups

#showing the total number of cuts
rect.hclust(fit, k=6, border="red")

mydata_clusters=data.frame(mydata,groups)
par(mfrow=c(2,2))
fit2 <-hclust(distance, method="complete")
fit3 <- hclust(distance, method="single")
fit4 <- hclust(distance, method='average')
fit5 <- hclust(distance, method='ward.D2')

dev.off()
par(mfrow = c(2, 2))

plot(fit2, leaflab = "textlike", main = "Complete", xlab = "")
plot(fit3, leaflab = "textlike", main = "Single", xlab = "")
plot(fit4, leaflab = "textlike", main = "Average", xlab = "")
plot(fit5, leaflab = "textlike", main = "Ward", xlab = "")

#finding optimum number of clusters
library(factoextra)
fviz_nbclust(data2, hcut, method = "wss")

#----------------------------K-Means Clustering--------------------------------------------#

set.seed(123)
fit <- kmeans(data2, 6)
fit$withinss
fit$betweenss

#metric of clusters
fit$cluster
fit$tot.withinss
fit
fit$centers

#install.packages("factoextra")
library(factoextra)
fviz_cluster(fit, data2)

#append cluster label to actual data frame
data2 <- data.frame(data2,fit$cluster)

wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(data2,centers=i)$withinss)
}

#the scree plot
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fviz_nbclust(data2[,-c(18)], kmeans, method = "wss")
#choose the elbow point, that is your number of cluster formed
set.seed(123)
final_fit_kmeans <- kmeans(data2, 8)

#------------------------Cluster Formation on unseen data----------------------------------#

#for unseen data compute its distance from all the cluster centroids
#and assign it to that cluster that is nearest to it

test_datapoint <- data2[sample(1:nrow(data2),1),]
closest.cluster <- function(x) {
  cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
  print(cluster.dist)
  return(which.min(cluster.dist)[1])
}
closest.cluster(test_datapoint)

#-----------------------------------Quality check------------------------------------------#

library(cluster)
distance_matrix <- daisy(x = data2, metric = "euclidean")
clust_assignment <- data2$fit.cluster
sil_value_hc_mixed <- silhouette(clust_assignment, dist = distance_matrix)
plot(sil_value_hc_mixed)

#
#
#
#
#
#
#----------------------------------stability check----------------------------------------#

set.seed(123)
index <- (sample(nrow(data2),.70*nrow(data2)))
data3 <- data2[index,]
fit6 <- kmeans(data3,3)
data3$clusters <- fit6$cluster

group1 <- data2$fit.cluster[index]
group2 <- data3$clusters

#loop through this for n times. 
#install.packages("fossil")
library(fossil)
stability_check <- adj.rand.index(group1, group2)
stability_check
#across samples: avg_stabilitycheck
#index value is between 0 and 1, where 1 means the two clustering outcomes match identically.


#install.packages("clusteval")
library(clusteval)
stab_index <- cluster_similarity(group1, group2,
                                 similarity = "jaccard", method="independence")
stab_index