rm(list=ls())

#Read the file.
data <- read.csv('BackOrders.csv',header=TRUE)
str(data)
summary(data)
head(data)

#Converted a variable to factor
data$sku <- as.factor(data$sku)
str(data)

#Sum of NA's
sum(is.na(data))

#to check which column has the highest number of NAs.
sapply(data, function(x) sum(is.na(x)))

# Just noting things down
# If You need NA count of all - table(is.na273(z))
# If you need NA count Column wise - sapply(z, function(x) sum(is.na273(x)))
# If you need NA count Row wise - rowSums(is.na273(z))

#Splitting data into train and test without imputing.
set.seed(123)
rows = seq(1,nrow(data),1)
set.seed(123)
trainRows = sample(rows,(20*nrow(data))/100)
trainRows2 = data[-trainRows,]#this contains 80% data
#sample again from these
set.seed(123)
trainRows3 = sample(rows,(20*nrow(trainRows2))/100) #20% of 80% data
train = data[trainRows,]
test = data[trainRows3,]

# train_sku <- train$sku
# test_sku <- test$sku
# rm('train_sku','test_sku')

# Segregating train in train_num and & train_cat
train_cat <- train[,sapply(train,is.factor)]
train_num <- train[,sapply(train,is.numeric)]
# Segregating test in test_num & test_cat
test_cat <- test[,sapply(test, is.factor)]
test_num <- test[,sapply(test, is.numeric)]

#finding mode function

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

sum(is.na(train_num)) #showing 692 NA's
#centralImputation on train_cat
library(DMwR)
#Imputing NAs in train_num with centralImputation
train_num <- centralImputation(train_num)
getmode(train_num$lead_time)
#Imputing mode of train into NA's of test
test_num$lead_time[is.na(test_num$lead_time)] <- 8
sum(is.na(test_num))
sum(is.na(train_num))
# rm('train_mean','train_sd')
# Now taking the mean and sd of the train_num
train_mean <- apply(train_num,2,mean)
train_sd <- apply(train_num,2,sd)

# Standardizing the train_num
std_train <- sweep(sweep(train_num,2,train_mean),2,train_sd,"/")
train_standard <- data.frame(cbind(std_train,train_cat))
#train_standard is the final dataset for the train dataset

#rm('std_test')
std_test <- sweep(sweep(test_num,2,train_mean),2,train_sd,"/")
test_standard <- data.frame(cbind(std_test,test_cat))
#test_standard is the final dataset for the test dataset

### Building a basic Logistic Regression Model
log_reg <- glm(went_on_backorder~., data = train_standard, family = binomial)
summary(log_reg)

prob_train <- predict(log_reg, type="response")
# By default if no dataset is mentioned, training data is used
prob_test <- predict(log_reg, test_standard, type="response") # Predicting on test data

library(ROCR)
pred <- prediction(prob_train, train_standard$went_on_backorder)
perf <- performance(pred, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

pred_class <- ifelse(prob_train > 0.1, 1, 0)
table(train_standard$went_on_backorder,pred_class)

#probability values v/s predicted values table
prob_val <- predict(log_reg, test_standard, type = "response")
preds_val <- ifelse(prob_val > 0.1, 1, 0)
table(preds_val)

#Manually creating confusion matrix
conf_matrix <- table(test_standard$went_on_backorder, preds_val)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)

#Use vif to find any multi-collinearity
library(car)
log_reg_vif = vif(log_reg)
log_reg_vif

#Improve the model using stepAIC
library(MASS)
log_reg_step = stepAIC(log_reg, direction = "both")
summary(log_reg)