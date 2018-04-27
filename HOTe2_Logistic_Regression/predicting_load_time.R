rm(list = ls())

#Read the data
data <- read.csv('BackOrders.csv',header = TRUE)

#Explorative Data Analysis(EDA)
summary(data)
str(data)
sapply(data, function(x) sum(is.na(x)))
nrow(data)

#Segregation of NA rows into variable which we will be predicting
#with the help of linear regression
rows_with_na_values_in_lead_time <- data[which(is.na(data$lead_time)),]

#Omitted the NA rows
data <- na.omit(data)

#split the data
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
test = data[-trainRows,]

#take the target variable both from train and test dataset.
train_target_field <- train$lead_time
test_target_field <- test$lead_time

#Null the target variable in the dataset both(train/test)
train$lead_time <- NULL
test$lead_time <- NULL

#converting some numeric variables as factors
train$sku <- as.factor(train$sku)
test$sku <- as.factor(test$sku)

#seperate the categorical and numerical in train set
train_cat <- train[,sapply(train,is.factor)]
train_num <- train[,sapply(train,is.numeric)]

#seperate the categorical and numerical in test set
test_cat <- test[,sapply(test, is.factor)]
test_num <- test[,sapply(test,is.numeric)]

#separating SKU(train/test) from train_cat and test_cat
train_sku <- train_cat$sku
test_sku <- test_cat$sku

#equating SKU(train/test) to NULL in train_cat and test_cat
train_cat$sku <- NULL
test_cat$sku <- NULL

#Just making sure that I did everything right till now
head(train_cat)
head(test_cat)

#Now dummying the train_cat data as every factor has only 2 levels.
library(dummies)
x1 <- dummy(train_cat$potential_issue)
x2 <- dummy(train_cat$deck_risk)
x3 <- dummy(train_cat$oe_constraint)
x4 <- dummy(train_cat$ppap_risk)
x5 <- dummy(train_cat$stop_auto_buy)
x6 <- dummy(train_cat$rev_stop)
x7 <- dummy(train_cat$went_on_backorder)

train_cat <- NULL
train_cat <- cbind(train_sku,x1,x2,x3,x4,x5,x6,x7)

#Now dummying the test_cat data as every factor has only 2 levels.
library(dummies)
x1 <- dummy(test_cat$potential_issue)
x2 <- dummy(test_cat$deck_risk)
x3 <- dummy(test_cat$oe_constraint)
x4 <- dummy(test_cat$ppap_risk)
x5 <- dummy(test_cat$stop_auto_buy)
x6 <- dummy(test_cat$rev_stop)
x7 <- dummy(test_cat$went_on_backorder)

test_cat <- NULL
test_cat <- cbind(test_sku,x1,x2,x3,x4,x5,x6,x7)

#Standardize the train_num and test_num

# Now taking the mean and sd of the train_num
train_mean <- apply(train_num,2,mean)
train_sd <- apply(train_num,2,sd)

# Standardizing the train_num
std_train <- sweep(sweep(train_num,2,train_mean),2,train_sd,"/")
train_standard <- data.frame(cbind(std_train,train_cat))
train_standard$load_time <- train_target_field
#train_standard is the final dataset for the train dataset

std_test <- sweep(sweep(test_num,2,train_mean),2,train_sd,"/")
test_standard <- data.frame(cbind(std_test,test_cat))
test_standard$load_time <- test_target_field
#test_standard is the final dataset for the test dataset

#Predicting load time from here on
model <- lm(formula = load_time ~ ., data = train_standard)
summary(model)
par(mfrow=c(2,2))
plot(model)
# y-hats is predicted value
train_model_preds <- predict(object = model, newdata = train_standard)
test_model_preds <- predict(object = model, newdata = test_standard)
# See for the error metric
DMwR::regr.eval(trues = train_standard$load_time, preds = train_model_preds)
DMwR::regr.eval(trues = test_standard$load_time, preds = test_model_preds)

