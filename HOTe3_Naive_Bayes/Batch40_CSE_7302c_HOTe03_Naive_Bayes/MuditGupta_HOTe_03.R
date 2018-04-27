rm(list=ls())

data <- read.csv('germandata.csv')
str(data)
summary(data)

#EDA
sapply(data, function(x) sum(is.na(x)))

#giving column heading
colnames(data) <- c('status_of_existing_checking_account','duration_in_month','credit_history',
                    'purpose','credit_amount','saving_account_bonds','present_installment_since',
                    'install_rate_in_percent',
                    'personal_status_sex','debtors_guarantors','present_residence_since',
                    'property','age','other_install','housing','existing_credits','job',
                    'no_of_people_liable','telephone','foreign_worker','target')

names(data)
head(data)
str(data)

# Now let us build Naive Bayes logistic regression model
# Sample the rows
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,round(nrow(data)*0.6))
data_train = data[trainRows,] 
data_test = data[-trainRows,] 

# Build a model
library(e1071)
model = naiveBayes(target ~ ., data = data_train)
model

pred = predict(model, data_train)
table(pred, data_train$target)

pred = predict(model, data_test)
table(pred, data_test$target)

