#####################################Problem 1###################################
library(readr)
diabetes <- read.csv("C:\\Users\\hp\\Desktop\\ensemble assi\\Diabeted_Ensemble.csv" , stringsAsFactors = TRUE)
str(diabetes)

#normalize the data
diabetes[, 1:8] <- scale(diabetes[,1:8])

#data partitioning
library(caret)
split <- createDataPartition(diabetes$Class.variable, p = 0.7, list = F)
training <- diabetes[split, ]
testing <- diabetes[-split, ]

#Bagging technique
#randomforest
install.packages("randomForest")
library(randomForest)
library(C50)

test_pred1 <- c()
test_pred2 <- c()
test_acc1 <- c()
test_acc2 <- c()
train_pred1 <- c()
train_pred2 <- c()
train_acc1 <- c()
train_acc2 <- c()
for(i in 1:10) {
#building model for DT and random forest
dt_bagging <- C5.0(Class.variable ~ ., data = training)
rf_bagging <- randomForest(Class.variable ~ ., data = training, mtry = 7)

#predicting for DT and random forest models on test data
test_pred1 <- predict(dt_bagging, testing)
test_pred2 <- predict(rf_bagging, testing)

#test accuracy for DT model and Random forest model
test_acc1 <- c(test_acc1, mean(testing$Class.variable == test_pred1))
test_acc2 <- c(test_acc2, mean(testing$Class.variable == test_pred2))

#predicting for DT and random forest models on train data
train_pred1 <- predict(dt_bagging, training)
train_pred2 <- predict(rf_bagging, training)

#train accuracy for DT model and Random forest model
train_acc1 <- c(train_acc1,mean(training$Class.variable == train_pred1))
train_acc2 <- c(train_acc2,mean(training$Class.variable == train_pred2))
}

#all test accuracy and mean test accuracy for DT model
test_acc1
mean(test_acc1)

#all train accuracy and mean train accuracy for DT model
train_acc1
mean(train_acc1)

#all test accuracy and mean test accuracy for RF model
test_acc2
mean(test_acc2)

#all train accuracy and mean train accuracy for RF model
train_acc2
mean(train_acc2)

#Boosting technique
#adaboosting
install.packages("adabag")
library(adabag)

#building ada boosting model
adaboost <- boosting(Class.variable ~ ., data = training, boos = TRUE)

# Test data
adaboost_test <- predict(adaboost, testing)

table(adaboost_test$class, testing$Class.variable)
mean(adaboost_test$class == testing$Class.variable)

# Train data
adaboost_train <- predict(adaboost, training)

table(adaboost_train$class, training$Class.variable)
mean(adaboost_train$class == training$Class.variable)

#XG boosting
diabetes <- read.csv("C:\\Users\\hp\\Desktop\\ensemble assi\\Diabeted_Ensemble.csv" , stringsAsFactors = TRUE)

str(diabetes)
library(caTools)
set.seed(1234)
split <- sample.split(diabetes$Class.variable, SplitRatio = 0.8)
train <- subset(diabetes, split == TRUE)
test <- subset(diabetes, split == FALSE)

install.packages("xgboost")
library(xgboost)

train_y <- train$Class.variable == "YES"
# create matrix on train attributes
train_x <- model.matrix(train$Class.variable ~ . -1, data = train)

test_y <- test$Class.variable == "YES"
# create matrix on test attributes
test_x <- model.matrix(test$Class.variable ~ .-1, data = test)

# DMatrix on train
Xmatrix_train <- xgb.DMatrix(data = train_x, label = train_y)
# DMatrix on test 
Xmatrix_test <- xgb.DMatrix(data = test_x, label = test_y)


# Max number of boosting iterations - nround
xg_boosting <- xgboost(data = Xmatrix_train, nround = 50,
                       objective = "multi:softmax", eta = 0.3, 
                       num_class = 2, max_depth = 100)

# Prediction for test data
xgbpred_test <- predict(xg_boosting, Xmatrix_test)
table(test_y, xgbpred_test)
mean(test_y == xgbpred_test)

# Prediction for train data
xgbpred_train <- predict(xg_boosting, Xmatrix_train)
table(train_y, xgbpred_train)
mean(train_y == xgbpred_train)

#voting technique
diabetes <- read.csv("C:\\Users\\hp\\Desktop\\ensemble assi\\Diabeted_Ensemble.csv" , stringsAsFactors = TRUE)
set.seed(12345)
Train_Test <- sample(c("Train", "Test"), nrow(diabetes), replace = TRUE, prob = c(0.7, 0.3))
Train <- diabetes[Train_Test == "Train",]
TestX <- within(diabetes[Train_Test == "Test", ], rm("Class.variable"))
TestY <- diabetes[Train_Test == "Test", "Class.variable"]

library(randomForest)
# Random Forest Analysis
diabetes_RF <- randomForest(Class.variable ~ ., data = Train, keep.inbag = TRUE, ntree = 500)

# Overall class prediction (hard voting)
diabetes_RF_Test_Margin <- predict(diabetes_RF, newdata = TestX, type = "class")

# Prediction
diabetes_RF_Test_Predict <- predict(diabetes_RF, newdata = TestX, type = "class", predict.all = TRUE)

sum(diabetes_RF_Test_Margin == diabetes_RF_Test_Predict$aggregate)
head(diabetes_RF_Test_Margin == diabetes_RF_Test_Predict$aggregate)

# Majority Voting
dim(diabetes_RF_Test_Predict$individual)

Row_Count_Max <- function(x) names(which.max(table(x)))

Voting_Predict <- apply(diabetes_RF_Test_Predict$individual, 1, Row_Count_Max)

head(Voting_Predict)
tail(Voting_Predict)

all(Voting_Predict == diabetes_RF_Test_Predict$aggregate)
all(Voting_Predict == diabetes_RF_Test_Margin)

#accuracy of model on test data
mean(Voting_Predict == TestY)

##############################Problem 2##############################################

library(readr)
tumor_data <- read.csv("C:\\Users\\hp\\Desktop\\ensemble assi\\Tumor_Ensemble.csv" , stringsAsFactors = TRUE)
tumor_data <- tumor_data[,-1]

str(tumor_data)
#normalize the data
tumor_data[, -1] <- scale(tumor_data[,-1])

#data partitioning
library(caret)
split <- createDataPartition(tumor_data$diagnosis, p = 0.7, list = F)
training <- tumor_data[split, ]
testing <- tumor_data[-split, ]

#Bagging technique
#randomforest
install.packages("randomForest")
library(randomForest)
library(C50)

test_pred1 <- c()
test_pred2 <- c()
test_acc1 <- c()
test_acc2 <- c()
train_pred1 <- c()
train_pred2 <- c()
train_acc1 <- c()
train_acc2 <- c()
for(i in 1:10) {
  #building model for DT and random forest
  dt_bagging <- C5.0(diagnosis ~ ., data = training)
  rf_bagging <- randomForest(diagnosis ~ ., data = training, mtry = 7)
  
  #predicting for DT and random forest models on test data
  test_pred1 <- predict(dt_bagging, testing)
  test_pred2 <- predict(rf_bagging, testing)
  
  #test accuracy for DT model and Random forest model
  test_acc1 <- c(test_acc1, mean(testing$diagnosis == test_pred1))
  test_acc2 <- c(test_acc2, mean(testing$diagnosis == test_pred2))
  
  #predicting for DT and random forest models on train data
  train_pred1 <- predict(dt_bagging, training)
  train_pred2 <- predict(rf_bagging, training)
  
  #train accuracy for DT model and Random forest model
  train_acc1 <- c(train_acc1,mean(training$diagnosis == train_pred1))
  train_acc2 <- c(train_acc2,mean(training$diagnosis == train_pred2))
}

#all test accuracy and mean test accuracy for DT model
test_acc1
mean(test_acc1)

#all train accuracy and mean train accuracy for DT model
train_acc1
mean(train_acc1)

#all test accuracy and mean test accuracy for RF model
test_acc2
mean(test_acc2)

#all train accuracy and mean train accuracy for RF model
train_acc2
mean(train_acc2)

#Boosting technique
#adaboosting
install.packages("adabag")
library(adabag)

#building ada boosting model
adaboost <- boosting(diagnosis ~ ., data = training, boos = TRUE)

# Test data
adaboost_test <- predict(adaboost, testing)

table(adaboost_test$class, testing$diagnosis)
mean(adaboost_test$class == testing$diagnosis)

# Train data
adaboost_train <- predict(adaboost, training)

table(adaboost_train$class, training$diagnosis)
mean(adaboost_train$class == training$diagnosis)

#XG boosting
tumor_data <- read.csv("C:\\Users\\hp\\Desktop\\ensemble assi\\Tumor_Ensemble.csv" , stringsAsFactors = TRUE)

str(tumor_data)
library(caTools)
set.seed(1234)
split <- sample.split(tumor_data$diagnosis, SplitRatio = 0.8)
train <- subset(tumor_data, split == TRUE)
test <- subset(tumor_data, split == FALSE)

install.packages("xgboost")
library(xgboost)

train_y <- train$diagnosis == "M"
# create matrix on train attributes
train_x <- model.matrix(train$diagnosis ~ . -1, data = train)

test_y <- test$diagnosis == "M"
# create matrix on test attributes
test_x <- model.matrix(test$diagnosis ~ .-1, data = test)

# DMatrix on train
Xmatrix_train <- xgb.DMatrix(data = train_x, label = train_y)
# DMatrix on test 
Xmatrix_test <- xgb.DMatrix(data = test_x, label = test_y)


# Max number of boosting iterations - nround
xg_boosting <- xgboost(data = Xmatrix_train, nround = 50,
                       objective = "multi:softmax", eta = 0.3, 
                       num_class = 2, max_depth = 100)

# Prediction for test data
xgbpred_test <- predict(xg_boosting, Xmatrix_test)
table(test_y, xgbpred_test)
mean(test_y == xgbpred_test)

# Prediction for train data
xgbpred_train <- predict(xg_boosting, Xmatrix_train)
table(train_y, xgbpred_train)
mean(train_y == xgbpred_train)

#voting technique
tumor_data <- read.csv("C:\\Users\\hp\\Desktop\\ensemble assi\\Tumor_Ensemble.csv" , stringsAsFactors = TRUE)
set.seed(12345)
Train_Test <- sample(c("Train", "Test"), nrow(tumor_data), replace = TRUE, prob = c(0.7, 0.3))
Train <- tumor_data[Train_Test == "Train",]
TestX <- within(tumor_data[Train_Test == "Test", ], rm("diagnosis"))
TestY <- tumor_data[Train_Test == "Test", "diagnosis"]

library(randomForest)
# Random Forest Analysis
tumor_RF <- randomForest(diagnosis ~ ., data = Train, keep.inbag = TRUE, ntree = 500)

# Overall class prediction (hard voting)
tumor_RF_Test_Margin <- predict(tumor_RF, newdata = TestX, type = "class")

# Prediction
tumor_RF_Test_Predict <- predict(tumor_RF, newdata = TestX, type = "class", predict.all = TRUE)

sum(tumor_RF_Test_Margin == tumor_RF_Test_Predict$aggregate)
head(tumor_RF_Test_Margin == tumor_RF_Test_Predict$aggregate)

# Majority Voting
dim(tumor_RF_Test_Predict$individual)

Row_Count_Max <- function(x) names(which.max(table(x)))

Voting_Predict <- apply(tumor_RF_Test_Predict$individual, 1, Row_Count_Max)

head(Voting_Predict)
tail(Voting_Predict)

all(Voting_Predict == tumor_RF_Test_Predict$aggregate)
all(Voting_Predict == tumor_RF_Test_Margin)

#accuracy of model on test data
mean(Voting_Predict == TestY)

###############################################problem 3#######################################
library(readr)

cocoa_data <- readxl::read_xlsx("C:\\Users\\hp\\Desktop\\ensemble assi\\Coca_Rating_Ensemble.xlsx")

#removing unwanted data
cocoa_data <- cocoa_data[, -c(1,2,4,6,8,9)]

cocoa_data$Rating <- as.factor(cocoa_data$Rating)
str(cocoa_data)

#data partitioning
library(caret)
split <- createDataPartition(cocoa_data$Rating, p = 0.7, list = F)
training <- cocoa_data[split, ]
testing <- cocoa_data[-split, ]

#Bagging technique
#randomforest
#install.packages("randomForest")
#install.packages("C50")
library(randomForest)
library(C50)


test_pred2 <- c()
test_acc1 <- c()
test_acc2 <- c()
train_pred1 <- c()
train_pred2 <- c()
train_acc1 <- c()
train_acc2 <- c()
for(i in 1:10) {
  #building model for DT and random forest
  dt_bagging <- C5.0(Rating ~ ., data = training)
  rf_bagging <- randomForest(Rating ~ ., data = training, mtry = 2)
  
  #predicting for DT and random forest models on test data
  test_pred1 <- predict(dt_bagging, testing)
  test_pred2 <- predict(rf_bagging, testing)
  
  #test accuracy for DT model and Random forest model
  test_acc1 <- c(test_acc1, mean(testing$Rating == test_pred1))
  test_acc2 <- c(test_acc2, mean(testing$Rating == test_pred2))
  
  #predicting for DT and random forest models on train data
  train_pred1 <- predict(dt_bagging, training)
  train_pred2 <- predict(rf_bagging, training)
  
  #train accuracy for DT model and Random forest model
  train_acc1 <- c(train_acc1,mean(training$Rating == train_pred1))
  train_acc2 <- c(train_acc2,mean(training$Rating == train_pred2))
}

#all test accuracy and mean test accuracy for DT model
test_acc1
mean(test_acc1)

#all train accuracy and mean train accuracy for DT model
train_acc1
mean(train_acc1)

#all test accuracy and mean test accuracy for RF model
test_acc2
mean(test_acc2)

#all train accuracy and mean train accuracy for RF model
train_acc2
mean(train_acc2)

#Boosting technique
#adaboosting
install.packages("adabag")
library(adabag)

#building ada boosting model
adaboost <- boosting(Rating ~ ., data = training, boos = TRUE)

# Test data
adaboost_test <- predict(adaboost, testing)

table(adaboost_test$class, testing$Rating)
mean(adaboost_test$class == testing$Rating)

# Train data
adaboost_train <- predict(adaboost, training)

table(adaboost_train$class, training$Rating)
mean(adaboost_train$class == training$Rating)

#voting technique
cocoa_data <- readxl::read_xlsx("C:\\Users\\hp\\Desktop\\ensemble assi\\Coca_Rating_Ensemble.xlsx")

#removing unwanted data
cocoa_data <- cocoa_data[, -c(1,2,4,6,8,9)]

cocoa_data$Rating <- as.factor(cocoa_data$Rating)
str(cocoa_data)

#data partitioning
set.seed(12345)
Train_Test <- sample(c("Train", "Test"), nrow(cocoa_data), replace = TRUE, prob = c(0.7, 0.3))
Train <- cocoa_data[Train_Test == "Train",]
TestX <- within(cocoa_data[Train_Test == "Test", ], rm("Rating"))
TestY <- cocoa_data[Train_Test == "Test", "Rating"]

library(randomForest)
# Random Forest Analysis
cocoa_RF <- randomForest(Rating ~ ., data = Train, keep.inbag = TRUE, ntree = 500)

# Overall class prediction (hard voting)
cocoa_RF_Test_Margin <- predict(cocoa_RF, newdata = TestX, type = "class")

# Prediction
cocoa_RF_Test_Predict <- predict(cocoa_RF, newdata = TestX, type = "class", predict.all = TRUE)

sum(cocoa_RF_Test_Margin == cocoa_RF_Test_Predict$aggregate)
head(cocoa_RF_Test_Margin == cocoa_RF_Test_Predict$aggregate)

# Majority Voting
dim(cocoa_RF_Test_Predict$individual)

Row_Count_Max <- function(x) names(which.max(table(x)))

Voting_Predict <- apply(cocoa_RF_Test_Predict$individual, 1, Row_Count_Max)

head(Voting_Predict)
tail(Voting_Predict)

all(Voting_Predict == cocoa_RF_Test_Predict$aggregate)
all(Voting_Predict == cocoa_RF_Test_Margin)

#accuracy of model on test data
mean(Voting_Predict == TestY)

###############################Problem 4#########################################################
library(readr)
password_data <- readxl::read_xlsx("C:\\Users\\hp\\Desktop\\ensemble assi\\Ensemble_Password_Strength.xlsx")

password_data$characters_strength <- as.factor(password_data$characters_strength)
str(password_data)

sum(is.na(password_data))
sum(is.null(password_data))

#data partitioning
library(caret)
split <- createDataPartition(password_data$characters_strength, p = 0.7, list = F)
training <- password_data[split, ]
testing <- password_data[-split, ]

#Bagging technique
#randomforest
install.packages("randomForest")
library(randomForest)
library(C50)


test_pred2 <- c()
test_acc2 <- c()
train_pred2 <- c()
train_acc2 <- c()
for(i in 1:10) {
  #building model for random forest
  rf_bagging <- randomForest(characters_strength ~ ., data = training)
  
  #predicting for random forest models on test data
  test_pred2 <- predict(rf_bagging, testing)
  
  #test accuracy for Random forest model
  test_acc2 <- c(test_acc2, mean(testing$characters_strength == test_pred2))
  
  #predicting for random forest models on train data
  train_pred2 <- predict(rf_bagging, training)
  
  #train accuracy forRandom forest model
  train_acc2 <- c(train_acc2,mean(training$characters_strength == train_pred2))
}

#all test accuracy and mean test accuracy for RF model
test_acc2
mean(test_acc2)

#all train accuracy and mean train accuracy for RF model
train_acc2
mean(train_acc2)

#Boosting technique
#adaboosting
install.packages("adabag")
library(adabag)

#building ada boosting model
adaboost <- boosting(characters_strength ~ ., data = training, boos = TRUE)

# Test data
adaboost_test <- predict(adaboost, testing)

table(adaboost_test$class, testing$characters_strength)
mean(adaboost_test$class == testing$characters_strength)

# Train data
adaboost_train <- predict(adaboost, training)

table(adaboost_train$class, training$characters_strength)
mean(adaboost_train$class == training$characters_strength)

#voting technique
password_data <- readxl::read_xlsx("C:\\Users\\hp\\Desktop\\ensemble assi\\Ensemble_Password_Strength.xlsx")
set.seed(12345)
Train_Test <- sample(c("Train", "Test"), nrow(password_data), replace = TRUE, prob = c(0.7, 0.3))
Train <- password_data[Train_Test == "Train",]
TestX <- within(password_data[Train_Test == "Test", ], rm("characters_strength"))
TestY <- password_data[Train_Test == "Test", "characters_strength"]

library(randomForest)
# Random Forest Analysis
pass_RF <- randomForest(characters_strength ~ ., data = Train, keep.inbag = TRUE, ntree = 500)

# Overall class prediction (hard voting)
pass_RF_Test_Margin <- predict(pass_RF, newdata = TestX, type = "class")

# Prediction
pass_RF_Test_Predict <- predict(pass_RF, newdata = TestX, type = "class", predict.all = TRUE)

sum(pass_RF_Test_Margin == pass_RF_Test_Predict$aggregate)
head(pass_RF_Test_Margin == pass_RF_Test_Predict$aggregate)

# Majority Voting
dim(pass_RF_Test_Predict$individual)

Row_Count_Max <- function(x) names(which.max(table(x)))

Voting_Predict <- apply(pass_RF_Test_Predict$individual, 1, Row_Count_Max)

head(Voting_Predict)
tail(Voting_Predict)

all(Voting_Predict == pass_RF_Test_Predict$aggregate)
all(Voting_Predict == pass_RF_Test_Margin)

#accuracy of model on test data
mean(Voting_Predict == TestY)

###########################################END#############################################