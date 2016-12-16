
My.Location <- "C:/Users/Abheek Sinha/study materials/Business Analytics MSBA - San Diego/6.Collecting and Analyzing large datasets/DataDriven-Competition"
setwd(My.Location)
require(caret)

train <- read.csv("myTrain1.csv")
test <- read.csv("myTest1.csv")
unique(train$scheme_management)
train$scheme_management <- as.character(train$scheme_management)
train$scheme_management <- ifelse(is.na(train$scheme_management),"Other",train$scheme_management)
train$scheme_management <- ifelse(train$scheme_management == "","Other",train$scheme_management)
train$scheme_management <- as.factor(train$scheme_management)

test$scheme_management <- as.character(test$scheme_management)
test$scheme_management <- ifelse(is.na(test$scheme_management),"Other",test$scheme_management)
test$scheme_management <- ifelse(test$scheme_management == "","Other",test$scheme_management)
test$scheme_management <- as.factor(test$scheme_management)
table(train$scheme_management)
table(test$scheme_management)

missing.records <- train[!complete.cases(train),]

table(train$public_meeting)
table(train$permit)
table(test$public_meeting)
table(train$permit)
str(train$public_meeting)

train$public_meeting <- as.character(train$public_meeting)
train$public_meeting <- ifelse(train$public_meeting == "","False",train$public_meeting)
train$public_meeting <- as.factor(train$public_meeting)

test$public_meeting <- as.character(test$public_meeting)
test$public_meeting <- ifelse(test$public_meeting == "","False",test$public_meeting)
test$public_meeting <- as.factor(test$public_meeting)

train$permit <- as.character(train$permit)
train$permit <- ifelse(train$permit == "","False",train$permit)
train$permit <- as.factor(train$permit)

test$permit <- as.character(test$permit)
test$permit <- ifelse(test$permit == "","False",test$permit)
test$permit <- as.factor(test$permit)

table(train$permit)
table(test$permit)
table(train$public_meeting)
table(test$public_meeting)

missing.records <- train[!complete.cases(train),]

str(train)

# dummies <- dummyVars(id ~ ., data = train)
# train2 <- as.data.frame(predict(dummies, newdata = train))
# train2 <- cbind(id=train$id, train2)
# test <- as.data.frame(predict(dummies, newdata = test))
# test <- cbind(id=test$id, test)

str(train)

train_label <- read_csv('Training_set_Labels.csv')
train <- train %>%
  left_join(train_label,by="id")

# str(train$status_group)
train$status_group <- as.factor(train$status_group)

# install.packages("flexclust")
library("flexclust")
classification.table <- matrix(ncol=5, nrow=0)
# colnames(classification.table) <- c("K","mtry","ntree","dev.rate","val.rate")
for (k1 in 10:30){
  
    cl1 = kcca(cbind(train$latitude, train$longitude), k=k1, kccaFamily("kmeans"))
    # cl1
    pred_train <- predict(cl1)
    # image(cl1)
    # points(cbind(train$latitude, train$longitude), col=pred_train, pch=19, cex=0.3)
    pred_train <- as.data.frame(pred_train)
    pred_train$columnid <- c(1:nrow(pred_train))
    train$columnid <- c(1:nrow(train))
    train <- train %>%
      left_join(pred_train,by="columnid")
    train$kmeans_class <- as.factor(train$pred_train)  
    train <- train[,-(ncol(train)-1)]
    train <- train[,-(ncol(train)-1)]
    
    # Predicting Test clusters 
    pred_test <- predict(cl1, newdata=cbind(test$latitude, test$longitude))
    pred_test <- as.data.frame(pred_test)
    pred_test$columnid <- c(1:nrow(pred_test))
    test$columnid <- c(1:nrow(test))
    test <- test %>%
      left_join(pred_test,by="columnid")
    test$kmeans_class <- as.factor(test$pred_test)  
    test <- test[,-(ncol(test)-1)]
    test <- test[,-(ncol(test)-1)]
    
    # str(train$`funder.Government Of Tanzania`)
    # 
    # names(train) <- gsub(" ", ".", names(train))
    # names(train) <- gsub("/", "_", names(train))
    # names(train) <- gsub("-", "_", names(train))
    # 
    # names(test) <- gsub(" ", ".", names(test))
    # names(test) <- gsub("/", "_", names(test))
    # names(test) <- gsub("-", "_", names(test))
    
    set.seed(123)
    d <- sort(sample(nrow(train), nrow(train)*0.94))
    dev<-train[d,]
    val<-train[-d,]
    # table(train$status_group)
    
    ### Random Forest
    # library(randomForest)
    # library(matlab)
    # library(e1071)
    # library(caret)
    # library(deepnet)
    # library(gbm)
    # library(aod) # For chi square test
    # 
    # library(ROCR) # For ROC, AUC calculation
    # library(rms)
    # library(dtplyr)
    # library(tidyverse)
    
    for (mtry in 3:7){
      for (ntree in 100:400){
          rf = randomForest(status_group~. , dev[,-c(1,6,7)], mtry=mtry, ntree=ntree)
          rf_predictions = predict(rf,val[,-c(1,6,7)], type="class")
          #rf_predictions
          # table(val$status_group, rf_predictions)
          m <- table(val$status_group, rf_predictions)
          val.rate <- (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
          
          rf_predictions = predict(rf,dev[,-c(1,6,7)], type="class")
          #rf_predictions
          m <- table(dev$status_group, rf_predictions)
          dev.rate<-(m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
          
          
          classification.table <- rbind(classification.table, c(k1,mtry, ntree, val.rate, dev.rate))


    }
}
test <- test[,-(ncol(test))]
train <- train[,-(ncol(train))]
}

classification.table <data.frame(classification.table)

colnames(classification.table) <- c("K","mtry","ntree","dev.rate","val.rate")
write.csv(classification.table, "classification.table1.csv", row.names=FALSE)





################################### End ###########################

install.packages("mlbench")
library(mlbench)
library(caret)

  k1 <- 10
  cl1 = kcca(cbind(train$latitude, train$longitude), k=k1, kccaFamily("kmeans"))
  # cl1
  pred_train <- predict(cl1)
  # image(cl1)
  # points(cbind(train$latitude, train$longitude), col=pred_train, pch=19, cex=0.3)
  pred_train <- as.data.frame(pred_train)
  pred_train$columnid <- c(1:nrow(pred_train))
  train$columnid <- c(1:nrow(train))
  train <- train %>%
    left_join(pred_train,by="columnid")
  train$kmeans_class <- as.factor(train$pred_train)  
  train <- train[,-(ncol(train)-1)]
  train <- train[,-(ncol(train)-1)]
  
  # Predicting Test clusters 
  pred_test <- predict(cl1, newdata=cbind(test$latitude, test$longitude))
  pred_test <- as.data.frame(pred_test)
  pred_test$columnid <- c(1:nrow(pred_test))
  test$columnid <- c(1:nrow(test))
  test <- test %>%
    left_join(pred_test,by="columnid")
  test$kmeans_class <- as.factor(test$pred_test)  
  test <- test[,-(ncol(test)-1)]
  test <- test[,-(ncol(test)-1)]
  
  # str(train$`funder.Government Of Tanzania`)
  # 
  # names(train) <- gsub(" ", ".", names(train))
  # names(train) <- gsub("/", "_", names(train))
  # names(train) <- gsub("-", "_", names(train))
  # 
  # names(test) <- gsub(" ", ".", names(test))
  # names(test) <- gsub("/", "_", names(test))
  # names(test) <- gsub("-", "_", names(test))
  
  set.seed(123)
  d <- sort(sample(nrow(train), nrow(train)*0.5))
  dev<-train[d,]
  val<-train[-d,]
  # table(train$status_group)
  
  ### Random Forest
  # library(randomForest)
  # library(matlab)
  # library(e1071)
  # library(caret)
  # library(deepnet)
  # library(gbm)
  # library(aod) # For chi square test
  # 
  # library(ROCR) # For ROC, AUC calculation
  # library(rms)
  # library(dtplyr)
  # library(tidyverse)
  
  
  # Create model with default paramters
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  seed <- 7
  metric <- "Accuracy"
  set.seed(seed)
  mtry <- sqrt(ncol(dev))
  tunegrid <- expand.grid(.mtry=mtry)
  rf_default <- train(status_group~., data=dev[,-c(1,6,7)], method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  print(rf_default)
  
  
  
  
  for (mtry in 3:3){
    for (ntree in 123:123){
      rf = randomForest(status_group~. , dev[,-c(1,6,7)], mtry=mtry, ntree=ntree)
      rf_predictions = predict(rf,val[,-c(1,6,7)], type="class")
      #rf_predictions
      # table(val$status_group, rf_predictions)
      m <- table(val$status_group, rf_predictions)
      val.rate <- (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
      
      rf_predictions = predict(rf,dev[,-c(1,6,7)], type="class")
      #rf_predictions
      m <- table(dev$status_group, rf_predictions)
      dev.rate<-(m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
      
      
      classification.table <- rbind(classification.table, c(k1,mtry, ntree, val.rate, dev.rate))
      
      
    }
  }
  test <- test[,-(ncol(test))]
  train <- train[,-(ncol(train))]











# (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])

#Test Prediction
# str(dev)
# str(test)
# is.fact <- sapply(train, is.factor)
# table(dev$public_meeting)
# table(test$public_meeting)
# 
rf_predictions = predict(rf,test[,-c(1,6,7)], type="class")
str(rf_predictions)
output_test <- cbind(test$id,as.character(rf_predictions))
colnames(output_test) <- c("id","status_group")
write.csv(output_test, "SubmissionFormat_LaCanyon.csv", row.names=FALSE)























################### NeuralNet Models ##############################
library(nnet)

weighted.acc <- function(predictions, actual)
{
  freqs <- as.data.frame(table(actual))
  tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
  tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
  acc.pc <- tab[,1]/freqs[,2]
  return(sum(acc.pc)/length(acc.pc))
}

results <- matrix(ncol=6, nrow=0)
models <- list()

cw1 <- rep(1, 3) # all equal
cw2 <- c(10, 100, 10) # powers of 10
freqs <- as.data.frame(table(dev$status_group))
cw3 <- cbind(freqs[1], apply(freqs, 1, function(s) { length(dev[,34])/as.integer(s[2])})) # 1/counts

class.weights <- rbind(cw1, cw2, cw3[,2])
colnames(class.weights) <- c("functional", "functional needs repair", "non functional")

c <- 3

data.weights <- do.call(rbind, Map(function(s)
{
  class.weights[c,s]
}, dev$status_group))

h<- 20
  ann <- nnet(status_group~., data=dev[,-c(1,6,7)], weights=data.weights[], size=h, decay=5e-1, maxit=200,MaxNWts = 7000)
  
  pred <- predict(ann, dev[,-c(1,6,7)], type="class")
  tacc <- weighted.acc(pred, dev[,34]) # weighted training accuracy
  
  m<-table(dev$status_group, pred)
  (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
  
  pred <- predict(ann, val[,-c(1,6,7)], type="class")
  wacc <- weighted.acc(pred, val[,34]) # weighted test accuracy
  
  m<-table(val$status_group, pred)
  (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
  
  results <- rbind(results, c(h, tacc, wacc, c))
  models[[(length(models)+1)]] <- ann

  table(dev$status_group, pred)
  m<-table(dev$status_group, pred)
  (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])






for (i in 1:3)
{
  
  for (c in 1:length(class.weights[,1]))
  {
    
    data.weights <- do.call(rbind, Map(function(s)
    {
      class.weights[c,s]
    }, dev$status_group))
    
    for (h in 2:30)
    {
      
      ann <- nnet(status_group~., data=dev[,-c(1,6,7)], weights=data.weights[], size=h, decay=5e-4, maxit=200)
      
      pred <- predict(ann, dev[,-c(1,6,7)], type="class")
      tacc <- weighted.acc(pred, dev[,34]) # weighted training accuracy
      pred <- predict(ann, dev[,-c(1,6,7)], type="class")
      wacc <- weighted.acc(pred, val[,34]) # weighted test accuracy
      
      results <- rbind(results, c(h, tacc, wacc, c))
      models[[(length(models)+1)]] <- ann
    }
  }
}

seedsANN =nnet(status_group~., dev[,-c(1,6,7)],size=4, decay=5e-4, maxit=200)
pred <- predict(seedsANN, val[,-c(1,6,7)], type="class")
table(val$status_group, pred)
m<-table(val$status_group, pred)
(m[1,1]+m[3,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1]+m[3,1]+m[3,2])
tacc <- weighted.acc(pred, val[,34])

################### XGBoost Models ##############################
# load data
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
# fit model
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nround = 2,
               nthread = 2, objective = "binary:logistic")
# predict
pred <- predict(bst, test$data)
Julia




################### NeuralNet Models ##############################
install.packages("neuralnet")
library(nnet)
library(neuralnet)
data("irisdata")
seeds<-cbind(train[,-c(1,6,7,34)], class.ind(seeds$status_group))
names(seeds) <- gsub(" ", ".", names(seeds))
seedstrain<- sample(1:59400,50000)
seedstest <- setdiff(1:59400,seedstrain)
colnames(seeds)
seedsANN = nnet(functional + functional.needs.repair + non.functional ~ ., seeds[seedstrain,],size=6,MaxNWts = 84581)

table(predict(seedsANN, seeds[seedstrain,]))
table(predict(seedsANN, seeds[seedstest,-c(31:33)]),seeds[seedstest,]$class)

seedstrain<- sample(1:59400,50000)
seedstest <- setdiff(1:59400,seedstrain)
ideal <- class.ind(seeds$status_group)

head(ideal)
nrow(ideal)
nrow(seeds[seedstrain,-31])
nrow(seeds[seedstest,-31])
str(seeds[seedstrain,-31])
?class.ind
?nnet
seedsANN = nnet(status_group~., seeds[seedstrain,], size=6, softmax=TRUE)
seedsANN = nnet(seeds[seedstrain,-31], ideal[seedstrain,], size=6, softmax=TRUE,na.action=na.omit)
predict(seedsANN, seeds[seedstrain,-31], type="class")
table(predict(seedsANN, seeds[seedstest,-31], type="class"),seeds[seedstest,]$class)


data(iris)
library(nnet)
trainData <- cbind(iris[, 1:4], class.ind(iris$Species))
neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainData)

################### ADA Boost Models ##############################

install.packages("ada")
library("rpart")
library("ada")
n <- nrow(train)
set.seed(100)
ind <- sample(1:n)
trainval <- ceiling(n * .5)
testval <- ceiling(n * .3)
train2 <- train[ind[1:trainval],]
test2 <- train[ind[(trainval + 1):(trainval + testval)],]
valid <- train[ind[(trainval + testval + 1):n],]

control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
gen1 <- ada(status_group~., data = train2[,-c(1,6,7)], test.x = test2[,-c(1,6,7,34)], test.y = test2[,34], type = "gentle", control = control, iter = 70)
gen1 <- addtest(gen1, valid[,-73], valid[,73])
summary(gen1)
varplot(gen1)




install.packages("ada")
library("rpart")
library("ada")
data("soldat")
n <- nrow(soldat)
set.seed(100)
ind <- sample(1:n)
trainval <- ceiling(n * .5)
testval <- ceiling(n * .3)
train2 <- soldat[ind[1:trainval],]
test2 <- soldat[ind[(trainval + 1):(trainval + testval)],]
valid <- soldat[ind[(trainval + testval + 1):n],]

control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
gen1 <- ada(y~., data = train2[,-c(1,6,7)], test.x = test2[,-c(1,6,7,34)], test.y = test2[,34], type = "gentle", control = control, iter = 70)
gen1 <- addtest(gen1, valid[,-73], valid[,73])
summary(gen1)
varplot(gen1)

################### Decision Tree Models ##############################
library(rpart)
treeimb <- rpart(status_group ~ ., data = dev[,-c(1,6,7)])
pred.treeimb <- predict(treeimb, newdata = val[,-c(1,6,7)])
table(val$status_group, pred.treeimb)
m<-table(val$status_group, rf_predictions)
(m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])

accuracy.meas(test_cancellation_final$booking_status_0_1, pred.treeimb[,2])
roc.curve(test_cancellation_final$booking_status_0_1, pred.treeimb[,2], plotit = T)

################### SVM ##############################
set.seed(123)
tune.out=tune(svm, status_group ~ ., data = dev[,-c(1,6,7)], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)

svmfit=svm(status_group ~ ., data = dev[,-c(1,6,7)], kernel="radial",gamma=1,cost=1e5)
# plot(svmfit,training_cancellation_final[keep_var])
summary(svmfit)

###### Predicting Training fit
pred=predict(svmfit,newdata=training_cancellation_final[keep_var])
true=training_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(training_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
accuracy.meas(training_cancellation_final$booking_status_0_1, pred)
roc.curve(training_cancellation_final$booking_status_0_1, pred, plotit = T)

###### Predicting Test fit
pred=predict(svmfit,newdata=test_cancellation_final[keep_var])
true=test_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(test_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
accuracy.meas(test_cancellation_final$booking_status_0_1, pred)
roc.curve(test_cancellation_final$booking_status_0_1, pred, plotit = T)

###### Predicting Test fit from the best model
pred=predict(tune.out$best.model,test_cancellation_final[keep_var])
true=test_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(test_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
# accuracy.meas(test_cancellation_final$booking_status_0_1, pred)
roc.curve(test_cancellation_final$booking_status_0_1, pred, plotit = T)

write.csv(train, "myTrainDummy.csv", row.names=FALSE)
write.csv(test, "myTestDummy.csv", row.names=FALSE)
