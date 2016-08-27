#### week 9 ####
############### C50 관련 패키지 #####################
# install.packages("caret", dependencies = TRUE)
library(caret)
# install.packages('pbkrtest')  ## caret 이 설치안될 경우
library(pbkrtest)
# install.packages("C50")
library(C50)
# install.packages("ROCR")
library(ROCR)
#####################################################
############### NN(인공신경망) 관련 패키지 ##########
# install.packages("nnet")
library(nnet)
# install.packages("devtools")
library(devtools)
# install.packages("NeuralNetTools")
library(NeuralNetTools)
#####################################################
############## 앙상블 모델 : randomforest ###########
# install.packages("randomForest")
library(randomForest)

###### SVM ##################################
# install.packages('e1071', dependencies = TRUE)
library(e1071)
# install.packages("kernlab")
# library(kernlab)

###### glm ##################################
# install.packages("Matrix")
library(Matrix)
# install.packages("glmnet")
library(glmnet)
#####################################################
## 데이터 비율 지정 후 train/test set 으로 분리
get_train_test <- function(data, prob) {
  data$churn <- factor(data$churn)
  set.seed(1)
  inTrain <- createDataPartition(y=data$churn, p=prob, list=FALSE)
  
  transaction_data.train <- data[inTrain, ]
  transaction_data.test <- data[-inTrain, ] 
  
  dataList <- list("train" = transaction_data.train, "test" = transaction_data.test)
  return (dataList)
}

## 모델 가져오는 함수
get_model <- function(train_data, model_nm) {
  model = ""
  
  train <- as.data.frame(train_data)
  
  if (model_nm =="C50") { # C5.0 모델
    c5_options <- C5.0Control(winnow=FALSE, noGlobalPruning = FALSE)
    model <- C5.0(churn~., data=train, control=c5_options, rules = FALSE)
  } else if (model_nm == "NN") { # 인공신경망 모델
    model <- nnet(churn~., data=train, size=3, maxit=1000)
  } else if (model_nm == "RanFor") { # 앙상블 : randomforest 모델
    model <- randomForest(churn~., data=train, ntree = 10)
  } else if (model_nm == "SVM") {
    model <- svm(churn~., data=train, scale=FALSE) # SVM 모델
  } else if (model_nm == "glm") {   ## 로지스틱
    trainData <- sparse.model.matrix( ~. ,data = train_data[,-1]) ## Matrix 형태로 데이터 생성
    trainData_y <- train_data[,1]
    
    model <- cv.glmnet(x = trainData, y = trainData_y,  family="binomial", type.measure = "auc") # 로지스틱 모델
  }
  
  return (model)
}

# data_lst$train[,-1][,1]
## 예측 함수
func_prediction <- function(test_data, model, model_nm) {
  
  res_lst <- list()
  
  if (model_nm =="C50") { # C5.0 모델
    test_data$c5_pred <- predict(model, test_data, type="class")
    test_data$cb_pred_prob <- predict(model, test_data, type="prob")
    
    confusionMatrix(test_data$c5_pred, test_data$churn)
    
    pred <- prediction(test_data$cb_pred_prob[,2], test_data$churn)
    model.perf1 <- performance(pred, "tpr", "fpr") # ROC Chart
    model.perf2 <- performance(pred, "lift", "rpp") # Lift Chart
    
    res_lst <- list("predicted_data"= test_data, "pred"= pred, "model.perf1"= model.perf1, "model.perf2" = model.perf2)
    
  } else if (model_nm == "NN") { # 인공신경망 모델
    confusionMatrix(predict(model, newdata=test_data, type="class"), test_data$churn)
    
    pred<-prediction(predict(model, newdata=test_data, type="raw"), test_data$churn)
    model.perf1 <-performance(pred, "tpr", "fpr") # ROC-chart
    model.perf2 <-performance(pred, "lift", "rpp") # Lift chart
  
    res_lst <- list("predicted_data"= test_data, "pred"= pred, "model.perf1"= model.perf1, "model.perf2" = model.perf2)
    
  } else if (model_nm == "RanFor") { # 앙상블 : randomforest 모델
    test_data$rf_pred <- predict(model, test_data, type="response")
    test_data$rf_pred_prob <- predict(model, test_data, type="prob")
    
    confusionMatrix(test_data$rf_pred, test_data$churn)
    
    pred <- prediction(test_data$rf_pred_prob[,2], test_data$churn)
    model.perf1 <- performance(pred, "tpr", "fpr") # ROC Chart
    model.perf2 <- performance(pred, "lift", "rpp") # Lift Chart
    
    res_lst <- list("predicted_data"= test_data, "pred"= pred, "model.perf1"= model.perf1, "model.perf2" = model.perf2)
    
  } else if (model_nm == "SVM") { # SVM : Support Vector Machine
    
    predict <- predict(svm_model, test_data, decision.values = TRUE)
    
    probs <- attr(predict, "decision.values")
    class <- predict(model, test_data, type="class")
    
    confusionMatrix(class, test_data$churn)
    
    pred <- prediction(svm.probs , data_lst$test$churn)
    model.perf1 <- performance(pred, "tpr", "fpr") # ROC Chart
    model.perf2 <- performance(pred, "lift", "rpp") # Lift Chart
    
    res_lst <- list("predicted_data"= test_data, "pred"= pred, "model.perf1"= model.perf1, "model.perf2" = model.perf2)
    
  } else if (model_nm == "glm") {   ## 로지스틱
    
    tmpTestData <- sparse.model.matrix( ~. , data= test_data[,-1])
    tmpTestData_y <- test_data[,1]
    
    tmPpred <- predict(model, tmpTestData, type="class")
    tmPpred_prob <- predict(model,type="response", newx = tmpTestData, s='lambda.min')
    
    confusionMatrix(tmPpred, test_data$churn)
    
    pred <- prediction(tmPpred_prob, testData_y)
    model.perf1 <- performance(glm.pred, "tpr", "fpr") # ROC Chart
    model.perf2 <- performance(glm.pred, "lift", "rpp") # Lift Chart
    
    res_lst <- list("predicted_data"= test_data, "pred"= pred, "model.perf1"= model.perf1, "model.perf2" = model.perf2)
  }
  
  return (res_lst)
}

###########################################################################
transaction_data <- read.csv('transactions.csv', stringsAsFactors = FALSE)
head(transaction_data)
## train & test dataset 생성 및 모델 생성 ################
data_lst <- get_train_test(transaction_data, 0.6)
c5_model <- get_model(data_lst$train[,-1], "C50")
nn_model <- get_model(data_lst$train[,-1], "NN")
rF_model <- get_model(data_lst$train[,-1], "RanFor")
svm_model <- get_model(data_lst$train[,-1], "SVM")
glm_model <- get_model(data_lst$train[,-1], "glm")
##########################################################

## 구한 모델에 대한 그래프 ###############################

plot(C5.0(churn~., data=data_lst$train[,-1], control=c5_options, rules = FALSE)) # C5.0 그래프
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nn_model) # 인공신경망 그래프
garson(nn_model) +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # 인공신경망 변수중요도 그래프(garson 알고리즘)
varImpPlot(rF_model) # RandomForest 변수중요도 그래프

## ppt 만들기 위한 모델별 summary 저장
sink(file="C5.0_summary_prob07.txt")
summary(c5_model)
sink()
sink(file="rF_summary07.txt")
summary(rF_model)
sink()
sink(file="svm_summary07.txt")
summary(svm_model)
sink()
sink(file="glm_summary07.txt")
glm_model
sink()
#######################################
##########################################################

## 예측 ##################################################
c5_prediction <- func_prediction(data_lst$test[,-1], c5_model, "C50")
nn_prediction <- func_prediction(data_lst$test[,-1], nn_model, "NN")
rF_prediction <- func_prediction(data_lst$test[,-1], rF_model, "RanFor")
svm_prediction <- func_prediction(data_lst$test[,-1], svm_model, "SVM" )
glm_prediction <- func_prediction(data_lst$test[,-1], glm_model, "glm")
##########################################################

################ 각 모델 비교 평가 #######################
plot(c5_prediction$model.perf1, col="red")
plot(nn_prediction$model.perf1, col="blue", add=T)
plot(rF_prediction$model.perf1, col="green", add=T)
plot(svm_prediction$model.perf1, col="pink", add=T)
plot(glm_prediction$model.perf1, col="orange", add=T)
legend(0.7,0.9,c("C5","NN", "RdmF", "SVM", "glm"),cex=0.6,col=c("red","blue", "green", "pink", "orange"),lty=1)
# gains(data_lst$test[,-1], rF_prediction$predicted_data$rf_pred_prob)

## FPR 이 0.2인 경우는 glm >SVM > NN > RandomForest > C5.0 순이지만, TPR의 비율이 증가 할 수록 glm > SVM > RandomForest > NN > C5.0 순이다.
## 이에 대한 면적(AUC) 값을 살펴보면 다음과 같다. (SVM > RandomForest > NN > C5.0)
performance(c5_prediction$pred, "auc")@y.values[[1]]; performance(nn_prediction$pred, "auc")@y.values[[1]]; performance(rF_prediction$pred, "auc")@y.values[[1]]; performance(svm_prediction$pred, "auc")@y.values[[1]];performance(glm_prediction$pred, "auc")@y.values[[1]];
##########################################################