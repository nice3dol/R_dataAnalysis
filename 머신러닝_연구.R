##### 머신러닝 기반 데이터 분석 사례연구 #5 (A1팀) #####


### 1번. (분류기법 적용)


# 데이터 준비
raw.wisc <- read.csv("/rwork/wisc_bc_data.csv", header = T)
str(raw.wisc)
head(raw.wisc)

# 훈련, 테스트 데이터 준비
library(caret)
library(pROC)
library(RColorBrewer)

idx <- createDataPartition(raw.wisc$diagnosis, p = 0.7, list = F)
wisc.train <- raw.wisc[idx, ]
wisc.test <- raw.wisc[-idx, ]
table(wisc.train$diagnosis)
table(wisc.test$diagnosis)


## 1)나이브 베이즈 모형
library(e1071)

naive.model <- naiveBayes(wisc.train, wisc.train$diagnosis, laplace = 1)  # 모델링

naive.pred <- predict(naive.model, wisc.test, type = "class")  # 모델로 예측

nc <- confusionMatrix(naive.pred, wisc.test$diagnosis)  
nc
# 혼돈 매트릭스 결과 확인
#! 결과 : Accuracy : 0.9941

# 시각화
roc.naive <- roc(as.numeric(naive.pred), as.numeric(wisc.test$diagnosis))
plot.roc(roc.naive, col = "red", print.auc = T,
         max.auc.polygon = T, print.thres = T,
         print.thres.pch = 19, print.thres.col = "red",
         auc.polygon = T, auc.polygon.col="#D1F2EB")

plot(nc$table, col = brewer.pal(2, "Accent"))


## 2)로지스틱 회귀분석
library(nnet)  # 다항 로지스틱 회귀를 위한 패키지

logit.model <- multinom(diagnosis ~ ., data = wisc.train)  # 훈련 데이터 모형 적합
logit.pred <- predict(logit.model, wisc.test)  # 테스트 데이터를 이용한 평가
lc <- confusionMatrix(logit.pred, wisc.test$diagnosis) 
lc
# 분류 결과 
#! 정확도 : 0.9647

# 시각화
roc.logit <- roc(as.numeric(logit.pred), as.numeric(wisc.test$diagnosis))
plot.roc(roc.logit, col = "red", print.auc = T,
         max.auc.polygon = T, print.thres = T,
         print.thres.pch = 19, print.thres.col = "red",
         auc.polygon = T, auc.polygon.col="#D1F2EB")

plot(lc$table, col = brewer.pal(2, "Pastel2"))


## 3)의사결정트리
library(rpart)  # 의사결정트리를 사용하기 위한 패키지
library(rpart.plot)

rpart.model <- rpart(diagnosis ~ ., data = wisc.train)  # 훈련 모형 적합
rpart.plot(rpart.model)  # 트리 시각화

rpart.pred <- predict(rpart.model, wisc.test, type = "class")  # 평가

rc <- confusionMatrix(rpart.pred, wisc.test$diagnosis)
rc
#! 정확도 : 0.9471

# 시각화
roc.rpart <- roc(as.numeric(rpart.pred), as.numeric(wisc.test$diagnosis))
plot.roc(roc.rpart, col = "red", print.auc = T,
         max.auc.polygon = T, print.thres = T,
         print.thres.pch = 19, print.thres.col = "red",
         auc.polygon = T, auc.polygon.col="#D1F2EB")

plot(rc$table, col = brewer.pal(2, "Set1"))


## 4)인공 신경망 기법 (nnet)
library(nnet)

# 데이터 표준화
wisc.train.scale <- as.data.frame(sapply(wisc.train[, -2], scale))
wisc.test.scale <- as.data.frame(sapply(wisc.test[, -2], scale))

wisc.train.scale$diagnosis <- wisc.train$diagnosis
wisc.test.scale$diagnosis <- wisc.test$diagnosis

# 훈련데이터로 모형 적합
wisc.nnet <- nnet(diagnosis ~., wisc.train.scale, size = 3)
wisc.nnet.pred <- predict(wisc.nnet, wisc.test.scale, type = "class")

nnc <- confusionMatrix(as.factor(wisc.nnet.pred), wisc.test$diagnosis)
nnc

# 시각화
roc.nnc <- roc(as.numeric(as.factor(wisc.nnet.pred)), as.numeric(wisc.test$diagnosis))
plot.roc(roc.nnc, col = "red", print.auc = T,
         max.auc.polygon = T, print.thres = T,
         print.thres.pch = 19, print.thres.col = "red",
         auc.polygon = T, auc.polygon.col="#D1F2EB")

plot(nnc$table, col = brewer.pal(2, "Dark2"))


## 5)SVM
library(kernlab)

# 분석 실행
wisc.svm <- ksvm(diagnosis ~ ., wisc.train, kernel = "rbfdot")

# 예측 및 결과
wisc.svm.pred <- predict(wisc.svm, wisc.test, type = "response")
sc <- confusionMatrix(wisc.svm.pred, wisc.test$diagnosis)
sc

# 시각화
roc.svm <- roc(as.numeric(wisc.svm.pred), as.numeric(wisc.test$diagnosis))
plot.roc(roc.rpart, col = "red", print.auc = T,
         max.auc.polygon = T, print.thres = T,
         print.thres.pch = 19, print.thres.col = "red",
         auc.polygon = T, auc.polygon.col="#D1F2EB")

plot(sc$table, col = brewer.pal(2, "Set3"))


## 6)앙상블 (배깅 - 랜덤 포레스트)
library(randomForest)

# 분석 실행
wisc.rf <- randomForest(diagnosis ~ ., wisc.test,
                        importance = T)  # ntree = 500, mtry = 2
wisc.rf  # 랜덤포레스트 분석 결과

# 중요 변수 시각화
importance(wisc.rf) 
varImpPlot(wisc.rf)


## 7)앙상블 (부스팅 -xgboost)
library(xgboost)

# 변수 리코딩 - xgboost는 변수 레이블이 숫자로 표시 되어야 한다.
wisc.train$diagnosis2[wisc.train$diagnosis == 'B'] <- 1
wisc.train$diagnosis2[wisc.train$diagnosis == 'M'] <- 2

wisc.test$diagnosis2[wisc.test$diagnosis == 'B'] <- 1
wisc.test$diagnosis2[wisc.test$diagnosis == 'M'] <- 2

# 매트릭스로 변환
wisc.train.mat <- as.matrix(wisc.train[-c(1, 2, 33)])
wisc.train.mat
train.label <- wisc.train$diagnosis2

wisc.test.mat <- as.matrix(wisc.test[-c(1, 2, 33)])
wisc.test.mat
test.label <- as.factor(wisc.test$diagnosis2)

# 모델링
mat.train <- xgb.DMatrix(data = wisc.train.mat, label = train.label)
mat.train

xgb.model <- xgboost(data = mat.train, max_depth = 6,
                     eta = 0.3, nthread = 2, nrounds = 2,
                     objective = "multi:softmax",
                     num_class = 3, verbose = 0)

# 예측 및 결과
xgb.pred <- predict(xgb.model, wisc.test.mat)
confusionMatrix(as.factor(xgb.pred), test.label)

# 주요 변수 확인 및 시각화
importance.xgb <- xgb.importance(colnames(mat.train),
                                 model = xgb.model)
importance.xgb

xgb.plot.importance(importance.xgb)
