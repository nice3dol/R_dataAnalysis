# 데이터 준비
library(MASS)
data("Boston")

# 훈련, 테스트 데이터 분할
idx <- sample(1 : nrow(Boston), nrow(Boston) * 0.7, replace = F)
Boston.train <- Boston[idx, ]
Boston.test <- Boston[-idx, ]
dim(Boston.train); dim(Boston.test)


## 다중회귀분석 기법

boston.lm <- lm(medv ~ ., data = Boston.train)
summary(boston.lm)
#! 결과 : R값은 0.7399로 양호한 편이나 회귀계수들의 t통계량과 p값이 일부 계수에서 유의하지 않은 것으로 보인다.
#! 따라서 변수선택법을 통하여 다시 회귀분석을 실시한다.

# 변수선택법
boston.lm2 <- step(boston.lm, method = "both")
summary(boston.lm2)
#! 결과 : R값은 0.7412로 증가, F통계량도 증가, 모든 회귀 계수가 유의하게 나타남. 따라서 본 회귀 적합결과가 수치 예측에 더 유의하다.

# 예측과 플로팅
lm.model <- predict(boston.lm2, newdata = Boston.test)
summary(lm.model)
lm.mse <- mean((lm.model - Boston.test$medv) ^ 2)  # 평가데이터 평균제곱오차(MSE) - 19.01805
sqrt(lm.mse)  # 4.360
#! 결과 : MSE는 19.01805이며 이에 제곱근은 4.360이다.
#         따라서 예측값은 $4,360 이내에 있다


## 의사결정트리 
install.packages("tree")
library(tree)

boston.tree <- tree(medv ~ ., data = Boston.train)
summary(boston.tree)
#! 분석 결과 rm, lstat, dis, nox 네가지 변수만 모델링에 사용됨

plot(boston.tree)
text(boston.tree, pretty = 0)
#! 플로팅결과 rm 변수가 가장 영향력이 높아 첫 번째 분할 변수로 사용 되었다. 즉 평균 방의 개수가 많을 수록 주택가격은 높다.
#! 구체적으로 주택당 방의 개수가 7.5개 이상인 경우 주택가격이 47.15이다.
#! 또한 방의 개수가 6.9개 미만이면 지위(lstat)와 일산화질소농도(nox) 변수에 따라 주택가격은 달라진다

# 예측
tree.model <- predict(boston.tree, newdata = Boston.test)
tree.mse <- mean((tree.model - Boston.test$medv) ^ 2)
sqrt(tree.mse)
#! 분석결과 MSE는 32.65이며 제곱근은 약 5.71이다.
#! 죽 의사결정트리 모델의 예측값은 실제 주택가격 값의 약 $5,714 이내에 있다.


## rpart함수를 사용한 의사결정트리 분석
library(rpart)

rpart.boston <- rpart(medv ~ ., data = Boston.train)
summary(rpart.boston)
#! rpart 함수는 변수의 중요도를 쉽게 확인 할 수 있다.
#! 즉 rm lstat indus crim 등의 순서로 영향력이 있다는 것을 확인 할 수 있다.

# 플로팅
library(rpart.plot)

rpart.plot(rpart.boston, digits = 3, type = 0,
           extra = 1, fallen.leaves = F, cex = 1)
#! 시각화 결과 rm이 7.4 이상일때 주택가격이 47.2로 가장 높다. 또한 rm이 6.8 이하이면 lstat이 높은경우, nox가 높을 경우 주택 가격이 낮게 형성 된다.

# 예측
rpart.model <- predict(rpart.boston, newdata = Boston.test)
rpart.mse <- mean((rpart.model - Boston.test$medv) ^ 2)
sqrt(rpart.mse)
rpart.mse
#! 예측 결과 mse는 21.688, 이에 제곱근은 4.6571이다.


## 인공신경망 기법

# 정규화 함수
normalize <- function(x){return((x - min(x))/(max(x) - min(x)))}
boston.train.norm <- as.data.frame(sapply(Boston.train, normalize))
boston.test.norm <- as.data.frame(sapply(Boston.test, normalize))

# nnet 함수를 사용한 분석
library(nnet)

nnet.boston <- nnet(medv ~ ., data = boston.train.norm, size = 5)  # 인공신경망 적합

# 예측
nnet.model <- predict(nnet.boston, newdata = boston.test.norm, type = "raw")  # 예측 결과 생성

nnet.mse <- mean((nnet.model - boston.test.norm$medv) ^ 2)
nnet.mse

# neuralnet 함수 사용
library(neuralnet)

neural.boston <- neuralnet(medv ~ crim + zn + indus + 
                             chas + nox + age + rm + 
                             dis + rad + tax + ptratio +
                             black + lstat,
                           data = boston.train.norm, 
                           hidden = 5)

# 예측
neural.model <- compute(neural.boston, boston.test.norm[1 : 13])
neural.result <- neural.model$net.result  # $net.result가 실제 예측 값

neural.mse <- mean((neural.result - boston.test.norm$medv) ^ 2)
neural.mse
#! 예측 결과 MSE는 0.080226

plot(neural.boston)  # 인공신경망 시각화


## 앙상블 (랜덤포레스트)
library(randomForest)

set.seed(1234)
rf.boston <- randomForest(medv ~ ., data = Boston.train, mtry = 6, importance = T)
rf.boston

plot(rf.boston)  # 트리 개수 변화에 따른 error 감소 추이

importance(rf.boston)  # 변수의 중요도

varImpPlot(rf.boston)  # 변수의 중요도 플로팅

# 예측
rf.model <- predict(rf.boston, newdata = Boston.test)
rf.mse <- mean((rf.model - Boston.test$medv) ^ 2)
sqrt(rf.mse)
#! 예측 결과 MSE값은 약 11.43으로 앞서 한 다중회귀 분석의 MSE값 보다 많이 낮아졌다. 
#! MSE의 제곱근 값은 3.38이며 이는 실제 주택 가격이 약 $3.381 이내에 있다는 것을 의미한다.

###!!! 최종 결과 앙상블 모형인 랜덤포레스트가 가장 좋은 예측 성능을 보였다. 하지만 필요한 파라미터 조정이나 표본 추출을 반복하면서 모형 튜닝 작업을 시행한다면 더 좋은 결과 나올 수 있다.