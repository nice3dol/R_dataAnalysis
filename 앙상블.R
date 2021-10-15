### 배깅 ###


## iris 자료를 활용한 배깅 분석

# 데이터 준비
install.packages("adabag")
library(adabag)  # Adabegging을 사용하기 위한 라이브러리
data(iris)

# 배깅
iris.bagging <- bagging(Species ~ ., data = iris, mfinal = 10)  # mfinal = 반복수 or 트리수 (디폴트 100)

iris.bagging$importance  # 변수의 상대적 중요도도
iris.bagging

# 플로팅
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

# 예측
iris.pred <- predict(iris.bagging, newdata = iris)
iris.pred

table(iris.pred$class, iris[ , 5])  # 오분류표
#! 에러율 : 0.0333



### 부스팅 ###


## iris를 활용한 부스팅

# 데이터 준비
data(iris)
iris.boost <- boosting(Species ~ ., data = iris, boos = T, mfinal = 10)  # 논리 인수 boos가 TRUE이면(기본으로), 훈련 집합의 부트스트랩 표본은 반복에서 각 관측을 위한 가중치를 사용하도록 한다. 만약 boos이 FALSE이면, 모든 관측에서 자신의 가중치를 가지고 사용된다.
iris.boost$ importance

# 플로팅
plot(iris.boost$trees[[10]])
text(iris.boost$trees[[10]])

# 예측
iris.boost.pred <- predict(iris.boost, newdata = iris)

iris.boost.pred
table(iris.boost.pred$class, iris[ , 5])

#! 분석결과 : 오차율 0으로 배깅보다 높은 분류 정확도를 보임.


## 인공 신경망 적합

# 데이터 준비
install.packages("ada")
library(ada)
data(iris)
edit.iris <- iris[iris$Species != "setosa", ]  # setosa 50개 자료 제외

# 훈련, 테스트 데이터 분할
n <- dim(edit.iris)[1]
trind <- sample(1 : n, 0.6 * n, F)
teind <- setdiff(1 : n, trind)  # 차집합
edit.iris[ , 5] <- as.factor((levels(iris[ , 5])[2 : 3])[as.numeric(iris[, 5])- 1])

# 훈련 데이터를 이용하여 부스팅 모형 구축
gdis <- ada(Species ~ ., data = edit.iris[trind, ], iter = 20, nu = 1, type = "discrete")  # nu = 1(디폴트)은 부스팅을 위한 축소(shrinkage) 모수 // type = "discrete"(디폴트)은 부스팅 알고리즘 지정. 
gdis <- addtest(gdis, edit.iris[teind, -5], edit.iris[teind, 5])
gdis
#! 에러율 0

# 플로팅
plot(gdis, T, T)  # T, T 의 경우 훈련 검증용 모두 시각화

varplot(gdis)  # 변수의 중요도를 나타낸다. 그림에선 Sepal.Length 변수가 가장 중요한 변수이다.

pairs(gdis, iris[trind, -5], maxvar = 4)  # 두 예측 변수의 조합별고 분류된 결과. maxvar은 변수의 수 지정



### 랜덤포레스트 ###


## 랜덤포레스트 기본 모델 생성

# 데이터 준비
library(randomForest)
data("iris")

# 모델 생성
iris.model <- randomForest(Species ~ ., data = iris)  # ntree(트리 수), mtry(변수 수) 변수 생략시 ntree = 500, mtry = 2개로 지정
iris.model  

# 파라미터 조정
iris.model2 <- randomForest(Species ~ ., data = iris, 
                            ntree = 300,
                            mtry = 4,
                            na.action = na.omit)
iris.model2

# 중요 변수로 모델 생성
iris.model3 <- randomForest(Species ~ ., data = iris,
                            importance = T)  # importance 속성은 분류모델을 생성하는 과정에서 입력 변수 중 가장 중요한 변수를 알려준다.
importance(iris.model3)  # MeanDecreaseAccuracy는 분류정확도를 개선하는 데 기여한 변수를 수치로 제공,
# MeanDecreaseGini는 노드 불순도를 개선하는데 기여한 변수 수치 제공

# 플로팅
varImpPlot(iris.model3)  # Petal.Width의 기여도가 가장 높음


## 최적의 파라미터 찾기

# 속성값 생성
ntree <- c(400, 500, 600)
mtry <- c(2 : 4)
param <- data.frame(n = ntree, m = mtry)
param

# 이중 for 함수를 이용하여 모델 생성
for(i in param$n){
  cat('ntree = ', i, '\n')
  for(j in param$m){
    cat('mtry = ', j, '\n')
    iris.model4 <- randomForest(Species ~ ., data = iris, ntree = i, mtry = j, na.action = na.omit)
    print(iris.model4)
  }
}
#! 해석 : 루프를 통해 나온 9개 모델 중 OOB estimate of error rate를 비교하여 최적의 트리와 변수를 결정



### XGBOOST ###


## 다항분류 xgboost 모델

# 패키지
install.packages("xgboost")
library(xgboost)

# y 변수 생성
iris.label <- ifelse(iris$Species == "setosa", 0,
                     ifelse(iris$Species == "versicolor", 1, 2))
table(iris.label)
iris$label <- iris.label
iris

# dataset 생성
iris.idx <- sample(1 : nrow(iris), nrow(iris) * 0.7)
train.iris <- iris[iris.idx, ]
test.iris <- iris[-iris.idx, ]

# matrix 객체 변환
train.iris.mat <- as.matrix(train.iris[1:4])
dim(train.iris.mat)

train.iris.lab <- train.iris$label
length(train.iris.lab)

# xgb.DMatrix 객체 변환
dtrain <- xgb.DMatrix(data = train.iris.mat, label = train.iris.lab)

# 모델 생성 (xgboost matrix 객체 이용)
xgb.model <- xgboost(data = dtrain, max_depth = 2,
                     eta = 1, nthrea = 2, nrounds = 2,
                     objective = "multi:softmax",
                     num_class = 3, verbose = 0)
xgb.model

# testset 생성
test.iris.mat <- as.matrix(test.iris[1:4])
dim(test.iris.mat)

test.iris.lab <- test.iris$label

# 모델 예측
pred.iris <- predict(xgb.model, test.iris.mat)
pred.iris

# 혼돈행렬
table(pred.iris, test.iris.lab)

# 성능평가 - Accuracy
(13 + 15 + 15) / nrow(test.iris)  # 0.9555

# model의 중요 변수와 영향력 확인
importance.matrix <- xgb.importance(colnames(train.iris.mat), model = xgb.model)
importance.matrix

# plot
xgb.plot.importance(importance.matrix)

