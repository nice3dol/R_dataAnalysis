### Hold-out ###

data("iris")

# 연속적 순서의 인덱스 추출
iris.train <- iris[1 : 105, ]
iris.test <- iris[106 : 150, ]

# 무작위 샘플링 추출
idx <- sample(1 : nrow(iris), size = nrow(iris) * 0.7, replace = F)  # 무작위 추출을 위한 인덱스 생성
iris.train <- iris[idx, ]  # 생성된 인덱스를 이용 학습 데이터 추출(70%)
iris.test <- iris[-idx, ]  # 학습에 사용되지 않은 데이터를 평가용 데이터로 지정(30%)

dim(iris.train)  # 분할 확인
dim(iris.test)

# 빈도 일관성 확인 - 분류와 빈도에 따른 성능 오차 검정
table(iris$Species)  # 종속변수의 빈도
table(iris.train$Species)  # 학습 데이터 종속변수 빈도
table(iris.test$Species)  # 평가 데이터 종속변수 빈도
## 첨언 : iris 데이터는 종속변수가 3개 50개씩 동일하지만 학습 데이터와 평가 데이터는 그러지 못하다는 것을 확인 할 수 있다. 이땐 R에 기타 패키지를 사용하여 이 문제를 해결 하 수 있다.

# doBy 패키지를 활용한 샘플링
install.packages("doBy")
library(doBy)

iris.train2 <- sampleBy(~ Speices, frac = 0.7, data = iris)
dim(iris.train2)
table(iris.train2$Species)

# caret 패키지를 활용한 샘플링
install.packages("caret")
library(caret)

train.idx <- createDataPartition(iris$Species, p = 0.7, list = F)
train.idx

iris.train3 <- iris[train.idx, ]
iris.test3 <- iris[-train.idx, ]
dim(iris.train3)
dim(iris.test3)
table(iris.train3$Species)
table(iris.test3$Species)


install.packages("e1071")
library(e1071)

### 나이브 베이즈

naive.result <- naiveBayes(iris.train3, iris.train3$Species, laplace =  1)  # 나이브베이즈 적합
naive.pred <- predict(naive.result, iris.test3, type = "class")  # 테스트 데이터 평가 
naive.pred
naive.result

table(naive.pred, iris.test3$Species)  # 분류 결과

confusionMatrix(naive.pred, iris.test3$Species)  # 혼동행렬


# heart 사용

data <- read.csv("/rwork/heart.csv", header = T)
str(data)

set.seed(1234)
tr.data <- createDataPartition(y = data$AHD, p = 0.7, list = F)
tr <- data[tr.data, ]
te <- data[-tr.data, ]

bayes <- naiveBayes(AHD ~ ., data = tr)
bayes

pred <- predict(bayes, te, type = "class")
table(pred, te$AHD)

AHD <- as.factor(te$AHD)
confusionMatrix(pred, AHD)



### 로지스틱 회귀 분석 사용
install.packages("nnet")
library(nnet)

multi.result <- multinom(Species ~ ., iris.train3)
multi.pred <- predict(multi.result, iris.test3)
table(multi.pred, iris.test3$Species)
