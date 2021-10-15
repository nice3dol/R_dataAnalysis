### 유클리디안 거리


## 유클리디안 거리 계산법

# 매트릭스 생성
x <- matrix(1:9, nrow = 3, by = T)
x

# 유클리디안 거리 생성
dist <- dist(x, method = "euclidean")  # method는 euclidean이 기본값
dist

# 1행과 2행 변량의 유클리디안 거리 구하기
s <- sum((x[1, ] - x[2, ])^ 2)
sqrt(s)  # 제곱근 적용

# 1행과 3행 변량의 유클리디안 거리 구하기
s <- sum((x[1, ] - x[3, ])^ 2)
sqrt(s)



### 계층적 군집 분석


## 유클리디안 거리를 이용한 군집화

# 패키지
install.packages("cluster")  # hclust - 계층적 함수 제공
library(cluster)

# 유클리디안 거리 매트릭스를 이용한 군집화
hc <- hclust(dist)

# 플로팅
plot(hc)
#! 덴드로그램에 의해 클러스터 형태로 시각화(Height는 군집에 유클리디안 거리를 의미한다.)


## interview자료로 해보는 군집분석

# 1. 데이터 준비
interview <- read.csv("/rwork/dataset4/interview.csv", header = T)
head(interview)

# 2. 유클리디안 거리 계산
interview.df <- interview[c(2:7)]  # 필요없는 칼럼 생략
interview.dist <- dist(interview.df)  # 유클리디안 거리 생성
head(interview.dist)

# 3. 계층적 군집 분석
interview.hc <- hclust(interview.dist)
interview.hc

# 4. 시각화
plot(interview.hc, hang = -1)  # hang속성값을 -1로 지정하면 덴드로그램에서 음수값을 제거 할 수 있다
rect.hclust(interview.hc, k = 3, border = "red")
#! 시각화 결과 3개의 그룹으로 군집이 형성되었다. 


## 군집별 특징 확인

# 1. 군집별 서브셋 만들기
g1 <- subset(interview, no == 108 | no == 110 |
               no == 107 | no == 112 | no == 115 )
g2 <- subset(interview, no == 102 | no == 101 |
               no == 104 | no == 106 | no == 113 )
g3 <- subset(interview, no == 105 | no == 114 |
               no == 103 | no == 109 | no == 111 )
#! 군집분석으로 분류된 3개 그룹을 대상으로 서브셋 작성

# 2. 각 서브셋의 요약 통계량
summary(g1)
summary(g2)
summary(g3)
#! 제 1그룹 : 종합점수 평균(71.6), 인성 평균(9.4), 자격증 없음, 모두 불합격
#! 제 2그룹 : 종합점수 평균(75.6), 인성 평균(14.8), 자격증 있음, 모두 합격
#! 제 3그룹 : 종합점수 평균(62.8), 인성 평균(11), 자격증 없음 or 있음, 모두 불합격
#! 이처럼 그룹화된 군집은 다변량적 특성이 그룹 내적으로는 동일하고 외적으로는 이질적이다.


### 군집 수 자르기


## iris 데이터를 활용한 군집 수 자르기

# 1. 유클리디안 거리 계산
data("iris")
iris.dist <- dist(iris[1:4])  # Species 데이터 생략
iris.hc <- hclust(iris.dist)

plot(iris.hc, hang = -1)
rect.hclust(iris.hc, k = 4, border = "red")

# 2. 군집 수 자르기
iris.ghc <- cutree(iris.hc, k = 3)
iris.ghc

# 3. ghc 칼럽 추가
iris$ghc <- iris.ghc
table(iris$ghc)
head(iris)

# 4. 요약통계량
iris.g1 <- subset(iris, ghc == 1)
iris.g2 <- subset(iris, ghc == 2)
iris.g3 <- subset(iris, ghc == 3)

summary(iris.g1[1:4])
summary(iris.g2[1:4])
summary(iris.g3[1:4])
#! iris 데이터 셋을 대상으로 인위적으로 잘라서 군집을 생성하고 특징을 알아본 과정



### 비계층적 군집 분석


## K-means 알고리즘

# 1. 분석에 사용할 변수 추출
library(ggplot2)
data("diamonds")
diamonds

t <- sample(1 : nrow(diamonds), 1000)  # 1000개 데이터 샘플링
test <- diamonds[t, ]
head(test)

mydia <- test[c("price", "carat", "depth", "table")]  # 군집화를 위한 필요 변수 추출
head(mydia)

# 2. 계층적 군집 분석(탐색적)
dia.result <- hclust(dist(mydia), method = "average")  # 평균 거리 이용
dia.result

plot(dia.result, hang = -1)

# 3. 비계층적 군집 분석
dia.result2 <- kmeans(mydia, 3)  # 군집수 3개 지정
names(dia.result2)

dia.result2$cluster  # 각 케이스에 대한 군집수생성

mydia$cluster <- dia.result2$cluster  # 원형 데이터에 군집수 칼럼 추가

head(mydia)

# 4. 변수 간의 상관계수 확인
cor(mydia[ , -5], method = "pearson")  # 피어슨 상관계수

plot(mydia[, -5])  # 변수 간 상관계수 산점도

#! 산점도 분석 결과 price 변수에 가장 큰 영향을 미치는 변수는 carat 이며, depth는 약한 음의 영향을 미친다.

# 5. 상관 계수를 색상으로 시각화
install.packages("mclust")
library(mclust)

install.packages("corrgram")
library(corrgram)

corrgram(mydia[ , -5], upper.panel = panel.conf)  # 수치 추가(위쪽)
corrgram(mydia[ , -5], lower.panel = panel.conf)  # 수치 추가(아래쪽)

# 6. 비계층적 군집 시각화
plot(mydia$carat, mydia$price, col = mydia$cluster)
points(dia.result2$centers[ , c("carat", "price")],
       col = c(3, 1, 2), pch = 8, cex = 5)
# 설명 : pch = 중심점 문자, cex = 중심점 크기
#! 시각화 결과 다이아몬드가 클수록 가격은 상승한다는 특징을 보인다.
