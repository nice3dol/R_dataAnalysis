### 이산변수 시각화


## 세로 막대 차트

# 데이터 작성
chart.data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart.data) <-  c("2018년 1분기", "2019년 1분기",
                        "2018년 2분기", "2019년 2분기",
                        "2018년 3분기", "2019년 3분기",
                        "2018년 4분기", "2019년 4분기")
str(chart.data)
dd <- data.frame(chart.data)

# 세로 막대 차트 그리기
barplot(chart.data, ylim = c(0, 600),
        col = rainbow(8),
        main = "2018 vs 2019 매출현황",
        xlab = "년도별 분기 현황",
        ylab = "매출액(단위:만원)")


## 가로 막대 차트

# 가로 막대 차트 그리기
barplot(chart.data, xlim = c(0, 600), horiz = T,
        col = rainbow(8),
        main = "2018 vs 2019 매출현황",
        xlab = "매출액(단위:만원)",
        ylab = "년도별 분기 현황")

# 막대 크기와 사이 조정
barplot(chart.data, xlim = c(0, 600), horiz = T,
        col = rainbow(8),
        main = "2018 vs 2019 매출현황",
        xlab = "매출액(단위:만원)",
        ylab = "년도별 분기 현황",
        space = 1, cex.names = 0.8)

# 막대 색상 지정(색 번호)
barplot(chart.data, xlim = c(0, 600), horiz = T,
        main = "2018 vs 2019 매출현황",
        xlab = "매출액(단위:만원)",
        ylab = "년도별 분기 현황",
        space = 1, cex.names = 0.8,
        col = rep(c(2, 4), 4))  # 2~4번 색상 4번 반복
#! 색상번호 -> 1.black, 2.red, 3.green, 4.blue, 5.sky
#!             6.purple, 7.yellow 

# 막대 색상 지정(색 이름)
barplot(chart.data, xlim = c(0, 600), horiz = T,
        main = "2018 vs 2019 매출현황",
        xlab = "매출액(단위:만원)",
        ylab = "년도별 분기 현황",
        space = 1, cex.names = 0.8,
        col = c("red", "green"))


## 누적 막대 차트

# 데이터 준비
data("VADeaths")
VADeaths
str(VADeaths)

# 개별, 누적 차트 그리기
par(mfrow = c(1, 2))  # 그래프 1행 2열 보기

barplot(VADeaths, beside = T, col = rainbow(5),
        main = "버지니아주 하위계층 사망비율율")
legend(19, 70, c("50-54", "55-59", "60-64", "65-59", "70-74"), cex = 0.8, fill = rainbow(5))

barplot(VADeaths, beside = F, col = rainbow(5),
        main = "버지니아주 하위계층 사망비율",
        font.main = 4)
legend(3.8, 200, c("50-54", "55-59", "60-64", "65-59", "70-74"), cex = 0.8, fill = rainbow(5))


## 점 차트 시각화

# 점 차트그리기
dotchart(chart.data, color = c("blue", "red"),
         lcolor = "black", pch = 1:2,  # pch는 점 모양
         labels = names(chart.data),
         xlab = "매출액",
         main = "분기별 판매현황 : 점차트 시각화",
         cex = 1.2)


## 원형 차트 시각화


# 파이 차트 그리기
pie(chart.data, labels = names(chart.data),
    col = rainbow(8), cex = 1.2)
title("2018-2019 분기별 매출현황")



### 연속변수 시각화


## 상자 그래프 시각화

# notch = F 일때
boxplot(VADeaths, range = 0)  # 0속성은 최소값과 최대값을 점선으로 연결

# notch = T 일때
boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 1, col = "red")  # 기준선 추가(lty= line type)

# 요약 통계량과 비교
summary(VADeaths)


## 히스토그램 시각화

# 데이터 준비
data(iris)
names(iris)
head(iris)

# 히스토그램 그리기
summary(iris$Sepal.Length)
hist(iris$Sepal.Length, xlab = iris$Sepal.Length,
     col = "magenta", main = "iris sepal length",
     xlim = c(4.3, 7.9))  # xlim은 최소값과 최대값

summary(iris$Sepal.Width)
hist(iris$Sepal.Width, xlab = "sepal width",
     col = "mistyrose", xlim = c(2, 4.5))

# 히스토그램에서 빈도와 밀도 표현
par(mfrow = c(1, 2))
hist(iris$Sepal.Width, xlab = "sepal width",
     col = "green", main = "꽃받침 너비 : 빈도수", 
     xlim = c(2.0, 4.5))  # 빈도수로 그린 히스토그램

hist(iris$Sepal.Width, xlab = "sepal width",
     col = "mistyrose", freq = F, 
     main = "꽃받침 너비 : 확률밀도",
     xlim = c(2, 4.5))  # 확률밀도로 그리기

lines(density(iris$Sepal.Width), col = "red")  # 밀도 기준 선 추가

# 정규분포 추청 곡선 나타내기
par(mfrow = c(1, 1))

hist(iris$Sepal.Width, xlab = "sepal width",
     col = "mistyrose", freq = F, 
     main = "꽃받침 너비 : 확률밀도",
     xlim = c(2, 4.5))  # 확률밀도로 그리기

lines(density(iris$Sepal.Width), col = "red")  # 밀도 선추가

x <- seq(2, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width),
            sd = sd(iris$Sepal.Width)),
      col = "blue", add = T)


## 산점도 시각화

# 산점도 시각화
price <- runif(10, min = 1, max = 100)  # 난수 발생
plot(price, col = "red")

par(new = T)
line.chart <- 1:100
plot(line.chart, type = "l", col = "red",
     axes = F, ann = F)  # ann = 라인 제외 축 삭제

text(80, 80, "대각선 추가", col = "blue")

# 여러가지 산점도
par(mfrow = c(2, 2))
plot(price, type = "l")  # 실선
plot(price, type = "o")  # 원형과 실선
plot(price, type = "h")  # 직선
plot(price, type = "s")  # 꺽은선

# 산점도에 속성 추가 (pch = 연결점)
par(mfrow = c(2, 2))
plot(price, type = "o", pch = 5)  
plot(price, type = "o", pch = 15)   
plot(price, type = "o", pch = 20, col = "blue")
plot(price, type = "o", pch = 20, col = "orange", cex = 1.5)

# lwd 속성 추가 (선 굵기)
par(mfrow = c(1, 1))
plot(price, type = "o", pch = 20, col = "green", 
     cex = 4, lwd = 6)


## 중첩 자료 시각화

# 중복 수 만큼 점 크기 확대
x <- c(1, 2, 3, 4, 2, 4)
y <- rep(2, 6)

table(x, y)  # 교차 테이블 작성

plot(x, y)  # x의 두개 값이 중복 되어 4개 값만 출력

xy.df <- as.data.frame(table(x, y))
xy.df  # 데이터프레임으로 변환하면 빈도수가 추가

plot(x, y, pch = "@", col = "blue",
     cex = 0.5 * xy.df$Freq)  # 빈도 수를 곱하여 중첩 크기 표현


## galton 데이터 셋으로 중첩 자료 시각화

# 데이터 준비
install.packages("UsingR")
library(UsingR)

data("galton")
head(galton)
galton.data <- as.data.frame(table(galton$child, galton$parent))
head(galton.data)

# 칼럼 단위 추출
names(galton.data) <- c("child", "parent", "freq")
head(galton.data)
parent <- as.numeric(galton.data$parent)
child <- as.numeric(galton.data$child)
child

# 시각화
plot(parent, child, pch = 21, col = "blue",
     bg = "green", cex = 0.2 * galton.data$freq)



### 변수 간의 비교 시각화


## iris 데이터 셋으로 상호 비교
attributes(iris)
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])
pairs(iris[iris$Species == "versicolor", 1:4])
#! pairs 함수는 numeric 칼럼을 대상으로 비교 결과를 행렬구조로 보여준다


## 3차원으로 산점도 시각화 하기

# 패키지
library(scatterplot3d)

# 종류변 변수 분류
iris.setosa <- iris[iris$Species == "setosa", ]
iris.virginica <- iris[iris$Species == "virginica", ]
iris.versicolor <- iris[iris$Species == "versicolor", ]

# 3차원 frame 생성
d3 <- scatterplot3d(iris$Petal.Length,
                    iris$Sepal.Length,
                    iris$Sepal.Width,
                    type = "n")
#! Petal.Length는 밑변, Sepal.Lenght는 오른쪽 변, Sepal.Width은 왼쪽변으로 만든다
#! type = n 속성은 기본 산점도를 표시하지 않음

# 3차원 산점도 시각화
d3$points3d(iris.setosa$Petal.Length,
            iris.setosa$Sepal.Length,
            iris.setosa$Sepal.Width,
            bg = "orange", pch = 21)

d3$points3d(iris.virginica$Petal.Length,
            iris.virginica$Sepal.Length,
            iris.virginica$Sepal.Width,
            bg = "blue", pch = 23)

d3$points3d(iris.versicolor$Petal.Length,
            iris.versicolor$Sepal.Length,
            iris.versicolor$Sepal.Width,
            bg = "green", pch = 25)
