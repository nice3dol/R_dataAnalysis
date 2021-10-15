#### 격자형 기법 시각화 ####



### lattice 패키지

# 패키지와 데이터 준비
library(lattice)

install.packages("mlmRev")
library(mlmRev)

data("Chem97")
str(Chem97)
head(Chem97)


### 히스토그램


## histogram() 시각화
x11()

histogram(~ gcsescore, data = Chem97)  # lattice 패키지 사용
#! x축은 구간, y축은 변수값 출현 도수에 따른 백분율
hist(Chem97$gcsescore)  # 기본 히스토그램
#! y축은 빈도 수


## score 변수를 조건변수로 지정하여 데이터 시각화
Chem97$score  # 변수 확인
Chem97$gcsescore

histogram(~ gcsescore | score, data = Chem97)
histogram(~ gcsescore | factor(score), data = Chem97)  # score를 factor로 변환
#! score 값을 factor를 적용하면 패널의 제목이 변경된다.



### 밀도 그래프


## 밀도 그래프 그리기

densityplot(~ gcsescore | factor(score), data = Chem97,
            groups = gender, plot.points = T, 
            auto.key = T)
# plot.points = 밀도 점 표시 여부 auto.key = 범례 표시 여부
#! 밀도 그래프 시각화 결과 gcsescore 점수가 모든 범주에서 여학생이 더 높다는 것을 알 수 있다.



### 막대 그래프


## barchart() 활용

# 데이터 준비
data("VADeaths")
VADeaths

str(VADeaths)
class(VADeaths)
mode(VADeaths)

# 데이터 형식 변경 (matrix -> table)
vad.dft <- as.data.frame.table(VADeaths)  # 긴형식으로 변경
vad.dft
str(vad.dft)

# 막대 그래프 시각화
barchart(Var1 ~ Freq | Var2, data = vad.dft,
         layout = c(4, 1))  # layout은 그래프 행열 설정

# origin 속성 사용
barchart(Var1 ~ Freq | Var2, data = vad.dft,
         layout = c(4, 1), origin = 0)  # x축 구간을 0부터 설정



### 점 그래프


## dotplot()

# layout 속성이 없는 경우
dotplot(Var1 ~ Freq | Var2, vad.dft)

# layout 속성 추가
dotplot(Var1 ~ Freq | Var2, vad.dft, layout = c(4, 1))


## 선으로 연결

dotplot(Var1 ~ Freq, vad.dft,
        groups = Var2, type = "o",
        auto.key = list(space = "right", points = T,
                        lines = T))  # auto.key = legend



### 산점도 그래프


## airquality 데이터셋

# 데이터 준비
library(datasets)
str(airquality)
summary(airquality)

# xyplot() 으로 산점도 시각화 (Ozone -> y축, Wind -> x축)
xyplot(Ozone ~ Wind, data = airquality)

# 조건 변수 사용(Month)
xyplot(Ozone ~ Wind | Month, data = airquality)

# layout 속성 사용
xyplot(Ozone ~ Wind | Month, data = airquality,
       layout = c(5, 1))

# Month 변수 factor 형태로 변환
convert <- transform(airquality, Month = factor(Month))
str(convert)

xyplot(Ozone ~ Wind | Month, data = convert)
#! factor 변경시 패널 제목의 가독성이 높아짐


## quakes 데이터 셋

# 데이터 준비
head(quakes)
str(quakes)

# 진앙지 산점도
xyplot(lat ~ long, data = quakes, pch = ".")

# 그래프 변수화, 제목 문자열 추가
tplot <- xyplot(lat ~ long, data = quakes, pch = ".")

tplot <- tplot %>% update(main = "1964년 이후 태평양에서 발생한 지진 위치")
tplot


## 이산형 변수를 조건으로 지정하여 산점도 시각화

# depth 변수 범위 확인
summary(quakes$depth)

# 변수 리코딩 (범주화)
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6

head(quakes)

# 리코딩된 변수를 조건으로 산점도 시각화
convert.quakes <- transform(quakes, depth2 = factor(depth2))  # factor 변환
xyplot(lat ~ long | depth2, data = convert.quakes)


## 동일한 패널에 두 개의 변수값 표현 (airquality)

xyplot(Ozone + Solar.R ~ Wind | factor(Month),
       data = airquality, col = c("blue", "red"),
       layout = c(5, 1))  # Ozone은 파란색, Solar.R 은 빨간색



### 데이터 범주화


## 이산형 변수 범주화하기

# 1 ~ 150을 대상으로 겹치지 않게 4개 영역 범주화
numgroup <- equal.count(1 : 150, number = 4, overlap = 0)
numgroup

# death를 5개 영역으로 범주화
summary(quakes$depth)
depthgroup <- equal.count(quakes$depth, number = 5,
                          overlap = 0)

# 범주화된 변수를 사용하여 산점도 그리기
xyplot(lat ~ long | depthgroup, data = quakes,
       main = "Figi Earthquakes(depthgroup)",
       ylab = "latitude", xlab = "longtitude",
       pch = "@", col = "red")

## 수심과 리히터 규모 변수를 동시에 적용

# 리히터 규모를 2개로 범주화
magnitudegroup <- equal.count(quakes$mag, number = 2,
                              overlap = 0)
magnitudegroup

# magnitudegroup 변수를 기준으로 산점도 시각화
xyplot(lat ~ long | magnitudegroup, data = quakes,
       main = "Fiji Earthquakes(Magnitude)",
       ylab = "latitude", xlab = "longtitude",
       pch = "@", col = c("blue"))

# 수심과 리히터 규모를 동시에 표현 (2행 5열 패널)
xyplot(lat ~ long | depthgroup *  magnitudegroup, 
       data = quakes,
       main = "Fiji Earthquakes(Magnitude)",
       ylab = "latitude", xlab = "longtitude",
       pch = "@", col = c("red", "blue"))  # depth : red / magnitude : blue
#! 시각화 해석 : depth는 5개 영역, magnitude는 2개 영역으로 범주화 되어 표시된다. 
#! magnitude 기준으로 2행으로 나뉜 그래프 이다.
#! factor 변수로 변환하면 가독성이 좋아지지만 현재 변수들은 연속형이기 때문에 바로 변환이 불가 하다.


## 연속형 변수를 factor형으로 변환하여 산점도 시각화

# depth 리코딩
quakes$depth3[quakes$depth >= 39.5 & quakes$depth <= 80.5] <- "d1"
quakes$depth3[quakes$depth >= 79.5 & quakes$depth <= 186.5] <- "d2"
quakes$depth3[quakes$depth >= 185.5 & quakes$depth <= 397.5] <- "d3"
quakes$depth3[quakes$depth >= 396.5 & quakes$depth <= 562.5] <- "d4"
quakes$depth3[quakes$depth >= 562.5 & quakes$depth <= 680.5] <- "d5"
#! 변수 범위 설정은 이전 equal.count()와 동일

# mag 리코딩
summary(quakes$mag)
quakes$mag3[quakes$mag >= 3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.55 & quakes$mag <= 6.45] <- 'm2'
str(quakes$mag3)

# factor 형 변환
convert.quakes2 <- transform(quakes, 
                             depth3 = factor(depth3),
                             mag3 = factor(mag3))

# 산점도 시각화
xyplot(lat ~ long | depth3 * mag3, 
       data = convert.quakes2, 
       main = "Fiji Earthquakes",
       ylab = "latitude", xlab = "longitude",
       pch= "@", col = c("red", "blue"))



### 조건 그래프


## depth 조건에 의해서 위도와 경도의 조건 그래프 그리기 

coplot(lat ~ long | depth, data = quakes)
#! coplot을 기본 속성값으로 사용하면 조건변수를 대상으로 6개 사이 간격으로 구간을 나누어 주고,
#! 각 구간은 0.5 단위로 겹쳐서 조건 변수가 범주화 된다.


## coplot 속성 변경

# 구간 막대가 0.1 단위로 겹쳐 범주화
coplot(lat ~ long | depth, data = quakes,
       overlap = 0.1)

# 조건구간 : 5개 // 1행 5열 패널 생성
coplot(lat ~ long | depth, data = quakes,
       row = 1, number = 5)


## 패널과 조건 막대에 색 적용

# 패널 영역에 부드러운 곡선 추가
coplot(lat ~ long | depth, data = quakes,
       row = 1, number = 5, 
       panel = panel.smooth)

# 조건 막대에 색 적용
coplot(lat ~ long | depth, data = quakes,
       row = 1, number = 5,
       col = "blue",  # 패널에 색 적용
       bar.bg = c(num = "green"))  # 막대에 색 적용



### 3차원 산점도 그래프


## 위도 경도 깊이를 이용하여 3차원 산점도 시각화

cloud(depth ~ lat * long, data = quakes,
      zlim = rev(range(quakes$depth)),
      xlab = "경도", ylab = "위도", zlab = "깊이")


## 테두리와 회전 속성 추가
cloud(depth ~ lat * long, data = quakes,
      zlim = rev(range(quakes$depth)),
      panel.aspect = 0.9,  # 테두리 사이즈
      screen = list(z = 45, x = -25),
      xlab = "경도", ylab = "위도", zlab = "깊이")
#! screen은 여기서 z축으로 45도, x축 반대 방향으로 25도 회전하는 속성이다.



