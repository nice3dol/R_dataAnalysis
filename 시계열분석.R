### 비정상성 시계열을 정상성 시계열로 변환 ###

# 데이터 준비
data("AirPassengers")
head(AirPassengers)

# 차분 적용 - 평균 정상화
par(mfrow = c(1, 1))
ts.plot(AirPassengers)  # 시계열 시각화
diff <- diff(AirPassengers)  # 차분 수행
plot(diff)  # 차분된 시계열 시각화

# 로그적용 - 분산 정상화
plot(AirPassengers)  # 시계열 시각화
log <- diff(log(AirPassengers))  # 로그+차분 수행
plot(log)  # 분사 정상화



### 시계열 자료 시각화 ###


## 단일 시계열 자료

# 데이터 준비
data("WWWusage")
str(WWWusage)

# 시계열 자료 추세선 시각화
X11()
ts.plot(WWWusage, col = "red")


## 다중 시계열 자료 시각화

# 데이터 준비
data("EuStockMarkets")
head(EuStockMarkets)
euStock <- data.frame(EuStockMarkets)
head(euStock)

# 다중 시계열 자료 시각화
plot(euStock$DAX[1:1000], type = "l", col = 'red')

# 다중 시계열 자료 추세선 시각화
plot.ts(cbind(euStock$DAX[1:1000], euStock$SMI[1:1000]),
     main = "주가지수 추세선")



### 시계열 요소 분해 시각화 ###

## 시계열 요소 분해 시각화

# 데이터 준비
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 56, 56, 64, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

# 시계열 자료 생성 - 시계열 형식으로 객체 생성
tsData <- ts(data, start = c(2016, 1), frequency = 12)
tsData

# 추세선 확인 - 시계열 요소를 시각적으로 확인
ts.plot(tsData)

# 시계열 분해
plot(stl(tsData, "periodic"))  # periodic : 주기적인

# 시계열 분해와 변동요인 제거
m <- decompose(tsData)
attributes(m)

plot(m)  # 추세, 계절, 불규칙 요인이 포함된 그래프
plot(tsData - m$seasonal)  # 계절 요인 제거

# 추세요인과 불규칙 요인 제거
plot(tsData - m$trend)  # 추세 요인 제거
plot(tsData - m$seasonal - m$trend)  # 추세, 계절 요인 제거



### 자기 상관 함수 / 부분 자기 상관 함수 시각화 ###


## 시계열 요소 분해 시각화

# 데이터 준비
input <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)
length(input)
tsInput <- ts(input, start = c(2015, 2), frequency = 12)
tsInput

# 자기 상관 함수 시각화
acf(na.omit(tsInput), main = "자기상관함수", col = "red")

# 부분 자기 상관 함수 시각화
pacf(na.omit(tsInput), main = "부분 자기 상관함수", col = "red")



### 추세 패턴 찾기 시각화 ###


## 시계열 자료의 추세 패턴 찾기

# 추세선 시각화
plot(tsInput, col = "red")

plot(diff(tsInput, differences = 1))
