### 평활법(이동평균) ###


# 평활 관련 패키지 설치
install.packages("TTR")
library(TTR)

# 데이터 준비
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 56, 56, 64, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

tsData <- ts(data, start = c(2016, 1), frequency = 12)
tsData

# 이동평균법으로 평활 및 시각화
par(mfrow = c(2, 2))
plot(tsData, main = "원 시계열 자료료")
plot(SMA(tsData, n = 1), main = "1년 단위 이동평균법으로 평활")
plot(SMA(tsData, n = 2), main = "2년 단위 이동평균법으로 평활")
plot(SMA(tsData, n = 3), main = "3년 단위 이동평균법으로 평활")

par(mfrow = c(1, 1))
X11()



### ARIMA 모형 ###


## 계절성이 없는 정상성 시계열 분석

# 데이터 준비
input <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)
length(input)
tsInput <- ts(input, start = c(2015, 2), frequency = 12)
tsInput

# 추세선 시각화
plot(tsInput, type = "l", col = 'red')
diffInput <- diff(tsInput)
plot(diffInput)

# 모형 식별과 추청
install.packages("forecast")
library(forecast)

arimaInput <- auto.arima(tsInput)
arimaInput

# 모형 생성
model <- arima(tsInput, order = c(1, 1, 0))
model

# 모형 진단(타당성 검정)
tsdiag(model)  #자기상관함수에 의한 진단

Box.test(model$residuals, lag = 1, type = "Ljung")  # Box-Ljung에 의한 잔차항 진단

# 미래 예측
fore <- forecast(model)  # 향후 2년 예측
fore

par(mfrow = c(1, 2))
plot(fore)
model2 <- forecast(model, h = 6)  # 향후 6개월 예측
plot(model2)


## 계절성을 갖는 정상성 시계열분석

# 데이터 준비
data2 <- c(55, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 61, 65, 69, 53, 70, 75, 56, 56, 65, 55, 68, 80, 65, 67, 77, 69, 79, 82, 57, 55, 63, 60, 68, 70, 58, 65, 70, 55, 65, 70)
length(data2)

tsData2 <- ts(data2, start = c(2020, 1), frequency = 12)
tsData2

# 시계열 요소 분해 시각화
tsFeature <- stl(tsData2, s.window = "periodic")
plot(tsFeature)

# 정상성 시계열 변환 (계절 차분)
par(mfrow = c(1, 2))
plot(tsData2)
diff2 <- diff(tsData2)
plot(diff2)

# 모형 식별과 추청
tsModel <- auto.arima(tsData2)
tsModel

# 모형 생성
model2 <- arima(tsData2, c(0, 1, 1), seasonal = list(order = c(1, 1, 0)))
model2

# 모형 진단 (타당성 검정)
tsdiag(model)  #자기 상관함수에 의한 진단

Box.test(model2$residuals, lag = 1, type = "Ljung")  # Box-Ljung에 의한 잔차항 진단

# 미래 예측
par(mfrow = c(1, 2))
fore2 <- forecast(model2, h = 24); plot(fore)  # 2년 예측
fore3 <- forecast(model2, h = 6); plot(fore2)  # 6개월 예측
