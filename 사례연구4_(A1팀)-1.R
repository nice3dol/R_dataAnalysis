###### 사례연구4_A1팀 (김나현, 이상언, 유하영) ######

install.packages("magicfor")  # for문 처리용 패키지
library(magicfor)
library(dplyr)
library(lmtest)
library(TTR)
library(forecast)
library(readxl)


### 1번 ###


## 1-1. 세계은행 데이터를 활용하여 연도별 우리나라 인구 데이터를 기반으로 회귀분석 또는 시계열 분석을 이용하여 예측 하시오.

# 데이터 준비


kor.pop <- read_excel("/rwork/worldbank_korea.xlsx")

# 데이터 전처리
kor.pop <- t(kor.pop)  # 전치행렬
df.kor.pop <- as.data.frame(kor.pop)  # 구조 변경
colnames(df.kor.pop) <- "인구수"  # 열이름 정리

kor.pop <- ts(df.kor.pop, start = 1960, end = 2019, frequency = 1)
kor.pop

# 단일 시계열 자료 시각화
ts.plot(kor.pop, xlab = "Years", ylab = "Kor.Population") 

# 회귀 분석법 적용 검증
df.kor <- df.kor.pop %>% mutate(year = row.names(df.kor.pop))
colnames(df.kor) <- c("인구수", "연도")  # 이중 데이터를 만들기 위해 시계열 
                                         # 자료를 데이터프레임으로 변환
df.kor <- df.kor[, c(2, 1)]

qqplot(df.kor$인구수, df.kor$연도) 

formula <- kor$인구수 ~ kor$연도
lm.kor <- lm(formula = formula, data = df.kor)
summary(lm.kor)  

plot(formula = formula, data = df.kor)
abline(lm.kor, col = "red")  # 회귀 분석 (잔차)에 의한 추세선

# ACF와 PACF 확인
acf(kor.pop)   
pacf(kor.pop)

# 모형 식별과 모델링 
auto.arima(kor.pop)  
model <- arima(kor.pop, order = c(0, 2, 1))
plot(model$residuals)

# 모형의 타당성 검정
tsdiag(model)  

Box.test(model$residuals, lag = 1, type = "Ljung") 

# 차분을 통한 모형 확인
diff <- diff(kor.pop, differences = 2) 
plot(diff)  

# 예측 
fore <- forecast(kor.pop, h = 2)
plot(fore, type = "l")
fore



## 1-2 인구 순증감을 기반으로한 분석

# 데이터 준비
raw.birth <- read_excel("/rwork/birth.xlsx")  # 출생아수 
raw.death <- read_excel("/rwork/death.xlsx")  # 사망자수
birth <- t(as.data.frame(raw.birth))  # 데이터 전치 및 데이터프레임 변환
death <- t(as.data.frame(raw.death))  # 데이터 전치 및 데이터프레임 변환
strictly.increasing <- birth - death  # 순증감 데이터 만들기
colnames(strictly.increasing) <- "순증감"  # 컬럼명 변경
ts.si <- ts(strictly.increasing, start = 2000, 
            end = 2019, frequency = 1)  # 시계열 자료로 변환
ts.si

# 시각화
plot(ts.si, xlab = "Years", ylab = "Strictly_Increasing")   

# ACF와 PACF 확인
acf(ts.si)  
pacf(ts.si)  

# 모형 식별과 모델링 
auto.arima(ts.si)  
model2 <- arima(ts.si, order = c(0, 1, 1))
plot(model2$residuals)

# 모형의 타당성 검정
tsdiag(model2)  

Box.test(model2$residuals, lag = 1, type = "Ljung")  

# 예측 
fore.si <- forecast(ts.si, h = 2)
plot(fore.si, type = "l")
fore.si


# 1-3 AAGR 계산


magic_for(print, silent = T)  # for문을 저장하기 위한 함수
i = 1
for(i in 1:59){
  aagr <- (kor.pop[i + 1] / kor.pop[i] - 1) * 100
  print(aagr)
}
aagr <- magic_result_as_dataframe(aagr)
aagr <- aagr[, -1]

result.aagr <- round(sum(aagr) / length(aagr), 2)
result.aagr  # AAGR = 1.24%

# 1-4 CAGR 계산
cagr <- round((((kor.pop[60] / kor.pop[1]) ^ (1 / length(kor.pop))) - 1) * 100,
              2)
cagr  # CAGR = 1.22%

# 1-5 기타 기법
rm.kor <- df.kor %>% mutate(ra = runMean(인구수, 2))  # 이동평균법을 통한 예측
forecast(kor$ra, h = 2)

ses.kor <- ses(kor.pop, h = 2)  # 단순지수평활법을 통한 예측
ses.kor

holt.kor <- holt(kor.pop, damped = T, h = 2)  # 홀트 선형지수평활법을 통한 예측
kor.holt
plot(kor.holt)


