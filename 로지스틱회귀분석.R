### 날씨 관련 요인 변수로 비(rain) 유무 예측 ###


install.packages("ROCR")
library(ROCR)

# 데이터 가져오기
weather = read.csv("/rwork/dataset4/weather.csv", stringsAsFactors = F)
head(weather)

# 변수 선택과 더미 변수 생성
weather_df <- weather[, c(-1, -6, -8, -14)]
str(weather_df)
# 로지스틱 회귀분석을 위한 더미 변수(0,1) 생성
weather_df$RainTomorrow[weather_df$RainTomorrow == "Yes"] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == "No"] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)
str(weather_df)

# Train, Test 데이터 분류
idx <- sample(1:nrow(weather_df), nrow(weather_df) * 0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]

# 로지스틱 회귀모델 생성
weather_model <- glm(RainTomorrow ~ ., data = train, family = "binomial", na.action = na.omit)
weather_model
summary(weather_model)

pred <- predict(weather_model, newdata = test, type = "response")
pred

result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred

table(result_pred)

table(result_pred, test$RainTomorrow)

# ROC Curve를 이용한 모델 평가

pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)



### mtcar 데이터를 이용한 로지스틱 회귀분석 ###

# 데이터 선별
data(mtcars)
dat <- subset(mtcars, select = c(mpg, am, vs))  # mpg : Miles per Gallon / vs : 0-V engine, 1-straight engine / am : Transmission(0-auto, 1-maunal)

# 로지스틱 회귀분석 실시 
log_reg <- glm(vs ~ mpg, data = dat, family = "binomial")
summary(log_reg)
