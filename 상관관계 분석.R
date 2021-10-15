# 상관관계 분석

# 데이터 확보
product <- read.csv("/rwork/dataset2/product.csv", header = T)
head(product)
summary(product)

# 상관계수 확인
cor(product$제품_친밀도, product$제품_적절성)  # 다소 높은 양의 상관관계
cor(product$제품_친밀도, product$제품_만족도)  # 다소 높은 양의 상관관계
cor(product$제품_적절성, product$제품_만족도)  # 높은 양의 상관관계
cor(product$제품_적절성 + product$제품_친밀도, product$제품_만족도)

# 피어슨의 상관계수 
cor(product, method = "pearson")

# 상관계수 시각화
install.packages("corrgram")
library(corrgram)

corrgram(product)
corrgram(product, upper.panel = panel.conf)  # 상관계수 추가
corrgram(product, lower.panel = panel.conf)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(product, histogram = , pch = "+")

## 차트 해설 : 세 변수 모두 대체로 정규분포 형태를 가지며 유의수준 0.05에서 세 변수 모두 상관성이 있는 것으로 나타 났다. 또한 제품 만족도 변수에 가장 큰 영향을 미치는 변수는 제품적절성(0.77) 으로 나타난다. ##


##### 상관 관계 행렬

data("EuStockMarkets")
dim(EuStockMarkets)

dax <- EuStockMarkets[, "DAX"]
smi <- EuStockMarkets[, "SMI"]

summary(dax)
var(dax)
sd(dax)
range(dax)

boxplot(dax)
plot(dax, smi)

plot(EuStockMarkets)

# 상관분석
cor(dax, smi)
cor(EuStockMarkets)

# 상관계수 행렬 plot
install.packages("corrplot")
library(corrplot)

corrplot(cor(EuStockMarkets), method = "pie")

