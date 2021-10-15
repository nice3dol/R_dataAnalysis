### 연관규칙 평가 ###


## 트랜젝션 객체를 대상으로 연관규칙 생성

# 1. 패키지 및 데이터 준비
library(arules)

tran <- read.transactions("/rwork/dataset4/tran.txt",
                          format = "basket", sep = ",")
tran  # 트랜잭션 객체 생성

# 2. 트랜잭션 데이터 확인
inspect(tran)

# 3. 규칙발견 1
rule1 <- apriori(tran, parameter = list(supp = 0.3, conf = 0.1))
inspect(rule1)  # 지지도 0.3, 신뢰도 0.1일때 16개 규칙 발견 
# 4. 규칙발견 2
rule2 <- apriori(tran, parameter = list(supp = 0.1, conf = 0.1))
inspect(rule2) # 지지도 0.1, 신뢰도 0.1일때 35개 규칙 발견



### 트랜잭션 객체 생성


## single 트랜잭션 객체 생성
stran <- read.transactions("/rwork/dataset4/demo_single",
                           format = "single", cols = c(1, 2))
inspect(stran)
#! 한 개의 트랜잭션 구분자에 의해서 item이 연결된 경우
#! format = "single" 속성을 지정하고, sep을 생략하면
#! item은 공백으로 구분 된다. 또 cols 속성으로 칼럼을 지정한다.


## 중복 트랜잭션 제거 

# 1. 트랜잭션 데이터 준비
stran2 <- read.transactions("/rwork/dataset4/single_format.csv", format = "single", sep = ",",
                            cols = c(1, 2), rm.duplicates =  T)
stran2

# 2. 요약통계량
summary(stran2)
#! sizes를 확인하면 각 item 수에 구성된 transation 수 확인이 가능하다.

# 3. 규칙 생성
astran2 <- apriori(stran2)  # supp=0.1, conf=0.8

# 4. 발견된 규칙 확인
inspect(astran2)

# 5. 상위 5개의 향상도를 내림차순으로 정렬
inspect(head(sort(astran2, by = "lift")))



### 연관규칙 시각화 ###


## Adult 데이터셋을 활용한 연관규칙

# 데이터 준비
data(Adult)
str(Adult)
#! Adult는 arules 패키지에서 제공하는 AdultUCI 데이터 셋을 트랜잭션 객체로 변환한 데이터이다.

data("AdultUCI")
str(AdultUCI)

# 요약통계량
adult <- as(Adult, "data.frame")
str(adult)
head(adult)

summary(Adult)

# 기본 연관규칙 발견
ar <- apriori(Adult)  # supp=0.1, conf =0.8
#! 연관규칙 6137개 발견

# 다양한 신뢰도와 지지도 적용
ar1 <- apriori(Adult, parameter = list(supp = 0.2))  # 1306개 규칙 발견
ar2 <- apriori(Adult, parameter = list(supp = 0.2, conf = 0.95))  # 348개 규칙 발견
ar3 <- apriori(Adult, parameter = list(supp = 0.3, conf = 0.95))  # 124개 규칙 발견
ar4 <- apriori(Adult, parameter = list(supp = 0.35, conf = 0.95))  # 67개 규칙 발견
ar5 <- apriori(Adult, parameter = list(supp = 0.4, conf = 0.95))  # 36개 규칙발견

# 규칙 결과보기
inspect(head(ar5))  # 상위 6개 규칙

inspect(head(sort(ar5, by = "confidence")))  # 신뢰도 기준 내림차순 상위 6개 
inspect(head(sort(ar5, by = "lift")))  # 향상도 기준 내림차순 상위 6개

# 연관규칙 시각화
install.packages("arulesViz")
library(arulesViz)

plot(ar3, method = "graph", control = list(type = "items"))  # control 생략가능

#! 시각화 결과 124개 규칙 중 100개만 시각화 되었다.
#! 연봉 5만달러 이상 수령자와 관련된 연관어는 다음과 같다.
#! Full-time, White, Us, None, Hs-grad.....


## groceries 데이터 셋으로 연관분석하기

# 데이터 준비
data("Groceries")
str(Groceries)
Groceries

# 데이터프레임으로 변환
groceries.df <- as(Groceries, "data.frame")
groceries.df  # 품목과 데이터를 확인하기 더 편함

# 규칙 발견 (default = supp=0.1, conf=0.8)
groceries.rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))  # 410개 규칙 발견

# LHS -> RHS item 빈도수 보기
plot(groceries.rules, method = "grouped")
#! 빈도수 분석 결과 B상품의 빈도수가 가장 높은 것은 whole milk와 other vegetables 이다.

# 최대 길이 제한 규칙색성 (3)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen = 3))
#! 규칙을 구성하는 LHS와 RHS의 길이를 합쳐 3개 이하의 길이를 갖는 규칙만 생성했다. 규칙수는 29개 이다.

# 규칙 보기
rules <- sort(rules, by = "confidence")
inspect(rules)  # 신뢰도 기준 내림차순 정렬

# 발견된 규칙 시각화
library(arulesViz)
plot(rules, method = "graph")
#! 시각화 결과 whole milk와 other vegetables 단어를 중심으로 연관어가 형성되어 있음.

# 특정 상품으로 서브셋 작성과 시각화
wmilk <- subset(rules, rhs %in% "whole milk")
wmilk
inspect(wmilk)
plot(wmilk, method = "graph")

oveg <- subset(rules, rhs %in% "other vegetables")
oveg
inspect(oveg)
plot(oveg, method = "graph")

butter.yogurt <- subset(rules, lhs %in% c("butter", "yogurt"))
butter.yogurt
inspect(butter.yogurt)
plot(butter.yogurt, method = "graph")
