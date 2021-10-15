### 교차분석 ###


# 데이터 준비
data <- read.csv("/rwork/dataset2/cleandescriptive.csv", header = T)
head(data)

# 변수 리코딩
x <- data$level2
y <- data$pass2

# 데이터프레임 생성
result <- data.frame(Leveel = x, Pass = y)
dim(result)
str(result)

# 교차분할표 작성
table(result)  # 기본 함수 

# 교차분할표 작성을 위한 패키지 설치
install.packages("gmodels")
library(gmodels)
library(ggplot2)

# 패키지를 이용한 교차 분할표 작성. 1
CrossTable(x = diamonds$color, y = diamonds$cut)

# 패키지를 이용한 교차 분할표 작성. 2
CrossTable(x, y)

# CrossTable 함수를 이용한 카이제곱 검정
CrossTable(x = diamonds$cut, y = diamonds$color, chisq = T)  # p 값이 0.05 보다 작으므로 귀무가설을 기각할 수 있다. 따라서 두 변인은 서로 독립적이지 않다. 


### 카이제곱 검정 ###


## 일원 카이제곱검정 (적합도 분석)
chisq.test(c(4, 6, 17, 16, 8, 9))  # 유의확률을 활용한 해석법 : 유의확률이 0.05 보다 작으므로 귀무가설을 기각 할 수 있다. 따라서 주사위 게임은 적합하지 않다.
# 검정통계량을 활용한 해석법 : 검정통계량 카이제곱 값이 14.2 이고 자유도가 5일때의 카이제곱 분포표를 찾으면 임계값이 11.071에 해당한다. 본 분석에서 카이제곱값은 14.2이므로 임계값을 초과하였기 때문에 귀무가설을 기각하고 연구가설을 채택할 수 있다.

## 일원 카이제곱검정 (선호도 분석)
data <- textConnection(
  "스포츠음료 관측도수
  1 41
  2 30
  3 51
  4 71
  5 61"
)
x <-  read.table(data, header = T)

chisq.test(x$관측도수)  # 유의확률 0.05 미만이기 때문에 귀무가설을 기각 할 수 있다. 


## 이원 카이제곱검정 (독립성 검정)

# 데이터 준비
data <- read.csv("/rwork/dataset2/cleandescriptive.csv", header = T)  
head(data)

# 변수리코딩
x <- data$level2  
y <- data$pass2
CrossTable(x, y, chisq = T)  # 유의확률 0.05 이상이므로 귀무가설을 기각 할 수 없다. / 카이제곱값 2.766951 자유도 2에 해당하는 카이제곱 분포표 임계값은 5.99 이다. 본 분석의 카이제곱 값은 2.766 이므로 임계값을 넘지 못하기 때문에 귀무가설을 기각 할 수 없다.


## 이원 카이제곱검정 (동질성 검정)

# 데이터 준비
data <- read.csv("/rwork/dataset2/homogenity.csv")
head(data)

# 변수 리코딩
data$method2[data$method == 1] <- "방법1"
data$method2[data$method == 2] <- "방법2"
data$method2[data$method == 3] <- "방법3"

data$survey2[data$survey == 1] <- "1.매우만족"
data$survey2[data$survey == 2] <- "2.만족"
data$survey2[data$survey == 3] <- "3.보통"
data$survey2[data$survey == 4] <- "4.불만족"
data$survey2[data$survey == 5] <- "5.매우불만족"

# 교차 분할표 작성
table(data$method2, data$survey2)

# 동질성 검정 - 모든 특성치에 대한 추론검정
chisq.test(data$method2, data$survey2)  # 유의확률 0.05보다 크므로 귀무가설을 기각 할 수 없다

