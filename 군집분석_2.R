### K 평균 군집화


## 군집 수 K 결정

# 패키지 및 데이터 준비
library(cluster)
data(iris)

# 군집화(K = 2)
nc2 <- pam(iris, 2)  # 데이터를 2개로 군집화
si2 <- silhouette(nc2)  # 실루엣값 계산산
summary(si2)
#! summary 평균값이 Average silhouette width 값이다. 따라서 iris 데이터를 K를 2로 설정시 실루엣 값은 0.68254 이다.

# 플로팅
plot(si2)  # 실루엣 계산 결과 출력

# 군집화(K = 3)
nc3 <- pam(iris, 3)
si3 <- silhouette(nc3)
summary(si3)
#! K를 3으로 설정시 실루엣 평균 값은 0.5729로 2보다 더 낮은 값이다.
#! 전체 실루엣 평균값은 0~1 사이의 값을 가지며 1에 가깔 울 수록 좋은 모델이다.

## K 평균 군집화

# 군집화(K = 2)
iris.kc2 <- kmeans(iris[ , -5], 2)  # Species 칼럼 제외
iris.kc2

# 군집화(K = 3)
iris.kc3 <- kmeans(iris[ , -5], 3)  # Species 칼럼 제외
iris.kc3

#! 분석결과 군집수 3개가 2개 보다 분류 정확도가 높다.
#! 실루엣값은 2가 높게 나왔지만 실제 군집은 2가 아닐 수 있다. 따라서 그 결과를 반드시 다시 확인해야한다.



### 2차 K-평균 클러스터링

# 데이터 준비
iris2 <- iris[, -5]

# K평균 클러스터링
km.out.withness <- c()
km.out.between <- c()
for(i in 2:7){
  set.seed(1)
  km.out <- kmeans(iris2, i)
  km.out.withness[i - 1] <- km.out$tot.withinss  # 군집 내 제곱합 저장
  km.out.between[i - 1] <- km.out$betweenss  # 군집 간 제곱합 저장
}
data.frame(km.out.withness, km.out.between)
#! K값을 2 ~ 7 로 변화시켜가며 비교
#! K가 증가함에 따라 군집 내 제곱합은 감소하고, 군집 간 제곱합은 증가한다.

plot(km.out.withness, type = "b")
plot(km.out.between, type = "b")
#! 플로팅 결과 군집 내, 군집 간 모두 K값 3에서 엘보우포인트가 보인다. 따라서 K 개수는 3이 적당하다고 볼 수 있다.

# K=3 클러스터링
km.out.k3 <- kmeans(iris2, 3)
km.out.k3$centers  # 각 군집의 중심점
km.out.k3$cluster  # 군집 번호호
km.out.k3$size  # 데이터 관측치 개수수

table(km.out.k3$cluster, iris$Species)  # 군집 결과와 원래 품종 비교

# 플로팅
plot(iris2[, 1:2], col = km.out.k3$cluster,
     pch = ifelse(km.out.k3$cluster == 1, 16,
     ifelse(km.out.k3$cluster == 2, 17, 18)),
     cex = 2)  # 편의상 2개 변수만 시각화
points(km.out.k3$centers, col = 1:3, pch = 16:18,
       cex = 5)



### 예제1 ###

# 데이터 준비
data("USArrests")
str(USArrests)
head(USArrests)

# 유클리디안 거리와 계층적 군집화
arrests.dist <- dist(USArrests, method = "euclidean")  # 유클리디안거리
arrests.hc <- hclust(arrests.dist, method = "ave")
  
# 플로팅
plot(arrests.hc, hang = -1)  # 덴드로그램 시각화

# 군집 수 자르기
arrests.group <- cutree(arrests.hc, k = 6)
arrests.group

rect.hclust(arrests.hc, k = 6, border = "red")  # 덴드로 그램에 그룹화 표시

# 2차 플로팅
hca <- hclust(dist(USArrests))
plot(hca, hang = -1)
rect.hclust(hca, k = 3, border = "red")
rect.hclust(hca, k = 49, 
            which = c(2, 7), 
            border = 3:4)



### 예제 2 ###

# 패키지
library(cluster)

agn1 <- agnes(USArrests, metric = "manhattan",
              stand = T)
par(mfrow = c(1, 2))
plot(agn1)

agn2 <- agnes(daisy(USArrests), diss = T, method = "complete")
plot(agn2)

agn3 <- agnes(USArrests, method = "flexible", 
              par.meth = 0.6)
plot(agn3)

par(mfrow = c(1, 1))



### 예제 3 ###

# 데이터 준비
install.packages("rattle")
data(wine, package = "rattle")
head(wine)

# 표준화
wine.df <- scale(wine[-1])
wine.df

# 군집 수 정하기
wssplot <- function(data, nc = 15, seed = 1234){
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, center = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}  # wssplot 함수 정의

wssplot(wine.df)
#! 군집수 3에서 엘보우 포인트 확인 

# Nbclust를 활용한 군집 수 결정
install.packages("NbClust")
library(NbClust)

set.seed(1234)
nc <- NbClust(wine.df, min.nc = 2, max.nc = 15,
              method = "kmeans")
nc.table <- table(nc$Best.nc[1, ])
nc$Best.nc

barplot(nc.table, xlab = "Number of Clusters",
        ylab = "Number of Criteria", 
        main = "Number of Clusters chosen by 26 Criteria")
#! 최적의 군집수를 정하기 위해 사용되는 지수(총 30개 중 여기서는 26개가 계산됨) 가운데 15개의 지수가 3을 최적의 군집수로 투표한 결과를 보여준다.
#! 따라서 군집의 수 K는 3으로 결정할 수 있다.

# Kmeans 군집화
set.seed(1234)
wine.km <- kmeans(wine.df, 3, nstart = 25)
wine.km$size  # 군집 별 크기
wine.km$centers  # 군집 별 중심점

# 플로팅
plot(wine.df, col = wine.km$cluster)
points(wine.km$centers, col = 1:3, pch = 8, cex = 1.5)

# 요약값 정리
aggregate(wine[-1], by = list(cluster = wine.km$cluster), mean)

# 혼돈매트릭스
wine.cm <- table(wine$Type, wine.km$cluster)
wine.cm
(59 + 65 + 48) / nrow(wine)
#! 분류 정확도 : 0.9662921

# flexclust 활용
install.packages("flexclust")
library(flexclust)

randIndex(wine.cm)  # ARI = 0.897495
#! randIndex는 실제 와인의 종류와 군집간의 일치도를 나타내는 수정된 순위 지수 이다.
#! 이 지수는 -1(no agreement)와 1(perfect agreement) 사이 값을 가진다.



### 예제 4 ###

# 데이터 준비
data("Nclus")  # 서로 다른 4개의 이변량 정규분포로부터 발생된 난수
plot(Nclus)

c1 <- kcca(Nclus, k = 4, family = kccaFamily("kmeans"))
image(c1)
points(Nclus)

barplot(c1)  # 변수 별 중심이 전체 군집의 중심(상자안의 막대)로부터 얼마나 벗어나 있는지를 나타냄

stripes(c1)  # 해당 군집의 평균으로부터 떨어진 정도



### 예제 5 ###
install.packages("cclust")
library(cclust)

cl.1 <- cclust(Nclus, 4, 20, method = "kmeans")
plot(Nclus, col = cl.1$cluster)
points(cl.1$centers, col = 1:4, pch = 8, cex = 1.5)
