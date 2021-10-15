#### 기하학적 기법 시각화



### qplot() - plot()의 기능 확장


## 패키지와 데이터 준비

library(ggplot2)
data(mpg)
str(mpg)
head(mpg)
summary(mpg)
table(mpg$drv)  # 구동방식 빈도수


## qplot()의 fill과 binwidth 속성

# 도수분포를 세로 막대 그래프로 표현
qplot(hwy, data = mpg)  # 현재 도수분포도는 30

# fill 적용
qplot(hwy, data = mpg, fill = drv)  # fill은 변수별 색상 적용

# binwidth 적용
qplot(hwy, data = mpg, fill = drv,
      binwidth = 2)  # binwidth는 막대 폭 넓이


## facets 속성으로 drv 변수 행열 단위로 패널 생성

# 열 단위 패널 생성
qplot(hwy, data = mpg, fill = drv,
      facets = . ~ drv, binwidth = 2)  # 열 단위 생성

# 행 단위 패널 생성
qplot(hwy, data = mpg, fill = drv,
      facets = drv ~ .,
      binwidth = 2)  # 행 단위 생성



### 두 개 변수 대상으로 qplot() 적용


## color 속성으로 두 변수 구분

# displ과 hwy 변수 사용
qplot(displ, hwy, data = mpg)

# displ과 hwy 변수 사용하며 drv 변수에 색 적용
qplot(displ, hwy, data = mpg, color = drv)
#! 시각화 해석 : displ(엔진크기)가 작고, drv(구동방식)이 f일때 hwy 주행 마일 수가 더 크다.

# drv 변수로 두 변수 구분 (열 단위 패널)
qplot(displ, hwy, data = mpg, color = drv,
      facets = . ~ drv)



### 미적 요소 맵핑


## mtcars 데이터에 색, 크기, 속성 적용

# 데이터 준비
data(mtcars)
head(mtcars)

# 색 적용
qplot(wt, mpg, data = mtcars, color = factor(carb))
#! 해석 : wt(중량)이 작을 수록 mpg(연비)는 높다
#!        carb(카뷰레터 수)가 작을 수록 mpg(연비)는 높다

# 크기 적용
qplot(wt, mpg, data = mtcars, color = factor(carb),
      size = qsec)  # qsec = 1/4마일 소요시간

# 모양 적용
qplot(wt, mpg, data = mtcars, color = factor(carb),
      size = qsec, shape = factor(cyl))  # cyl = 실린더수



### 기하학적 객체 적용


## diamonds 데이터에 적용

# 데이터 준비
data("diamonds")
head(diamonds)

# geom, fill 속성 사용
qplot(clarity, data = diamonds,
      fill = cut, geom = "bar")  # fill->cut 속성으로 색 채우기 / geom='bar' -> bar 속성으로 막대그래프 생성

# 테두리 색 적용
qplot(clarity, data = diamonds,
      colour = cut, geom = "bar")  # colour->테두리색

# geom = "point" 속성으로 산점도 시각화 (mtcars 데이터)
qplot(wt, mpg, data = mtcars, size = qsec,
      geom = "point")

# 산점도 그래프에 cyl, carb 변수 추가
qplot(wt, mpg, data = mtcars, size = factor(cyl),
      color = factor(carb), geom = "point")
#! 이산변수인 cyl를 size속성으로 사용하지 않는 것이 좋다란 경고문구

# 산점도 그래프에 qsec으로 포인트 크기, cyl 모양 적용
qplot(wt, mpg, data = mtcars, size = qsec,
      color = factor(carb), shape = factor(cyl),
      geom = "point")  # geom의 기본 속성은 point이므로 생략 가능

# geom = "smooth" 속성으로 산점도 그래프에 평활 추가
qplot(wt, mpg, data = mtcars,
      geom = c("smooth", "point"))

# 산점도 그래프의 평활에 cyl 변수 요인 색으로 적용
qplot(wt, mpg, data = mtcars, color = factor(cyl),
      geom = c("smooth", "point"))

# geom = "line" 속성 사용
qplot(mpg, wt, data = mtcars,
      color = factor(cyl), geom = "line")

# geom = c("point", "line") 속성 사용
qplot(mpg, wt, data = mtcars,
      color = factor(cyl), geom = c("line", "point"))



### ggplot()


## 미적요소 맵핑

# diamonds 데이터에 aes() 함수 적용
pd <- ggplot(diamonds, aes(carat, price, color = cut))
pd + geom_point()  # point 차트 추가
#! aes는 aesthetics(미학)의 약자

# mtcars 데이터에 미적요소맵핑
pm <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
pm + geom_point()


## 기하학적 객체 적용
#! 설명 : geom()함수는 기하학적 객체(geometric object)라는 용어를 축약함. 
#! ggplot()에서 정의된 미적요소 객체를 상속 받아 별도로 사용 가능. 상속은 + 연산자 이용

# geom_line() 레이어 추가
pm <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
pm + geom_line()  # 선그래프 레이어 추가

# geom_point() 레이어 추가
pm <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
pm + geom_point()  # 산점도 레이어 추가


## 미적요소맵핑과 기하학적 객체 적용

# 기본 미적요소 맵핑 객체를 생성한 뒤에 stat_bin() 적용
pd <- ggplot(diamonds, aes(price)) 
pd + stat_bin(aes(fill = cut), geom = "bar")

# price 빈도를 밀도로 스케일링
pd + stat_bin(aes(fill = ..density..), geom = "bar")  # density는 통계적 변환(밀도)
#! 밀도를 적용하면 총 밀도의 합은 1이다.

# stat_bin() 적용 영역그래프 시각화
pd <- ggplot(diamonds, aes(price))
pd + stat_bin(aes(fill = cut), geom = "area") 

# stat_bin() 적용 산점도 시각화
pd + stat_bin(aes(color = cut, size = ..density..),
              geom = "point")


## 산점도와 회귀선 적용

# 패키지 및 데이터 준비
library(UsingR)  # galton 데이터를 위한 라이브러리

data("galton")
head(galton)
summary(galton)
str(galton)

# 산점도와 회귀선 적용하기
pg <- ggplot(galton, aes(x = parent, y = child))  # 미적요소 객체
pg + geom_count() + geom_smooth(method = "lm") 
#! geom_count()는 숫자 크기에 따른 산점도 시각화
#! geom_smooth는 선, method = "lm"은 회귀선, 보조선 시각화
#! 여기서 보조선의 퍼짐 정도는 회귀모델의 예측치와 관측치간의 오차이다. 따라서 퍼짐 정도가 클수록 모델의 설명력은 떨어진다.


## 테마 적용

# 제목을 설정한 산점도 시각화
pd <- ggplot(diamonds, aes(carat, price, color = cut))
pd + geom_point() + ggtitle("다이아몬드 무게와 가격의 상관관계")

# theme() 이용하여 그래프의 외형 속성 적용하기
pd + theme(title = element_text(color = "blue", size = 25),  # 제목 텍스트 색상, 크기
          axis.title = element_text(size = 14, face = "bold"),  # 축 제목
          axis.title.x = element_text(color = "green"),  # x축 제목
          axis.title.y = element_text(color = "green"),  # y축 제목
          axis.text = element_text(size = 14),  # 축이름 크기
          axis.text.x = element_text(color = "purple"),  # x축 이름 색상
          axis.text.y = element_text(color = "red"),  # y축 이름 색상
          legend.title = element_text(size = 20,
                                      face = "bold",
                                      color = "red"),  # 범례 글자 속성
          legend.position = "bottom",  # 범례위치
          legend.direction = "horizontal"  # 범례방향
          ) + geom_point() + ggtitle("다이아몬드 무게와 가격의 상관관계")


## ggsave()

# 그래프 시각화
pd <- ggplot(diamonds, aes(carat, price, color = cut))
pd + geom_point()

# 가장 최근에 그려진 그래프 저장
ggsave(file = "/rwork/output/diamond_price.pdf")

ggsave(file = "/rwork/output/diamond_price.jpg",
       dpi = 72)

# 변수에 저장된 그래프 저장
pd <- ggplot(diamonds, aes(clarity))
pd <- pd + geom_bar(aes(fill = cut), position = "fill")
pd

ggsave(file= "/rwork/output/bar.png",
       plot = pd, width = 10, height = 5)




#### 지도 공간 기법 시각화



### Stamen Maps API 이용


## 패키지 
install.packages("ggmap")
library(ggmap)
library(ggplot2)


## 위도와 경도 중심으로 지도 시각화

# 서울 지역의 중심 좌표 설정
seoul <- c(left = 126.77, bottom = 37.40,
           right = 127.17, top = 37.70)
seoul.map <- get_stamenmap(seoul, zoom = 12,
                           maptype = 'terrain')
ggmap(seoul.map)


## 지도 이미지에 레이어 적용

# 데이터 준비
pop <- read.csv(file.choose(), header = T)
head(pop)

library(stringr)

region <- pop$'지역명'
lon <- pop$LON
lat <- pop$LAT
tot.pop <- as.numeric(str_replace_all(pop$'총인구수',
                                      ',', ''))  # 총인구수에 구분자(,) 제거
tot.pop.df <- data.frame(region, lon, lat, tot.pop)
tot.pop.df  # 필요한 칼럼으로만 데이터프레임화

tot.pop.df <- tot.pop.df[-18, ]  # total 제외
tot.pop.df

# 정적 지도 이미지 준비
daegu <- c(left = 123.4423013, bottom = 32.8528306,
           right = 131.601445, top = 38.8714354)

daegu.map <- get_stamenmap(daegu, zoom = 7,
                           maptype = 'watercolor')

#! 서울과 제주도를 포함한 대한민국 내륙 전체 지도를 나타내기 위해 대구를 중심으로 둠
x11()
# 지도 시각화
layer1 <- ggmap(daegu.map)
layer1

# 포인트 추가
layer2 <- layer1 + geom_point(data = tot.pop.df,
                              aes(x = lon, y = lat,
                                  color = factor(tot.pop), size = factor(tot.pop)))
layer2

# 텍스트 추가
layer3 <- layer2 + geom_text(data = tot.pop.df,
                             aes(x = lon + 0.01, 
                                 y = lat + 0.05,
                                 label = region),
                             size = 3)
layer3

# 크기를 지정하여 파일로 저장
ggsave("/rwork/pop201901.png", scale = 1, width = 10.24,
       height = 7.68)
