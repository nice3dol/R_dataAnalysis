# 1. 위의 각 패키지의 특징과 타 패키지와 차별화된 시각화 기법(차트)이 있다면 이를 설며. #####


# 2. 막대 차트(가로, 세로), 누적막대 차트, 점 차트, 원형 차트, 상자 그래프, 히스토그램, 산, ####
# 중첩자료 시각화, 변수간의 비교 시각화, 밀도그래프, 3차원 산점도 그래프,지도시각ᄒ######

chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c('2018 1분기', '2019 1분기', '2018 2분기', '2019 2분기', 
                       '2018 3분기', '2019 3분기',
                       '2018 4분기', '2019 4분기')


# 세로 막대 그래프   ###########################################################
par(mfrow = c(1,2))
barplot(chart_data, ylim = c(0, 600), col = rainbow(8), 
        main = "2018년도 vs 2019년도 매출 형 " , family="NanumGothicBold",
        ylab = "매출액(단위 : 만원)", xlab = "년도별 분기 현황")

# 가로 막대 차트   ###########################################################
barplot(chart_data, xlim = c(0, 600), col = rainbow(8), 
        main = "2018년도 vs 2019년도 매추 형황 비교" , family="NanumGothicBold",
        horiz = T,
        ylab = "매출액(단위 : 만원)", xlab = "년도별 분기 현황")

# 누적 ᄆ  ############################################################
library(ggplot2)
qplot(clarity, data = diamonds, colour = cut, geom = 'bar')

# 점 차트  #####################################################################
par(mfrow = c(1, 1))
dotchart(chart_data, color = c('blue', 'red'),
         lcolor = "black",
         pch = 1:2,
         labels = names(chart_data),
         xlab = "매출액",
         main = "분기별 판매현황 : ᄌ차트 시각화",
         cex = 1.2,  family="NanumGothicBold")

# 원형 차트 ####################################################################
par(mfrow = c(1,1))
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2, 
    main = "2018 - 2019 년도 분기별 매출 현황", family="NanumGothicBold")



# 상자 그래프 시각화 ###########################################################
boxplot(VADeaths, range = 0)
abline(h = 37, lty = 3, col = "red")



# 히스토그램 시각화 ############################################################

data(iris)
par(mfrow = c(1,2))
hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "darkgreen", 
     main = "iris 꽃받치 길이 Histogram", xlim = c(4.3, 7.9), family="NanumGothicBold")

hist(iris$Sepal.Width, xlab = "iris$Sepal.Length", col = "mistyrose", 
     main = "iris 꽃받침 넓이 Histogram", xlim = c(2.0, 4.5), family="NanumGothicBold")

par(mfrow = c(1,2))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "darkgreen", 
     main = "iris 꽃받침 너ᄇ Histogram: 빈도수",
     xlim = c(2.0, 4.5), family="NanumGothicBold")

hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "darkred", 
     main = "iris 꽃받침 너비 Histogram: 확률 밀ᄃ", freq = F,
     xlim = c(2.0, 4.5), family="NanumGothicBold")


# 산점도 그리기 ############################################################
data("airquality")

xyplot(Ozone ~ Wind, data = airquality)
xyplot(Ozone ~ Wind|Month, data = airquality)
xyplot(Ozone ~ Wind|Month, data = airquality, layout = c(5,1))

convert <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind|Month, data = convert)

data("quakes")
xyplot(lat ~ long, data = quakes, pch =".")
tplot <- xyplot(lat ~ long, data = quakes, pch = '.')
tplot <- update(tplot, main = "Pacific earthquake location since 1964" )
print(tplot)


quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6

convert <- transform(quakes, depth2 = factor(depth2))
xyplot(lat ~ long|depth2, data = convert)

xyplot(Ozone + Solar.R ~ Wind | factor(Month),
       data = airquality,
       col = c('blue', 'red'),
       layout = c(5,1))

# 중첩자료 ᄉ ##############################################################

x <- c(1, 2, 3, 4, 2, 4)
y <- rep(2, 6)
x: y

table(x, y)
plot(x, y)

xy.df <- as.data.frame(table(x,y))
xy.df

plot(x,y,
     pch = "@", col = "blue", cex = 0.5 * xy.df$Freq,
     xlab = "xᄇ터 원소", ylab = "y벡터 원소", family="NanumGothicBold")

library(UsingR)
data(galton)

galtonData <- as.data.frame(table(galton$child, galton$parent))
names(galtonData) = c("child", "parent", "freq")
parent <- as.numeric(galtonData$parent)
child <- as.numeric(galtonData$child)

par(mfrow = c(1,1))
plot(parent, child,
     pch = 21,
     col = "blue",
     bg = "green",
     cex = 0.2 * galtonData$freq,
     xlab = "parent", 
     ylab = "child")


# 변수ᄀ  시각화 #######################################################
attributes(iris)
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])



# 밀도 그래프   #######################################################
densityplot(~ gcsescore|factor(score), data = Chem97,
            groups = gender,
            plot.points = T,
            auto.key =  T)



# 3차원 산점도 시각화  #######################################################
# install.packages("scatterplot3d")
library(scatterplot3d)

iris_setosa = iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ]
iris_virginica = iris[iris$Species == 'virginica', ]

# ᄐ만들기
d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type = 'n')

# 산점도 시각화 
d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width,
            bg = 'orange' , pch = 21)
d3$points3d(iris_versicolor$Petal.Length,
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue' , pch = 21)
d3$points3d(iris_virginica$Petal.Length,
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width,
            bg = 'pink' , pch = 21)


# 지도 시각화  ############################################################
library(ggplot2)
library(ggmap)
pop <- population201901
pop <- read.csv('population201901.csv', header = T)

library(stringr)

region <- pop$지역명
lon <- pop$LON
lat <- pop$LAT
tot_pop <- as.numeric(str_replace_all(pop$'총인구수', ',',''))

df <- data.frame(region, lon, lat, tot_pop)
df

df <- df[1:17, ]

setwd("/Users/yuhayung/Desktop")
daegu <- c(left = 123.4423013, bottom = 32.8528306,
           right = 131.601445, top = 38.8714354)
map <- get_stamenmap(daegu, zoom = 8,  maptype = 'terrain', header = T)

layer1 <- ggmap(map) ; layer1

layer2 <- layer1 + geom_point(data = df,
                              aes(x = lon, y = lat,
                                  color = factor(tot_pop),
                                  size = factor(tot_pop))) ;layer2

layer3 <- layer2 + geom_text(data = df,
                             aes(x = lon + 0.01, y = 0.08,
                                 label = region), size = 8, family="NanumGothicBold") ;layer3




#3. 2번에서 사용한   위의 각 패키지내 관련 함수를 기술하고 이를 이용하여######
# 그래프를 그리고 비교하시오. (ᄒ 기능이 없다면 comment하시오)
library(esquisse)
library(ggplot2)


value <- c(305,450,320,460,330,480,380,520)
period <- c("2018 1분기","2019 1분기",
            "2018 2분기","2019 2분기",
            "2018 3분기","2019 3분기",
            "2018 4분기","2019 4분기")
chart_data2 <- data.frame( Period = period,Value = value)


# 세로 막대 그래프   ###########################################################
ggplot(chart_data2) +
  aes(x = Period, weight = Value) +
  geom_bar(fill = "#112446") +
  labs(
    x = "년도별 분기 현황",
    y = "매출ᄋ",
    title = "2018년도 vs 2019년도 매출 형황 비교"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 24L,
      hjust = 0.5
    ),
    axis.title.y = element_text(size = 20L),
    axis.title.x = element_text(size = 20L)
  )

# 가로 막대 차트   ###########################################################
ggplot(chart_data2) +
  aes(x = Period, weight = Value) +
  geom_bar(fill = "#112446") +
  labs(
    x = "매출액",
    y = "년도별 분기 현황",
    title = "2018년도 vs 2019년도 매출 현황 비교"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 24L,
      hjust = 0.5
    ),
    axis.title.y = element_text(size = 20L),
    axis.title.x = element_text(size = 20L)
  )

# 누적 막대 차트 ############################################################
ggplot(diamonds) +
 aes(x = clarity, colour = cut) +
 geom_bar(fill = "#112446") +
 scale_color_viridis_d(option = "viridis", 
 direction = 1) +
 theme_minimal() +
 theme(legend.position = "left")


# 점 차트  #####################################################################
ggplot(chart_data2, aes(x = value , y = period)) +
  geom_segment(aes(yend = period), xend = 0, colour = "grey50") +
  geom_point(size = 3) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed")
  )

# 원형 차트 ####################################################################
# install.packages("googleVis")
library(googleVis)
Pie1 <- gvisPieChart(data.frame(chart_data2), 
                  option = list(width=600, height=600, title="2018년도 vs 2019년도 매출 현황", 
                              pieHole = 0.5, is3D=TRUE), chartid="donut")
plot(Pie1)

# 상자 그래프 시각화 ###########################################################
vad <- as.data.frame.table(VADeaths)
ggplot(vad) +
  aes(x = Var2, y = Freq) +
  geom_boxplot(shape = "circle", fill = "#DDD3CA") +
  theme_minimal()

# 히스토그램 시각화 ############################################################
ggplot(iris) +
  aes(x = Sepal.Length) +
  geom_histogram(bins = 18L, fill = "#184918") +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 25L),
    axis.title.x = element_text(size = 25L)
  ) +
  xlim(4.5, 8)

ggplot(iris) +
  aes(x = Sepal.Width) +
  geom_histogram(bins = 30L, fill = "#E9CEE7") +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 25L),
    axis.title.x = element_text(size = 25L)
  ) +
  xlim(2L, 5L)


# 산점도 그리기 ############################################################
ggplot(airquality) +
  aes(x = Wind, y = Ozone) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 20L),
    axis.title.x = element_text(size = 20L))


ggplot(airquality) +
  aes(x = Wind, y = Solar.R, colour = Temp) +
  geom_point(shape = "circle", size = 2.3) +
  scale_color_gradient(low = "#DA7979", high = "#D61616") +
  theme_bw() +
  facet_wrap(vars(Month), nrow = 1L)

ggplot(airquality) +
  aes(x = Wind, y = Ozone, colour = Temp) +
  geom_point(shape = "circle", size = 2.3) +
  scale_color_gradient() +
  theme_bw() +
  facet_wrap(vars(Month), nrow = 1L)



# 중첩자료 시각화 ##############################################################
library(UsingR)
data(galton)


ggplot(galtonData) +
  aes(x = child, y = parent, fill = freq) +
  geom_tile(size = 1.2) +
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 25L),
    axis.title.x = element_text(size = 25L)
  )


# 변수간의 비교 시각화 #######################################################





# 밀도 그래프   #######################################################
library(mlmRev)
data(Chem97)

ggplot(Chem97) +
  aes(x = gcsescore, colour = gender) +
  geom_histogram(bins = 33L, fill = "#112446") +
  scale_color_hue(direction = 1) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 20L),
    plot.caption = element_text(size = 20L),
    axis.title.y = element_text(size = 25L),
    axis.title.x = element_text(size = 25L)
  ) +
  facet_wrap(vars(score))



# 3차원 산점도 ᄉ각화  #######################################################

# install.packages("scatterplot3d")
library(scatterplot3d)

str(iris)
str(quakes)


# 3D  그래프 1
cor(iris[ ,-5]) #  Petal.Length , Petal.Width,  Sepal.Length 순 관계성 높음 

cloud( Sepal.Length  ~ Petal.Length * Petal.Width, data = iris,
# 회전 속성 추가 
panel.aspect = 0.9,
screen = list(x = 30, y =30, z= 20),)



# 3D  그래프 2
scatterplot3d(iris[1:150,1:3])
scatterplot3d(iris[1:150,1:3], pch = 16, color = 'red')
colors = c('red', 'blue', 'green')
colors <- as.numeric(iris$Species)
scatterplot3d(iris[1:150,1:3], pch = 16, color = colors)

# 3D  그래프 3

# install.packages('rgl')  # OS: only window packages 
# library(rgl)
plot3d(iris[1:150,1:3])
with(iris, plot3d(iris[1:150, 1:3], type = "s", col = as.integer(iris$Species)))


# 지도 시각화  ##############################################################






