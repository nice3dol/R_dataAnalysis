library(dplyr)
library(readxl)
library(stringr)

### 데이터 가져오기 및 전처리

bus_pop <- read.csv("d:/rwork/envdata/bus_pop.csv")
head(bus_pop)

bus_pop$total <- rowSums(bus_pop[-c(1:2)], na.rm = TRUE)

head(bus_pop)[-c(1:2)]
bus_pop <- bus_pop[-(3:50)]

sum(is.na(bus_pop))
summary(bus_pop)
bus_pop <- na.omit(bus_pop)


# EDA
pp <- bus_pop %>% group_by(버스정류장ARS번호) %>% summarise(sum(total))
pp <- as.data.frame(pp)

head(pp)
names(pp) <- c("station", "pop")

pp %>% arrange(-pop)

quantile(pp$pop)
boxplot(pp$pop)

pp[2,2] <- 5452887
head(pp)

pp %>% filter(pop > 5000000) %>% sum()
pp %>% filter(pop > 4000000) %>% sum()
pp %>% filter(pop > 3000000) %>% sum()
b <- pp %>% filter(pop > 1000000) %>% sum()
pp %>% filter(pop > 0 & pop <= 3000000) %>% sum()
c <- pp %>% filter(pop > 1000000)
a <- sum(pp$pop)
pp
b
b / a * 100
a


# 정류장 위치 정보 
bus_location <- read.csv("d:/rwork/bus_location.csv")
head(bus_location)
bus_location <- bus_location[-1]

head(bus_location)

final_bus <- inner_join(c, bus_location, by = c('station'= 'ARS.ID'))

write.csv(final_bus, "final_bus.csv")


### 도로재비산먼지 데이터

# 1월
m1 <- read_xlsx("d:/rwork/envdata/1m.xlsx", skip = 3)
m1 <- m1[, c(6, 11 , 12)]
m1 <-  as.data.frame(m1)
names(m1) <- c("도로명", "농도", "범례")

y <- str_sub(m1$도로명, -5)
m1$도로명 <- str_replace(m1$도로명, y, "")
m1$도로명 <- gsub("[()]", "", m1$도로명)
head(m1)

# 2월
m2 <- read_xlsx("d:/rwork/envdata/2m.xlsx", skip = 3)
m2 <- m2[, c(6, 11 , 12)]
m2 <-  as.data.frame(m2)
names(m2) <- c("도로명", "농도", "범례")

y <- str_sub(m2$도로명, -5)
m2$도로명 <- str_replace(m2$도로명, y, "")
m2$도로명 <- gsub("[()]", "", m2$도로명)
head(m2)

# 3월
m3 <- read_xlsx("d:/rwork/envdata/3m.xlsx", skip = 3)
m3 <- m3[, c(6, 11 , 12)]
m3 <-  as.data.frame(m3)
names(m3) <- c("도로명", "농도", "범례")

y <- str_sub(m3$도로명, -5)
m3$도로명 <- str_replace(m3$도로명, y, "")
m3$도로명 <- gsub("[()]", "", m3$도로명)
head(m3)

# 4월
m4 <- read_xlsx("d:/rwork/envdata/4m.xlsx", skip = 3)
m4 <- m4[, c(6, 11 , 12)]
m4 <-  as.data.frame(m4)
names(m4) <- c("도로명", "농도", "범례")

y <- str_sub(m4$도로명, -5)
m4$도로명 <- str_replace(m4$도로명, y, "")
m4$도로명 <- gsub("[()]", "", m4$도로명)
head(m4)

# 5월
m5 <- read_xlsx("d:/rwork/envdata/5m.xlsx", skip = 3)
m5 <- m5[, c(6, 11 , 12)]
m5 <-  as.data.frame(m5)
names(m5) <- c("도로명", "농도", "범례")

y <- str_sub(m5$도로명, -5)
m5$도로명 <- str_replace(m5$도로명, y, "")
m5$도로명 <- gsub("[()]", "", m5$도로명)
head(m5)

# 6월
m6 <- read_xlsx("d:/rwork/envdata/6m.xlsx", skip = 3)
m6 <- m6[, c(6, 11 , 12)]
m6 <-  as.data.frame(m6)
names(m6) <- c("도로명", "농도", "범례")

y <- str_sub(m6$도로명, -5)
m6$도로명 <- str_replace(m6$도로명, y, "")
m6$도로명 <- gsub("[()]", "", m6$도로명)
head(m6)

# 7월
m7 <- read_xlsx("d:/rwork/envdata/7m.xlsx", skip = 3)
m7 <- m7[, c(6, 11 , 12)]
m7 <-  as.data.frame(m7)
names(m7) <- c("도로명", "농도", "범례")

y <- str_sub(m7$도로명, -5)
m7$도로명 <- str_replace(m7$도로명, y, "")
m7$도로명 <- gsub("[()]", "", m7$도로명)
head(m7)

# 8월
m8 <- read_xlsx("d:/rwork/envdata/8m.xlsx", skip = 3)
m8 <- m8[, c(6, 11 , 12)]
m8 <-  as.data.frame(m8)
names(m8) <- c("도로명", "농도", "범례")

y <- str_sub(m8$도로명, -5)
m8$도로명 <- str_replace(m8$도로명, y, "")
m8$도로명 <- gsub("[()]", "", m8$도로명)
head(m8)

# 9월
m9 <- read_xlsx("d:/rwork/envdata/9m.xlsx", skip = 3)
m9 <- m9[, c(6, 11 , 12)]
m9 <-  as.data.frame(m9)
names(m9) <- c("도로명", "농도", "범례")

y <- str_sub(m9$도로명, -5)
m9$도로명 <- str_replace(m9$도로명, y, "")
m9$도로명 <- gsub("[()]", "", m9$도로명)
head(m9)

# 10월
m10 <- read_xlsx("d:/rwork/envdata/10m.xlsx", skip = 3)
m10 <- m10[, c(6, 11 , 12)]
m10 <-  as.data.frame(m10)
names(m10) <- c("도로명", "농도", "범례")

y <- str_sub(m10$도로명, -5)
m10$도로명 <- str_replace(m10$도로명, y, "")
m10$도로명 <- gsub("[()]", "", m10$도로명)
head(m10)

# 11월
m11 <- read_xlsx("d:/rwork/envdata/11m.xlsx", skip = 3)
m11 <- m11[, c(6, 11 , 12)]
m11 <-  as.data.frame(m11)
names(m11) <- c("도로명", "농도", "범례")

y <- str_sub(m11$도로명, -5)
m11$도로명 <- str_replace(m11$도로명, y, "")
m11$도로명 <- gsub("[()]", "", m11$도로명)
head(m11)

# 12월
m12 <- read_xlsx("d:/rwork/envdata/12m.xlsx", skip = 3)
m12 <- m12[, c(6, 11 , 12)]
m12 <-  as.data.frame(m12)
names(m12) <- c("도로명", "농도", "범례")

y <- str_sub(m12$도로명, -5)
m12$도로명 <- str_replace(m12$도로명, y, "")
m12$도로명 <- gsub("[()]", "", m12$도로명)
head(m12)

road_dust <- rbind(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
road_dust

brt <- road_dust %>% group_by(도로명) %>% summarise(mean = mean(농도)) %>% arrange(-mean)

road_dust$농도 <- as.numeric(road_dust$농도)
str(road_dust)

brt <- as.data.frame(brt)
head(brt)
top_brt <- brt %>% filter(mean >= 100)
top_brt

### 서울시 도로 데이터
seoul_road <- read.csv("d:/rwork/envdata/seoul_road.csv")
head(seoul_road)

select <- inner_join(seoul_road, top_brt, by = c("RN" = "도로명"))
prp <- as.data.frame(unique(select$RN))

write.csv(select, "d:/select.csv")
select
