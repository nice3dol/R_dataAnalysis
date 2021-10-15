#### 스파크 설치 ####

install.packages("sparklyr")
library(sparklyr)

spark_available_versions()
spark_install(version = "3.0.0")

Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-11.0.11")
sc <- spark_connect(master = "local")


#### 시작하기 ####


### 웹 인터페이스
cars <- copy_to(sc, mtcars)
cars

spark_web(sc)


### 분석

# SQL 사용
library(DBI)
dbGetQuery(sc, "SELECT count(*) From mtcars")

# dplyr 사용
library(dplyr)
count(cars)
select(cars, hp, mpg) %>% sample_n(100) %>% collect() %>% plot()

# 모델링
model <- ml_linear_regression(cars, mpg ~ hp)
model

model %>% ml_predict(copy_to(sc, data.frame(hp = 250 + 10 * 1 : 10))) %>% transmute(hp = hp, mpg = prediction) %>% full_join(select(cars, hp, mpg)) %>% collect() %>% plot()


#### 분석 ####

# 데이터 조회
summarise_all(cars, mean) %>% show_query()

cars %>% mutate(transmission = ifelse(am == 0, "auto", "manual")) %>% group_by(transmission) %>% summarise_all(mean)

summarise(cars, mpg_percentile = percentile(mpg, 0.25)) %>% show_query()

summarise(cars, mpg_percentile = percentile(mpg, array(0.25, 0.5, 0.75))) %>% mutate(mpg_percentile = explode(mpg_percentile))

# 상관관계
install.packages("corrr")
library(corrr)
correlate(cars, use = "pairwise.complete.obs", method = "pearson")

correlate(cars, use = "pairwise.complete.obs", method = "pearson") %>% shave() %>% rplot()

## 시각화
# ggplot2
library(ggplot2)

ggplot(data = mtcars, aes(as.factor(cyl), mpg)) + geom_col()

car_group <- cars %>% group_by(cyl) %>% summarise(mpg = sum(mpg, na.rm = T)) %>% collect() %>% print()

ggplot(data = car_group, aes(as.factor(cyl), mpg)) +
  geom_col(fill = "#999999") + coord_flip()

# dbplot
install.packages("dbplot")
library(dbplot)

cars %>% dbplot_histogram(mpg, binwidth = 3) +
  labs(title = "MPG Distribution", 
       subtitle = "Histogram over miles per gallon")


## 모델링
cars %>% ml_linear_regression(mpg ~ .) %>% summary()

cars %>% ml_linear_regression(mpg ~ hp + cyl) %>% summary()

cars %>% ml_generalized_linear_regression(mpg ~ hp + cyl) %>% summary()

# 캐싱
cached_cars <- cars %>% 
  mutate(cyl = paste0("cyl_", cyl)) %>%
  compute("cached_cars")

cached_cars %>%
  ml_linear_regression(mpg ~ .) %>%
  summary()


## 의사소통
spark_disconnect(sc)

rmarkdown::render("report.Rmd")


#### 모델링 ####

### 개요
download.file(
  "https://github.com/r-spark/okcupid/raw/master/profiles.csv.zip",
  "c:/rwork/okcupid.zip")
unzip("c:/rwork/okcupid.zip", exdir = "c:/rwork/data")
unlink("c:/rwork/okcupid.zip")

profiles <- read.csv("c:/rwork/data/profiles.csv")
profiles
write.csv(dplyr::sample_n(profiles, 10 ^ 3),
          "c:/rwork/data/profiles.csv", row.names = F)

install.packages("ggmosic")
install.packages(c("forcats", "FactoMineR"))

library(forcats)
library(FactoMineR)

okc <- spark_read_csv(
  sc, 
  "c:/rwork/data/profiles.csv", 
  escape = "\"", 
  memory = FALSE,
  options = list(multiline = TRUE)
) %>%
  mutate(
    height = as.numeric(height),
    income = ifelse(income == "-1", NA, as.numeric(income))
  ) %>%
  mutate(sex = ifelse(is.na(sex), "missing", sex)) %>%
  mutate(drinks = ifelse(is.na(drinks), "missing", drinks)) %>%
  mutate(drugs = ifelse(is.na(drugs), "missing", drugs)) %>%
  mutate(job = ifelse(is.na(job), "missing", job))

glimpse(okc)

okc <- okc %>%
  mutate(
    not_working = ifelse(job %in% c("student", "unemployed", "retired"), 1 , 0)
  )

okc %>% 
  group_by(not_working) %>% 
  tally()

# 훈련, 테스트 데이터 나누기
data_splits <- sdf_random_split(okc, training = 0.8, testing = 0.2, seed = 42)
okc_train <- data_splits$training
okc_test <- data_splits$testing

okc_train %>%
  group_by(not_working) %>%
  tally() %>%
  mutate(frac = n / sum(n))

sdf_describe(okc_train, cols = c("age", "income"))

# 연령 분포도
dbplot_histogram(okc_train, age)

prop_data <- okc_train %>%
  mutate(religion = regexp_extract(religion, "^\\\\w+", 0)) %>% 
  group_by(religion, not_working) %>%
  tally() %>%
  group_by(religion) %>%
  summarize(
    count = sum(n),
    prop = sum(not_working * n) / sum(n)
  ) %>%
  mutate(se = sqrt(prop * (1 - prop) / count)) %>%
  collect()

prop_data

# 종교별 고용 지표
prop_data %>%
  ggplot(aes(x = religion, y = prop)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = prop - 1.96 * se, ymax = prop + 1.96 * se),
                width = .1) +
  geom_hline(yintercept = sum(prop_data$prop * prop_data$count) /
               sum(prop_data$count))

contingency_tbl <- okc_train %>% 
  sdf_crosstab("drinks", "drugs") %>%
  collect()

contingency_tbl

# 모자이크 플롯
library(ggmosaic)
library(forcats)
library(tidyr)
install.packages("ggmosaic")

contingency_tbl %>%
  rename(drinks = drinks_drugs) %>%
  gather("drugs", "count", missing:sometimes) %>%
  mutate(
    drinks = as_factor(drinks) %>% 
      fct_relevel("missing", "not at all", "rarely", "socially", 
                  "very often", "desperately"),
    drugs = as_factor(drugs) %>%
      fct_relevel("missing", "never", "sometimes", "often")
  ) %>%
  ggplot() +
  geom_mosaic(aes(x = product(drinks, drugs), fill = drinks, 
                  weight = count))

# ggplot을 활용한 결과 플로팅
dd_obj <- contingency_tbl %>% 
  tibble::column_to_rownames(var = "drinks_drugs") %>%
  FactoMineR::CA(graph = FALSE)  # 요인수준간 요약

dd_drugs <-
  dd_obj$row$coord %>%
  as.data.frame() %>%
  mutate(
    label = gsub("_", " ", rownames(dd_obj$row$coord)),
    Variable = "Drugs"
  )

dd_drinks <-
  dd_obj$col$coord %>%
  as.data.frame() %>%
  mutate(
    label = gsub("_", " ", rownames(dd_obj$col$coord)),
    Variable = "Alcohol"
  )

ca_coord <- rbind(dd_drugs, dd_drinks)

ggplot(ca_coord, aes(x = `Dim 1`, y = `Dim 2`, 
                     col = Variable)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = label)) +
  coord_equal()

### Feature engineering

scale_values <- okc_train %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age)) %>% collect()

scale_values

okc_train <- okc_train %>% mutate(scaled_age =
                                    (age - !!scale_values$mean_age) / !!scale_values$sd_age)

dbplot_histogram(okc_train, scaled_age)

okc_train %>%
  group_by(ethnicity) %>%
  tally()

# 인종에 대한 더미변수 생성
ethnicities <- c("asian", "middle eastern", "black", "native american", "indian", 
                 "pacific islander", "hispanic / latin", "white", "other")
ethnicity_vars <- ethnicities %>% 
  purrr::map(~ expr(ifelse(like(ethnicity, !!.x), 1, 0))) %>%
  purrr::set_names(paste0("ethnicity_", gsub("\\s|/", "", ethnicities)))
okc_train <- mutate(okc_train, !!!ethnicity_vars)
okc_train %>% 
  select(starts_with("ethnicity_")) %>%
  glimpse()

okc_train <- okc_train %>%
  mutate(
    essay_length = char_length(paste(!!!syms(paste0("essay", 0:9))))
  ) %>% compute()
okc_train

dbplot_histogram(okc_train, essay_length, bins = 100)

spark_write_parquet(okc_train, "data/okc-train.parquet")


### 지도학습

# 집합목록생성
vfolds <- sdf_random_split(
  okc_train,
  weights = purrr::set_names(rep(0.1, 10), paste0("fold", 1:10)),
  seed = 42
)

# 분할 생성
analysis_set <- do.call(rbind, vfolds[2:10])
assessment_set <- vfolds[[1]]

# age 변수 처리
make_scale_age <- function(analysis_data) {
  scale_values <- analysis_data %>%
    summarize(
      mean_age = mean(age),
      sd_age = sd(age)
    ) %>%
    collect()
  
  function(data) {
    data %>%
      mutate(scaled_age = (age - !!scale_values$mean_age) / !!scale_values$sd_age)
  }
}

scale_age <- make_scale_age(analysis_set)
train_set <- scale_age(analysis_set)
validation_set <- scale_age(assessment_set)

# 로지스틱회귀 모델링
lr <- ml_logistic_regression(
  analysis_set, not_working ~ scaled_age + sex + drinks + drugs + essay_length
)
lr

# 모델 평가
validation_summary <- ml_evaluate(lr, assessment_set)
validation_summary

# ROC곡선
roc <- validation_summary$roc() %>%
  collect()

ggplot(roc, aes(x = FPR, y = TPR)) +
  geom_line() + geom_abline(lty = "dashed")

validation_summary$area_under_roc()

# 분할 분석
cv_results <- purrr::map_df(1:10, function(v) {
  analysis_set <- do.call(rbind, vfolds[setdiff(1:10, v)]) %>% compute()
  assessment_set <- vfolds[[v]]
  
  scale_age <- make_scale_age(analysis_set)
  train_set <- scale_age(analysis_set)
  validation_set <- scale_age(assessment_set)
  
  model <- ml_logistic_regression(
    analysis_set, not_working ~ scaled_age + sex + drinks + drugs + essay_length
  )
  s <- ml_evaluate(model, assessment_set)
  roc_df <- s$roc() %>% 
    collect()
  auc <- s$area_under_roc()
  
  tibble(
    Resample = paste0("Fold", stringr::str_pad(v, width = 2, pad = "0")),
    roc_df = list(roc_df),
    auc = auc
  )
})

unnest(cv_results, roc_df) %>%
  ggplot(aes(x = FPR, y = TPR, color = Resample)) +
  geom_line() + geom_abline(lty = "dashed")

mean(cv_results$auc)


## 일반화 선형 회귀

# 로지스틱 회귀
glr <- ml_generalized_linear_regression(
  analysis_set, 
  not_working ~ scaled_age + sex + drinks + drugs, 
  family = "binomial"
)

tidy_glr <- tidy(glr)

# 계수 추정
tidy_glr %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, 
        ymax = estimate + 1.96 * std.error, width = .1)
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed")


## 기타 모델

# 신경망 모델에 적합
nn <- ml_multilayer_perceptron_classifier(
  analysis_set,
  not_working ~ scaled_age + sex + drinks + drugs + essay_length, 
  layers = c(12, 64, 64, 2)
)

# 예측
predictions <- ml_predict(nn, assessment_set)

ml_binary_classification_evaluator(predictions)



### 비지도 학습

## 데이터 준비
essay_cols <- paste0("essay", 0:9)
essays <- okc %>%
  select(!!essay_cols)
essays %>% 
  glimpse()

essays <- essays %>%
  # Replace `missing` with empty string.
  mutate_all(list(~ ifelse(. == "missing", "", .))) %>%
  # Concatenate the columns.
  mutate(essay = paste(!!!syms(essay_cols))) %>%
  # Remove miscellaneous characters and HTML tags
  mutate(words = regexp_replace(essay, "\\n|&nbsp;|<[^>]*>|[^A-Za-z|']", " "))

# 주제별 단어 추출
stop_words <- ml_default_stop_words(sc) %>%
  c(
    "like", "love", "good", "music", "friends", "people", "life",
    "time", "things", "food", "really", "also", "movies"
  )


lda_model <-  ml_lda(essays, ~ words, k = 6, max_iter = 1, min_token_length = 4, 
                     stop_words = stop_words, min_df = 5)

betas <- tidy(lda_model)
betas

# 시각화
betas %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


spark_disconnect(sc)
