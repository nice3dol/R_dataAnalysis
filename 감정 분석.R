install.packages("textclean")

library(textclean)
library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)

### 감정 사전 ###
# 감정사전 분석
dic <- read_csv("/rwork/tm_dataset/knu_sentiment_lexicon.csv")
head(dic)
dic %>% mutate(sentiment = ifelse(polarity >=1, "pos", ifelse(polarity <= -1, "neg", "neu"))) %>% count(sentiment)

# 감정 사전 테스트
df <- tibble(sentence = c("디자인 예쁘고 마감도 좋아서 만족스럽다.", "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다"))
df <- df %>% unnest_tokens(input = sentence,
                           output = word,
                           token = "words",
                           drop = F)
df <- df %>% left_join(dic, "word") %>% mutate(polarity = ifelse(is.na(polarity), 0 , polarity))
df

score_df <- df %>% group_by(sentence) %>% summarise(score = sum(polarity))
score_df

### 기생충 수상 관련 기사 댓글 분석 ###
raw_news_comment <- read_csv("/rwork/tm_dataset/news_comment_parasite.csv")
head(raw_news_comment)
# 전처리
news_comment <- raw_news_comment %>% mutate(id = row_number(), reply = str_squish(replace_html(reply)))
glimpse(news_comment)

## 토큰화, 감정 점수 부여 ##
word_comment <- news_comment %>% unnest_tokens(input = reply, output = word, token = "words", drop = F)
word_comment %>% select(word, reply)  # 토큰화

word_comment <- word_comment %>% left_join(dic, "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity ))
word_comment %>% select(word, polarity)  # 감정 점수

# 감정 분류
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
word_comment %>% count(sentiment)

# 막대 그래프 만들기
top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n = 10)
top10_sentiment

ggplot(top10_sentiment, aes(x = reorder(word, n),
                            y = n, fill = sentiment)) + 
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL)

## 감정 점수 ##

# 댓글 별 감정 점수 구하기
score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select(score, reply)

# 긍정 댓글
score_comment %>% select(score, reply) %>% arrange(-score)
# 부정 댓글
score_comment %>% select(score, reply) %>% arrange(score)
# 감정 경향
score_comment %>% count(score)

# 막대그래프 만들기
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))  # 감정 분류
frequency_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)  # 감정 빈도와 비율
frequency_score
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))

# 비율누적 막대그래프
frequency_score$dummy <- 0
ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) + 
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

## 감정 범주별 단어 분석  ##

# 감정 범주별 단어 빈도
comment <- score_comment %>% unnest_tokens(input = reply, output = word, token = "words", drop = F) %>% filter(str_detect(word, "[가-힣]") & str_count(word) > 1)

frequency_word <- comment %>% count(sentiment, word, sort = T)
frequency_word %>% filter(sentiment == "pos")  # 긍정 댓글 
frequency_word %>% filter(sentiment == "neg")  # 부정 댓글

# 단어 비교(로그 오즈비)
comment_wide <- frequency_word %>% filter(sentiment != "neu") %>% pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
comment_wide  # 오즈비 산출을 위해 wide form 으로 변경

comment_wide <- comment_wide %>% mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) / (((neg + 1) / (sum(neg + 1))))))
top10_log_odds <- comment_wide %>% group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>% slice_max(abs(log_odds_ratio), n = 10, with_ties = F)  # 동점 제외                                       
top10_log_odds  # 로그 오즈비가 큰 단어 10개씩 추출    

# 막대 그래프 만들기
ggplot(top10_log_odds, aes(x = reorder(word, log_odds_ratio), y = word, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
  
## 감정 사전 수정 ##

# 수정할 단어 선별
score_comment %>% filter(str_detect(reply, "소름")) %>% select(reply)
score_comment %>% filter(str_detect(reply, "미친")) %>% select(reply)
# 감정 사전 수정하기
dic %>% filter(word %in% c("소름", "소름이", "미친"))
new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("소름", "소름이", "미친"), 2, polarity))
new_dic %>% filter(word %in% c("소름", "소름이", "미친"))
# 수정한 사전으로 감정 점수 재부여
word_comment
new_word_comment <- word_comment %>% select(-polarity) %>% left_join(new_dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
# 댓글별 감정 점수 구하기
new_score_comment <- new_word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, reply) %>% arrange(-score)
# 감정 경향 살펴보기
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
new_score_comment %>% count(sentiment)
new_score_comment
# 전처리와 단어 빈도 산출
new_comment <- new_score_comment %>% unnest_tokens(input = reply, output = word, token = "words", drop = F) %>% filter(str_detect(word, "[가-힣]") & str_count(word) > 1)  

new_frequency_word <- new_comment %>% count(sentiment, word, sort = T)
new_frequency_word

# 로그 오즈비
new_comment_wide <- new_frequency_word %>% filter(sentiment != "neu") %>% pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0))  # wide form 변환

new_comment_wide <- new_comment_wide %>% mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) / ((neg + 1) / (sum(neg + 1)))))

# 로그 오즈비 막대그래프
new_top10 <- new_comment_wide %>% group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>% slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
new_top10  # 로그 오즈비 상위 10개씩 산출

ggplot(new_top10, aes(x = reorder(word, log_odds_ratio), y = log_odds_ratio, fill = sentiment)) +
  geom_col() +
  coord_flip() 
  
# 댓글 확인
new_score_comment %>% filter(sentiment == "neg" & str_detect(reply, "좌빨")) %>% select(reply)

# 분석 결과 비교
new_top10 %>% select(-pos, -neg) %>% arrange(-log_odds_ratio)
top10 %>% select(-pos, -neg) %>% arrange(-log_odds_ratio)
