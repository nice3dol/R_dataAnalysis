install.packages("readr")
library(readr)

raw_speeches <-  read_csv("/rwork/tm_dataset/speeches_presidents.csv")
raw_speeches

# 전처리
speeches <- raw_speeches %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))
speeches <- speeches %>% unnest_tokens(input = value, output = word, token = extractNoun)

# 단어 빈도
frequency <- speeches %>% count(president, word) %>% filter(str_count(word) > 1)
frequency

# TF-IDF 구하기
frequency <- frequency %>% bind_tf_idf(term = word, document = president, n = n) %>% arrange(-tf_idf)

# 막대 그래프 만들기
top10 <- frequency %>% group_by(president) %>% slice_max(tf_idf, n = 10, with_ties = F)
top10$president <- factor(top10$president, levels = c("문재인", "박근혜", "이명박", "노무현"))
top10
ggplot(top10, aes(x = reorder_within(word, tf_idf, president), y = tf_idf, fill = president)) + 
  geom_col(show.legend = F) +
  coord_flip() + 
  facet_wrap( ~ president, scales = "free", ncol = 2) +
  scale_x_reordered() + labs(x = NULL)
