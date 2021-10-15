library(KoNLP)
library(tm)
library(wordcloud)
library(wordcloud2)

facebook <- file("/rwork/dataset3/facebook_bigdata.txt", encoding = "UTF-8")
facebook_data <- readLines(facebook)
head(facebook_data)

# term = "추가단어", tag = ncn(명사지시코드)
user_dic <- data.frame(term = c("R 프로그래밍", "페이스북", "김진성", "소셜네트워크"), tag = 'ncn')

buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

# Sejong 사전에 등록된 신규 단어 테스트
paste(extractNoun("김진성은 많은 사람과 소통을 위해서 소셜네트워크에 가입하였습니다"), collapse = " ")

# 단어 추출을 위한 함수 정의
exNouns <- function(x){
  paste(extractNoun(as.character(x)), collapse = " ")
}
facebook_nouns <- sapply(facebook_data, exNouns)
facebook_nouns[1]

# 추출 단어 전처리
myCorpus <- Corpus(VectorSource(facebook_nouns))
myCorpus  # 말뭉치 생성

myCorpusPrepro <- tm_map(myCorpus, removePunctuation)  # 문장부호 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)  # 숫자 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)  # 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('en'))  # 불용어 제거
inspect(myCorpusPrepro[1:5])

# 단어 선별
myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4, 16)))  # 2~8 음절 단어 선정
myCorpusPrepro_term
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))  # matrix를 data.frame 으로 변경
dim(myTerm_df)

# 단어 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult[1:20]

# 불용어 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
myStopwords <- c(stopwords("en"), "사용", "하기")
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, myStopwords)

myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4, 16))) 
myCorpusPrepro_term
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))

wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult[1:20]

# 단어 구름 시각화
myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult)
str(word.df)  # 단어와 빈도수로 DF 생성
pal <- brewer.pal(12, "Paired")  # 단어 색상과 글꼴 지정
wordcloud(word.df$word, word.df$freq, scale = c(5, 1), 
          min.freq = 3, random.order = F,
          rot.per = .1, colors = pal, family = "malgun")

wordcloud2(word.df, color = "random-light",
           backgroundColor = "black", 
           fontFamily = '나눔바른고딕')

