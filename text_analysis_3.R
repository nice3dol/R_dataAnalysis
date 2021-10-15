install.packages("httr")
install.packages("XML")
library(httr)
library(XML)

url <- "http://media.daum.net"
web <- GET(url) 
web

# html 태그 파싱
html <- htmlTreeParse(web, useInternalNodes = T, trim = T,
                      encoding = "utf-8")
# html root node 찾기
rootNode <- xmlRoot(html)
# 태그 자료 수집
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']",
                    xmlValue)
news

# 전처리
news_pre <- gsub("[\r\n\t]", " ", news)  # 이스케이프 제거
news_pre <- gsub("[[:punct:]]", " ", news_pre)  # 문장부호 제거
news_pre <- gsub("[[:cntrl:]]", " ", news_pre)  # 특수문자 제거
news_pre <- gsub("\\d+", " ", news_pre)  # 숫자제거
news_pre <- gsub("[a-z]+", " ", news_pre)  # 영문자 제거
news_pre <- gsub("[A-Z]+", " ", news_pre)  # 영문자 제거
news_pre <- gsub("\\s+", " ", news_pre)  # 2개 이상 공백 교체 
news_pre

# 기사와 상관 없는 항목 제거
news_data <- news_pre[1:46]
news_data <- news_data[-(25:28)]
news_data

# 파일 저장과 읽기
setwd("/rwork/output")
write.csv(news_data, "news_data.csv", quote = F)
news_data <- read.csv("news_data.csv", header = T, stringsAsFactors = F)  # 문자열 형식으로 읽어오기
str(news_data)

names(news_data) <- c("no", "news_text")  # 칼럼명 지정 
head(news_data)

news_text <- news_data$news_text  # 기사 벡터 생성
news_text

# 토픽 분석
library(KoNLP)
user.dic <- data.frame(term = c("코로나", "박영선", "오세훈", "아이티"), tag = 'ncn')  # 추가 단어 만들기
buildDictionary(ext_dic = 'sejong', user_dic = user.dic)  # 세종에 단어 추가

# 단어 추출 사용자 함수 정의
ex_nouns <- function(x){
  paste(extractNoun(x), collapse = " ")
}
news_nouns <- sapply(news_text, ex_nouns)  # 함수로 단어 추출
news_nouns

# 말뭉치 생성
library(tm)
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
inspect(newsCorpus[1:5])

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4, 16)))  # 집계 행렬 생성
TDM

tdm.df <- as.data.frame(as.matrix(TDM))  # data.frame 구조로 변경
dim(tdm.df)

# 단어 출현 빈도수 구하기
word_result <- sort(rowSums(tdm.df), decreasing = T)
word_result

# WordCloud 생성
library(wordcloud2)
myNames <- names(word_result)
myNames

df <- data.frame(word = myNames, Freq = word_result)  # 단어 빈도수 구하기
head(df)

wordcloud2(df, color = "random-light", backgroundColor = "black", size = 0.3, fontFamily = "나눔바른고딕")
