library(KoNLP)
install.packages("arules")
library(arules)

marketing <- file("/rwork/dataset3/marketing.txt", encoding = "UTF-8")
marketing2 <- readLines(marketing)
rm(marketing)
head(marketing2)

# 줄 단위 단어 추출
lword <- Map(extractNoun, marketing2)
length(lword)
lword <- unique(lword)
length(lword)
# 중복 단어 제거와 추출 단어 확인
lword <- sapply(lword, unique)
length(lword)
lword
# 단어 필터링 함수 정의
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x){Filter(filter1, x)}
# 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
length(lword)
lword

# 트랜잭션 생성
wordtran <- as(lword, "transactions")
wordtran

# 연관 규칙 발견
tranruls <- apriori(wordtran, parameter = list(supp = 0.25, conf = 0.05))
inspect(tranruls)

# 연관 단어 시각화를 위해서 자료구조 변경
rules <- labels(tranruls, ruleSep = " ")
rules  # => 대신에 공백으로 분리

# 문자열로 묶인 연관 단어 행렬 구조로 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules
class(rules)

rulemat <- do.call(rbind, rules)  # matrix로 변환
class(rulemat)

# 연관어 시각화를 위한 igraph 패키치 설치와 로딩
install.packages("igraph")
library(igraph)

# edgelist 보기
ruleg <- graph.edgelist(rulemat[c(12:59), ], directed = F)
ruleg
rulemat

# edgelist 시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
            vertex.label.cex = 1.2,
            vertex.label.color = 'black',
            vertex.size = 20, vertex.color = 'green',
            vertex.frame.color = 'blue')

