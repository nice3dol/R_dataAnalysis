# 3. java, rJava 설치하기
install.packages("multilinguer")
library(multilinguer)
install_jdk()

# 4. 의존성 패키지 설치하기
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")

# 5. github 버전 설치하기
install.packages("remotes")

# 6. KoNLP 설치하기(64bit에서만 동작)
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

# 7. Test
library(KoNLP)
extractNoun('슬기로운 공빅생활')

libp