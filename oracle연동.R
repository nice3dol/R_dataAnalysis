### oracle 연동
install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")

library(DBI)
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk1.8.0_291")
library(rJava)  # RJDBC는 rJava에 의존적이기 때문에 rJava 패키지가 로딩 되어 있어야 실행 가능
library(RJDBC)


## 드라이버 로딩과 데이터베이스 연동

# 1)Driver 설정
drv <- JDBC("oracle.jdbc.driver.OracleDriver",
            "C:/oracleTest/ojdbc6_g.jar")

# 2)오라클 데이터베이스 연결
conn <- dbConnect(drv,"jdbc:oracle:thin:@//127.0.0.1:1521/xe",
                  "scott", "tiger")


## 데이터베이스로부터 레코드 검색, 추가, 수정, 삭제

# 1)모든 레코드 검색
query = "SELECT * FROM test_table"
dbGetQuery(conn, query)

# 2)정렬 조회
query = "SELECT * FROM test_table order by age desc"
dbGetQuery(conn, query)

# 3)insert
query = "insert into test_table values('kang', '1234', '강감찬', 45)"
dbSendUpdate(conn, query)

# 4)조건 검색
query = "select * from test_table where age >= 40"
result <- dbGetQuery(conn, query)
result

# 5)레코드 수정
query = "update test_table set age = 40 where name = '강감찬'"
dbGetQuery(conn, query)

# 6)레코드 삭제
query = "delete from test_table where name = '홍길동'"
dbSendUpdate(conn, query)

query = "SELECT * FROM test_table"
dbGetQuery(conn, query)

query = "SELECT * FROM GoodsInfo"
dbGetQuery(conn, query)
