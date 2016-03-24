input <- scan()
1
2
3
input 

input2 <- scan(what="") ## 문자 입력받을 때 
1
a
3
r
input2

fruits <- read.table('fruits.txt', header=T)

library(googleVis)

install.packages("sqldf")
library(sqldf)
write.csv(Fruits, "Fruits_sql.csv", quote=F, row.names=F)
??read.csv.sql
fruits_2 <- read.csv.sql("Fruits_sql.csv", sql="SELECT * FROM file WHERE Year = 2008")

## 64bit 에서 엑셀가져오기 
install.packages("XLConnect")
library(XLConnect)
data1 <- loadWorkbook("fruits_6.xls", create=T)
data2 <- readWorksheet(data1, sheet="sheet1", startRow=1, endRow=8, startCol = 1, endCol=5)
data2


## clipboard 사용 

fruits6 <- read.delim("clipboard", header=T)
fruits6


## 인터넷데이터 가져오기 
install.packages("XML")
library(XML)

pop <- 'http://en.wikipedia.org/wiki/World_population'
pop

pop_table <- readHTMLTable(pop)
length(poptable)


## oracle 연결
install.packages("DBI")
library(DBI)
library(RODBC)

db <- odbcConnect("test", uid="scott", pwd="tiger")

sql <- sqlQuery(db, "select * from emp")
