exam1 <- read.csv("exam1.csv", header = T)

## Select(선택)
# SPSS Modeler 에서 셀릭트 노드의 역할을 함
# SQL의 조회기능
exam1[exam1$Exam2 >= 1 & exam1$Quiz < 3.9,]

# subset : R에서 조회시 사용하는 함수
subset(exam1, Exam2 >= 1 & Quiz < 3.9)
subset(exam1, ID==3, c(Exam1, Exam2))

## Append(추가)
# SPSS Modeler 에서 밑에 row 추가를 하는 노드
app <- c(6,3.5, 1.5, 3.5)
rbind(exam1, app) # row bind : row형태로 vector를 추가

## Sort(정렬)
# SPSS Modeler 에서 특정 컬럼을 기준으로 정렬 
# indexing 매기는 기준은 정렬한 값이 원래데이터의 몇번째에 위치했는지를 나타냄
# 즉 원래 위치의 인덱스를 준다
exam1[order(exam1$Quiz, decreasing =  T), ]
exam1[order(exam1$Quiz),]

## Sample(추출)
# SPSS Modeler 에서 임의의 데이터를 뽑는 노드
exam1[sample(1:nrow(exam1),3),] # 1~exam1의 row까지 랜덤으로 3개
exam1[as.logical((1:nrow(exam1))%%2), ] # 1-in-n sampling 

## Aggregate(통합)
# SPSS Modeler 에서 그룹핑 하는 노드 
exam3 <- read.csv("exam3.csv", header = T)
head(exam3)
aggregate(exam3[2:4], by=list(Gender = exam3$Gender), mean) # 남,여 별 평균 시험점수
attach(CO2)
aggregate(CO2[4:5], by=list(Type = Type, Treatment = Treatment), mean) # Type, Treatment 별 평균
# group_by를 쓸 경우
exam3 %>% 
  group_by(Gender) %>%
  summarise(exam1_mean = mean(Exam1), exam2_mean = mean(Exam2), quiz_mean = mean(Quiz))


## Distinct(구별)
# SPSS Modeler 에서 중복 제거를 하는 노드
exam1[!duplicated(exam1$Quiz),]
subset(exam1, !duplicated(Quiz))
unique(exam1$Quiz)

## Derive(파생)
# SPSS Modelter 에서 기존변수를 활용한 다른 변수를 생성하는 노드
exam1$ExamSum <- exam1$Exam1 + exam1$Exam2; head(exam1)
exam1 <- transform(exam1, ExamMean = (Exam1+Exam2)/2); exam1

## Filter(필터)
# SPSS Modeler 에서 특정변수를 제외한 나머지 출력하는 노드
exam1[-1]
exam1[c(-1,-5)]

## Merge(병합)
# SPSS Modeler 에서 특정 키 값으로 테이블 병합
merge(exam1, exam3, by="ID")

## Transpose(전치)
# SPSS Modeler 에서 행과 열을 바꿔주는 노드
t(exam1)
t(t(exam1)) # as.data.frame() 를 쓰는 것과 같음
exam1

## Restructure(재구성) - 합계 및 평균값
# 하나 또는 여러개의 카테고리 변수를 통해 새로운 필드를 만드는 노드 
tapply(exam3$Quiz, exam3$Gender, sum)
tapply(exam3[,2], exam3$Gender, mean)

## Restructure(재구성) - Melt & Cast
# 하나 또는 여러개의 카테고리 변수를 통해 새로운 필드를 만드는 노드 
library(reshape)
tr <- read.csv("tr.csv", header = T)
tr.melt <- melt(tr, id.vars = c("id", "site")) # id.vars 값을 기준으로 아래로 펼침
measure.vars = c("pageview", "dwelltime")

# formular = var1~var2 : var1의 level을 행, var2를 열로 설정
tr.cast <- cast(tr.melt, id~site, sum, subset = variable == "pageview") 
tr.cast

tr.cast2 <- cast(tr.melt, id+site~variable, length)
tr.cast2

tr.cast3 <- cast(tr.melt, id~variable, mean, subset = variable == "pageview")
tr.cast3

tr.cast4 <- cast(tr.melt, id ~ variable, range)
tr.cast4
# aggregate를 사용한 경우 
# aggregate(tr[,c(1,3)], by = list(id=tr[,c(1,3)]$id), mean)

## Binning(구간화)
# 연속형 변수를 범주형으로 변경하는 노드
exam1$ExamSum <- exam1$Exam1+exam1$Exam2
exam1$Level <- cut(exam1$ExamSum, breaks=3, labels=F); exam1
exam1$Level <- cut(exam1$ExamSum, c(0,2,4,6,8), labels = F); exam1

######################################################################################

cs <- read.table("dataCustomers.tab", sep="\t", header = T, stringsAsFactors = F)
tr <- read.table("dataTransactions.tab", sep="\t", header = T, stringsAsFactors = F)

## Exercise 1
tail(subset(cs, age>=50 & age < 60 & gender == "여" & marriage == "기혼"), 6)
## Exercise 2
aggregate(cs[3], by = list(gender = cs$gender), mean)
## Exercise 3
cs[!duplicated(cs$residence),]$residence
## Exercise 4
head(tr)
# 수입, 국산 여부값을 나타내는 import 값을 조건문을 통해 새로운 변수 생성
tr$import_name <- ifelse(tr$import == 1, "수입품", "국산품")
table(tr$store, tr$import_name)

## Exercise 5  data없음

## Exercise 6 
p5 <- aggregate(tr[8], by=list(ID=tr$custid), sum)
head(p5[order(p5$amount, decreasing=T),],10)

## Exercise 7
library(reshape)
mg <- merge(tr,cs, by = 'custid')
mg.1 <- subset(mg, job=="개인사업")
mg.1$ym <- paste(substr(mg.1$datetime, 1,4), substr(mg.1$datetime,6,7), sep="")
mg.1.m <- melt(mg.1, id.vars = c("custid", "ym"), measure.vars = "amount")
head(cast(mg.1.m, custid~ym, length))

## Exercise 8
?weekdays
tr$wd <- weekdays(as.Date(substr(tr$datetime, 1,10), "%Y-%m-%d"), abbreviate = T) # 요일 구하기
tr$wd <- factor(tr$wd, levels = c("월","화","수","목","금","토","일")) # 요일화
tr.m <- melt(tr, id.vars = c("store", "wd"), measure.vars = "amount")
cast(tr.m , store~wd, sum)

## Exercise 9
a <- aggregate(tr[8], by=list(custid = tr$custid, coner = tr$corner), length)
head(a)
b <- aggregate(a[3], by=list(custid = a$custid), length)
b$coverage <- b$amount/26
head(b[,c(1,3)],10)
