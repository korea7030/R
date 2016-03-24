getwd()
############################
########## 20150418_강의내용 ##########
## Factor(요인) 타입 ##
## 레벨을 지정할 수 있다.
## 주로 카테고리 분류를 위해 factor를 사용

fac1 <- factor(c("Male", "Female", "Female", "Male"))
fac2 <- factor(c(1,2,1,1,2,3))
fac1
fac2
## 객체들에 대한 속성을 나타냄( ex : factor냐 array 냐 를 확인할 때)
attributes(fac1)
attributes(fac2)

### sample (x, size, replace=FALSE)  : 임의의 vector를 size만큼 random하게 뽑아내서 나열
## replace : 중복 허용 여부 TRUE일 경우 허용, FALSE일 경우 허용하지 않음

fac3 <- factor(sample(c("high","middle","low"), 20, replace=TRUE))
fac3
attributes(fac3)
## 빈도수 측정 할 때
table(fac3)
## 막대그래프
barplot(table(fac3))

fac4 <- factor(fac3, levels=c("low", "middle", "high"), ordered=TRUE)
fac4

## str : 객체의 구조를 나타냄
## attributes와의 차이는 attributes는 단순히 객체가 뭐다 라는 것만 나타내지만,(ex array, factor ...)
## str의 경우는 그 객체가 무엇이고, 값이 어떻게 구성되어 있는지 알려준다.
str(fac4)
barplot(table(fac4))

fac5 <- factor(sample(0:1, 20, replace=TRUE))  ## 0~1 값을 20번 반복하는데 중복을 허용
fac5

table(fac5)
## label의 이름 나타냄
levels(fac5)

## labels 의 경우 factor의 분류 이름을 나타내며, 변경이 가능하다. 
## 아래 두개를 통해 차이점을 알 수 있음.

## labels 지정한 경우
fac6 <- factor(fac5, labels=c("abs", "pre"))
fac6

## labels 지정 안한 경우
fac6 <- factor(fac5)
fac6

install.packages("moonBook")
library(moonBook)

## moonBook packages를 할 경우 acs 데이터가 있음
data(acs)
## acs 데이터의 구조
str(acs)
## 정보 요약 
summary(acs)

## 해당 데이터에 대해 통계적인 table을 제공(선형통계분석의 기술통계량 느낌이 남)
## 앞의 부분은  Dx를 기준으로 통계치를 뽑아라 라는 의미
mytable(Dx~., data=acs)

## 데이터 읽기/내보내기
## file.choose() : 파일 선택 화면이 나타난다 
## 파일명을 써줘도 좋지만, 오타가 많을 경우도 있기 때문에 file.choose()를 쓰기도 함. 
Q42014 <- read.csv(file.choose(), header=TRUE, na.strings="NA", stringsAsFactors=TRUE)
Q32014 <- read.csv(file.choose(), header=TRUE, na.strings="NA", stringsAsFactors=TRUE)

### 파일명을 쓸 경우
# Q42014 <- read.csv("./dataSmpl/2014-Q4-Trips-History-Data_smpl.csv",stringsAsFactor=F)
# Q32014 <- read.csv("./dataSmpl/2014-Q3-Trips-History-Data3_smpl.csv",stringsAsFactor=F)


head(Q42014)
head(Q32014)
head(Q42014,1)
head(Q32014,1)

Q42014$dateStart <- strptime(Q42014$Start.date , format="%Y-%m-%d %H:%M")
Q42014$dateEnd <- strptime(Q42014$End.date , format="%Y-%m-%d %H:%M")
Q42014$rideTime <- (as.numeric(Q42014$dateEnd - Q42014$dateStart)/60)


hist(Q42014$rideTime)
hist(Q42014$rideTime, breaks=200)

## Q3에 대해 그래프 나타내기 ## 
## 주의사항 : Q4와 Q3의 날짜 형식이 다르다는걸 확인해야 합니다. 
## Rstudio에 변수 올라와있는 형태를 보면 Q32014의 변수의 오른쪽 맨 끝에 표형태의 그림이 있는데 그걸 선택해서 날짜 형식을 확인! 
Q32014$dateStart <- strptime(Q32014$Start.date , format="%m/%d/%Y %H:%M")
Q32014$dateStart
Q32014$dateEnd <- strptime(Q32014$End.date , format="%m/%d/%Y %H:%M")
Q32014$dateEnd
Q32014$rideTime <- (as.numeric(Q32014$dateEnd - Q32014$dateStart)/60)

## breaks : 히스토그램을 몇개의 구간으로 쪼개느냐
hist(Q32014$rideTime)
hist(Q32014$rideTime, breaks=200)

################

#######################
#######################################

