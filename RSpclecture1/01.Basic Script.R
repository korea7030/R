# ====================================
# Varaiables, Vectors
#====================================
# Variables
a<-1
a 
print(a) 
(b<-1) 

# Vectors
x<-c("fee","fie","foe","fum")


c("Everyone", "loves", "stats.",1) 

c(1,1,1,1,1,1,"a")

c(1,1,2,3,5,8,13,21)

c(1*pi, 2*pi, 3*pi, 4*pi) ## pi = 3.141592

## TRUE,T = 1, FALSE,F = 0 

c(TRUE, TRUE, FALSE&T,T|2,3)

v1<-c(1,2,3)
v2<-c(4,5)
v3<-c(v1,v2) # 1,2,3,4,5
v3

print(v1+v2) # 5,7,7
print(v1*v2) # 4,10,12
v1^2 # 1, 4, 9

#====================================
# sequence
#====================================
1:5
b<-2:10
b

10:19
9:-1

e<-10:2
e

seq(from=0,to=20,by=2)  ## 2씩 증가
seq(from=0,to=20,length.out=5)   ## 1~20  사이의 구간 

seq(from=0,to=20,length=5)

seq(from=1,to=2,length.out=5)

seq(from=1,to=2,length=5)

seq(from=1,to=10,by=1)
seq(from=0,to=10,length.out=20)

(x<-seq(from=0,to=10,length.out=21))

n<-0
1:n

?rep
rep(1,times=5)
rep(1:2,times=2)


rep(c)

c<-1:5
rep(c,5)

rep(c,each=5)

rep(1:2,each=4)

#====================================
# type of, mode, class
#====================================
a<-3
a

b<-"Character"
b

paste("a","b", sep = " ")
paste0("a","b")

A<-c("a", "b", "c")
A
paste(A,c("d","e"))

paste(c("a", "b", "c"), c("d","e"))

f<-paste(A,10)
f

hp_1
hp_2
hp_3

rm(i)

for (i in 1:4) {
  assign(paste0("hp_",i),i*2)  
}

i

hp_1
hp_2

name2
name10 

ls()
assign(paste0("name",1),1)

A

paste(A,10,sep="")
paste(A,1:10,sep="")
?paste

paste(c("a","b","c"),1:10,sep="")

paste("Everybody","loves","cats.")
paste("Everybody","loves","cats.",sep="-")
paste("Everybody","loves","cats.",sep="")

#====================================
# Substr
#====================================
substr("BigDataAnalysis",2,4)
?substr

ss<-c("Moe","Larry","Curly")
substr(ss,1,3)

#====================================
# Boolean(logical)
#====================================
c<-TRUE
c
str(c)

d<-T
d


e<-FALSE
e

f<-F
F

a<-3
a==pi
a!=pi
a<pi
a>pi
a<=pi
a>=pi

#====================================
# matrix
#====================================
theData<-c(1.1, 1.2, 2.1, 2.2, 3.1, 3.2)

mat<-matrix(theData,2,3)

mat

dim(mat)

t(mat)

mat%*%t(mat)

?diag
diag(mat)

mat[1,]

mat[,3]

?colnames
colnames(mat)<-c("IBM","MSFT","GOOG")
rownames(mat)<-c("JAN","FEB")
mat

A<-matrix(0,4,5)
A

matrix(1:20,4,5)
A<-matrix(1:20,4,5, byrow = TRUE)
A

A[c(1,4),c(2,3)] 

A[c(1,4),c(2,3)] <- 1
A

A + 1

#====================================
# list
#====================================


lst<-list(3.14,"Moe",c(1,1,2,3),mean)
lst
lst[[1]]
a <- 1:10
b <- matrix(1:10,2,5)
c <- c("name1","name2")

alst<-list("KS"=a,b=b,c=c)
attach(alst)

alst
alst$KS
alst[[1]]

alst[[1]][2]

alst[[2]]

blst<-list(d=2:10*10)
blst
ablst<-c(alst,blst)
ablst
str(ablst)

score1<-list(10,20,30,40,50)
score2<-list(c("a","b"))

score1[score1>40]

score12<-list(c(score1,score2))
score12

score12[1]
score12[[1]]
score12[[1]][1]
score12[[2]][2]

score12[[2]]
score12[[2]][1]

unlist(score1)

unlist(score2)
unlist(score12)

### data frame

a=c(1,2,2,3)
b=c(4.1,3,2,1.1)
d=c("A","B","C","F")
c=data.frame(a,b,d)
?data.frame
c


dim(c)
str(c)

# 조회

c[1,2]
c[2,"a"]
c[2,`a`]
c$a
c[c$a==2,]

c[`a`==2,]

c["a"==2,]

# rbind, cbind
datairis <- data(iris)

View(iris, "X")
head(iris) 

iris

?iris
dim(iris)
summary(iris)

str(iris)

new_R<-data.frame(Sepal.Length=3.5, Sepal.Width=4.1, Petal.Length=2.1, Petal.Width=0.5, Species= "newversicolor" )
new_R2 <- data.frame(3.5, 4.1, 2.1, 0.5, "newversicolor" )
new_R

iris2 <- iris

head(iris2)

dim(cbind(iris, iris2))

head(iris)
tail(iris) # 마지막 2개 observation 조회

nR_iris<-rbind(iris,new_R2)

dim(nR_iris)

head(iris)

a <- iris[-150,]
a

iris[,-4]

tail(nR_iris,2)
dim(nR_iris)

new_C<-1:151
new_C
nRC_iris<-cbind(nR_iris,new_C)
head(nRC_iris,2) # 처음 2개 observation 조회
str(nRC_iris)

save(list = ls(), file="list_Data.RData")


# subset
data(iris)

head(iris)
## subset(data, select=가져올컬럼, subset=가져올데이터범위?)
## select 는 가져올 컬럼을 의미
## subset 부분은 어떤 데이터를 가져올지 지정
## select와 subset의 위치는 바뀔 수 있음.
subset(iris,select=Species,subset=(Petal.Length>5.0))

a<- subset(iris,subset=c(Sepal.Width==3.0 & Petal.Width==0.2),select=c(Sepal.Length,Petal.Length,Species))

head(a)

iris[iris$Sepal.Width==3.0 & iris$Petal.Width==0.2,-Petal.Width]
iris

head(iris)
datairis <- iris

# 

grepsubset <- subset(movies, subset= (movies$title == "%main%"), select = movies$title)

head(grepsubset)
data(iris)
library(datasets)
attach(iris) ## R에 올라와있는 Object의 search path를 지정 

iris[Sepal.Width==3.0&Petal.Width==0.2,]
? attach

A["a"]

# merge(df1, df2, by="df1와 df1의 공통된 열의 이름")
mrg_iris_org<-cbind(no=1:30,iris[c(1:10,51:60,101:110),])

head(mrg_iris_org,2)
tail(mrg_iris_org,2)

mrg_iris_1<-mrg_iris_org[,c(1,2,3)]
mrg_iris_1_ex <- mrg_iris_org[-1, c(1,2,3)]

head(mrg_iris_1,2)
head(mrg_iris_1_ex,2)

mrg_iris_2<-mrg_iris_org[,c(1,4,5,6)]

head(mrg_iris_2,2)

mrg_iris_12<-merge(mrg_iris_1_ex,mrg_iris_2,by="no")

head(mrg_iris_12,2)
head(mrg_iris_org,2)

?merge

mrg_iris_12==mrg_iris_org
table(mrg_iris_12==mrg_iris_org)

# grep(조회할 문자패턴, data)
install.packages("ggplot2")
data(movies,package="ggplot2") # ggplot2 패키지에서 movies data를 가져오라는 명령어
head(movies,2)

head(movies[grep("main",movies$title, ignore.case=F),"title"])

attach(movies)
# movies[,year]
str(movies)

?grep
# 
# grep("main",movies$title)
# 
# head(movies[grep("Main ",movies$title, ignore.case=F),"title"])
# 
# grep("Main ",movies$title)

#====================================
# vector, display subset
#====================================
fib<-c(0,1,1,2,3,5,8,13,21,34)
fib[-1]
fib[-c(1:3)]
fib

fib%%2==0
fib%%2==1

fib[fib%%2==0]

## 위치
(which(fib%%2 == 0))

fib[(which(fib%%2 == 0))]

fib[fib%%2==1]

## e) 자료형, 데이터 구조 변환하기

a<-"2.78"
a
class(a)

as.numeric(a)
class(as.numeric(a))

as.numeric("a")



# 숫자 -> 문자
b<-2.78
b
class(b)
as.character(b)

## boolean 
as.numeric(TRUE)

as.numeric(T)
as.numeric(F)


### 날짜로 변환(as.Date)
# 문자열날짜 > 날짜형태 변환
c<-"2020-01-01"
c
class(c)
c1<-as.Date(c)
c1
class(c1)

e <- "2020-01-02" 
e
e1 <- as.Date(e)

e1 - c1 

c1 > e1

d<-"01312020."
d1<-as.Date(d,format="%m%d%Y.")
d1

# format(날짜,포맷)
# as.character()

as.Date("31/01/2020",format="%d/%m/%Y")


string <- "31/01/2020 10:20:20"

strpdata <- strptime(string, format="%d/%m/%Y %H:%M:%S")

strpdata$hour
strpdata$wday
strpdata$mon

str(strpdata)
lst <- as.list(strpdata)

str(lst)
lst$sec

format(Sys.Date(),format="%d/%m/%Y")
format(Sys.Date(),'%a') # 요일
format(Sys.Date(),'%b') # 월수(7월)
format(Sys.Date(),'%B') # 월
format(Sys.Date(),'%d')
format(Sys.Date(),'%m')
format(Sys.Date(),'%y')
format(Sys.Date(),'%Y')

?Sys.Date()

str(Sys.Date())
?strptime

## f) Missing data

# NA, NaN 
#  NA : 쓰레기값 
#  NaN(Not a Number) : 연산오류

a<-0/0
a

is.nan(a)
is.na(a)

b<-log(0)
b

?is.finite

is.finite(b)

is.nan(b)
is.na(b)

c<-NA
is.na(c)
is.nan(c)

d<-c(1:3,NA)
d

is.na(d)


## g) 벡터의 기본 연산

v1<-c(1,3,5,7,9,11,20)
v1*v1

(v2<-v1+v1^2)
v2<-v1+v1^2
(v3<-1+v1+v1^3)

mean(v1)
median(v1)
sd(v1)
var(v1)
sum((v1-mean(v1))^2)/(length(v1)-1)

cor(v1,v2)


cov(v1,v3)
?cov

## h) 파일 읽기 등
v1
v4<-as.data.frame(v1)

# write.csv(변수이름, “지정할 파일이름.csv”)
# read.csv("저장된 파일이름.csv")
write.csv(v4,"v4.csv")


v5<-read.csv("v4.csv")
v5


?read.table

v6<-as.vector(v4)
v7<-as.vector(v5$v1)

v6==v7

# save(변수이름, file="지정할 데이터 파일이름.Rdata")
# load("저장된 파일이름.Rdata")
save(v4,v5,file="v.rdata")

load("v.rdata")
## 
rm(list=ls(v4))

(v8<-as.list(v4))
(v9<-as.list(v5))


v8$v1==v9$v1
v8[[1]]==v9[[2]]

# rm(object 명)
rm(v4,v5)
rm(list=ls()) # 모두 지우기

# summary
data(iris)
summary(iris) # 열별 data 요약

# install.packages("package명"): package설치
install.packages("party")
install.packages("party")

# library(package명): package를 memory에 load
library(party)

# vignette(“알고싶은 package이름”): party에 대한 tutorial pdf파일
vignette("party")

# q(): R 종료

save(v4,file="./test/v5.rdata")
load("v5.rdata")

getwd()
setwd("C:/Users/user/Documents")

q()
#  save(mrg_iris_1, file="mrg.rdata")
## 