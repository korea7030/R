getwd()
setwd("C:/Users/leejh/Documents/20150314_R")
getwd()

##### 20150314 수업진행 #####

# 예약어 확인
?reserved

/* 
## 예약어 목록
## if else repeat while function for in next break
## TRUE FALSE NULL Inf NaN NA NA_integer_ NA_real_ NA_complex_ NA_character_
*/

### 연산자 - 연습1
5+3 ## 8 
10000000 - 99 ## 999901
10000000 * 10000000 ## 1e+14 
3^2 ## 9
3**3 ## 27(세제곱??)
3%%2 ## 1 (나머지)
4%%2 ## 0 
7777777 %/% 3 ## 1(몫)
7777777 %% 3 ## 1(나머지)

### 논리연산자
5<7 ##TRUE
5<5 ## FALSE
5<= 5 ## TRUE
5 == 5.0 ## TRUE
5L == 5.0 ## TRUE(5L : 정수형 5)
5!= 5 ## FALSE
!TRUE ## FALSE
!FALSE ## TRUE
2>3 | 5>4 ## TRUE
2>3 & 5>4 ## FALSE
isTRUE(2>3 & 5>4) ## FALSE

### 연산자 - 연습2
5^1/5 ## 1 
5^(1/5) ## 1.37973
TRUE+TRUE ## 2
TRUE+FALSE ## 1
5+3%%2 ## 6
(5+3)%%2 ## 0 
!TRUE ## FALSE
!!TRUE ## TRUE

### 기초 내장함수
x <- -1
abs(x)  ## 절대값
sqrt(16) ## 4 제곱근
ceiling(1.2) ## 올림 2
floor(1.2) ## 내림 1 
exp(2)  ## 지수함수 값(e의 2승)
round(1.333, 2) ## 반올림 1.33
log(3)  ##상용로그 
log10(3)
nchar("xxxx") ## 4 문자열글자수(number of char)
substr("XXXXX", 1,3) ## 문자열 일부 선택, 시작은 1로 시작
paste("x","y", sep="") ## 단어 붙이기
is.na("x") ## NA 여부 확인

## 기초 내장함수 -연습 
abs(-4) + sqrt(100) ## 14 
10^2; ## 100  
10^2/5; ## 20
10^(2/5) ## 2.511886
round(2.3456789, digit= 2) ## 2.35
round(2.3456789, dig = 5) ## 2.34568 
exp(round(2.3456789, 2)) ## 
exp(round(2.3456789, 5)) ## 
nchar("uriappa") ## 7
mode(nchar("urippa")) ## 해당 내용의 type형식
father <- "uriappa"; mother <-"uriumma"
substr(father, 1,2) ## ur 
paste(father, mother) ## uriappa uriumma sep="" 안하면 한칸 띄워짐
paste(father, mother, sep= "   ") ## 간격 지정한 후 붙임
paste(father, mother, sep="") ## uriappauriumma
paste0(father, mother) ## uriappauriumma  sep="" 와 같은 말임

## 수식및 내장함수 코딩 연습 
Prob1 <- exp(3) / (1+exp(3)) 
Prob2 <- exp(9^(1/5))
Prob3 <- Prob2 / (1+exp(9^(1/5)))

### Vector 
# 같은 자료형의 성분을 일렬로 저장한 객체
# 숫자 + 문자가 섞이면 문자형으로 바꿔준다. 

## Vector 생성 
x1 <- c(3,6,9) ## Combine 생성
x2 <- 1:7 ## 1,2,3,4,5,6,7 
x3 <- c(1,"two"); mode(x3); ## 강제 형변환
x4 <- scan(); ## scan 함수
1 2 3 4 5 6 7
string.Data <- scan(what=character())  ## character로 된 데이터를 scan
rucy buttler ## read items 2 

string.Data ## "rucy"    "buttler"
(x<- seq(-5, 5, 0.1)) ## sequence(시작값, 끝값, 간격) 출력시 () 묶음
?seq
example(seq) ## seq 예 

prob <- exp(x) / (1+exp(x))

plot(x, prob)  ## 기본 plot 차트

plot(x, prob, type="l") ## 선형(line) plot 차트

x<- seq(from=-5, by=0.1, to=5)
sigmoid <- exp(x) / (1+exp(x)) 
sigmoid

plot(x, sigmoid)
## example(plot)
example(seq)

x<- 1:4 ## 1 2 3 4
y <- 5:8 ## 5 6 7 8
x/y  ## 1/5 2/6 3/7 4/8 

x<- 1:4 ## x[1] = 1 x[2] = 2 ...
y<- 5:7
x*y ## 5 12 21 20 에러는 나지만 적은 자리의 vector값의 일부가 반복되어 진다.

### Vector indexing 및 filtering 
idxData <- seq(from =-20, to =20, by=3)
idxData
length(idxData) ## Vector의 길이 
idxData[3]
idxData[c(1,5:7)]  ## 1부터 시작해서 5부터 7까지의 위치는 빼고 출력
idxData <- idxData[c(1:5,7:length(idxData))]
idxData
which(idxData<0) ## 데이터 중에 0보다 작은 위치(번호)는?
which(idxData %% 2 == 1) ## 나머지가 1이 아닌거
idxData[which(idxData %% 2 != 1)]  ## 짝수인 것만 구하기 

genData <- seq(-200 , 150, 3)
length(genData)

oddGenData <- genData[genData %% 2 == 1] ## 홀수 
length(oddGenData)
sum(oddGenData)
evenGetData <- genData[which(genData %% 2 == 0)]
sum(evenGetData)
chrGenData <- as.character(genData)
mode(chrGenData)
nchar3genData <- chrGenData[nchar(chrGenData) == 3]
length(nchar3genData)
sum(nchar(nchar3genData))
