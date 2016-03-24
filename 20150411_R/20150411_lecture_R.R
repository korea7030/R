getwd()
setwd("C://Users//leejh//Documents//R//20150411_R")
getwd()
###########20150411_강의내용############
## 흐름제어 - 조건문, 반복문
## if ( 조건식 ) 
##{ 조건식이 참인 경우 실행} 
##else if 
##(조건식2) { 조건식이 거짓이고, 조건식2가 참인 경우 수행}
## else { 조건식, 조건식2가 둘다 거짓인 경우 나머지 수행}

if (TRUE) {
  print("참입니다.")
}else {
  print("거짓입니다.")
}

## for (변수 in Vector) {  Vector뿐 아니라 index번호로도 가능하다
## break : 반복문 중 반복하기 싫을 경우 적어줌, for문을 벗어나게 된다.
## next 현재 수행중인 반복문은 건너띄고 다음 반복문을 수행할 경우, for문을 벗어나지 않고 끝까지 수행
## }

## Vector 생성
myVector <- c("multiple Comparison", "t-test", "Tukey's HSD", "bonferroni correction")
## 해당 Vector가 4보다 크면 참, 아니면 거짓
if (length(myVector) > 4) {  ## Vector 길이가 4이지만 > 이므로 4보다 무조건 큰 것만 참이 된다.
  print("참입니다")
} else {
  print("거짓입니다.")
}

myVector <- c("multiple Comparison", "t-test", "Tukey's HSD", "bonferroni correction")
for (i in myVector) {
  print(i)
}

myMatrix <- matrix(0, ncol=4, nrow=5)
myMatrix

## 해당 matrix의 row의 모든 값을 1,2,3,4,5 로 바꿔라
for (row in 1:5) {
  myMatrix[row,] <- row
}

## 해당 matrix의 column에 해당되는 모든 값을 1,2,3,4 로 바꿔
for (column in 1:4) {
  myMatrix[,column] <- column 
}

myMatrix

## 이중 for문 
## 바깥 for문 먼저 수행 즉, column이 1일 경우
## 안의 for 문에서 row값이 1~5까지 수행 후 바깥 for문을 또 수행 한다. 
## 즉 이 for문의 경우는 column * row = 20 번을 수행하게 된다. 
## matrix는 for문 반복횟수와 matrix 길이가 맞지 않으면 for문에서 에러 
## dataframe의 경우는 for문의 반복횟수와 dataframe의 길이가 같지 않아도 에러 안남
## 수행속도 면에 있어서 matrix가 dataframe보다 빠르다. 

for(column in 1:4) {
  for(row in 1:5) {
    myMatrix[row,column] <- row*column
  }
}

myMatrix

## matrix
row <- 1000
column <- 1000
myMatrix <- matrix(nrow=row, ncol=column)
myMatrix

## 프로그램에서 해당 명령어가 수행되는 시간예측
system.time(
  for(column in 1:column) {
    for(row in 1:row) {
      myMatrix[row,column] <- row*column
    }
  }
)


## dataframe 
## 동적으로 row와 column을 생성하기 때문에 그만큼 유연하지만 오래 걸린다.
row <- 1000
column <- 1000
myDataFrame <- data.frame()  ## matrix는 row와 colum을 지정하지만 dataframe은 아니다.

## 프로그램에서 해당 명령어가 수행되는 시간예측
system.time(
  for(column in 1:column) {
    for(row in 1:row) {
      myDataFrame[row,column] <- row*column
    }
  }
)

install.packages("raster", dependencies=T)  

library(raster)
dir()

mtrushRGB <- brick("image_sample.jpg")
mtrushRGBvalue <- getValues(mtrushRGB)
dim(mtrushRGB)
str(mtrushRGB)
class(mtrushRGB)
max(mtrushRGB)
min(mtrushRGB)

mtrushTweak <- mtrushRGB
head(mtrushTweak[100:200, 100:200][,])

plotRGB(mtrushTweak)


mtrushTweak[,50][,] = 255
nrow(mtrushTweak)

## nrow 함수와 ncol 함수로 할 경우
## nrow의 경우 해당 matrix의 row 개수를 의미
## ncol의 경우 해당 matrix의 col 개수를 의미

nrow(mtrushTweak)  ## 1080
ncol(mtrushTweak)  ## 1440

## 가로 하얀줄 for문 돌린다. 
## 여기서 i 변수의 경우는 이 for문 안에서 수행할 때 index를 나타낸다. 
## 즉 이말은 mtrushTweak의 index접근을 i 값으로 접근하겠다 라고 생각
for (i in 1:nrow(mtrushTweak)) {
  if (i %% 50 == 0) {   ## 50, 100 , 150 순으로 하얀색을 뿌리기 위한 작업
    mtrushTweak[i,][,1] = 255;
    mtrushTweak[i,][,2] = 255;
    mtrushTweak[i,][,3] = 255;
  }
}

plotRGB(mtrushTweak)

## 세로 하얀줄 for문 돌린다. 
## 여기서 j 변수의 경우는 이 for문 안에서 수행할 때 index를 나타낸다. 
## 즉 이말은 mtrushTweak의 index접근을 j 값으로 접근하겠다 라고 생각
for (j in 1:ncol(mtrushTweak)) {
  if (j %% 50 == 0) {  ## 50, 100 , 150 순으로 하얀색을 뿌리기 위한 작업
    mtrushTweak[,j][,1] = 255;
    mtrushTweak[,j][,2] = 255;
    mtrushTweak[,j][,3] = 255;
  }
}

plotRGB(mtrushTweak)

## dim으로 길이 지정을 해서 for문을 돌릴 경우
for (i in 1:dim(mtrushTweak)[1]) {
  if (i %% 50 == 0) {
    mtrushTweak[i,][,1] = 255;
    mtrushTweak[i,][,2] = 255;
    mtrushTweak[i,][,3] = 255;
  }
}

## dim(mtrushTweak)[2]

for(j in 1:dim(mtrushTweak)[2]) {
  if (j %% 50 == 0) {
    mtrushTweak[,j][,1] = 255;
    mtrushTweak[,j][,2] = 255;
    mtrushTweak[,j][,3] = 255;
  }
}

plotRGB(mtrushTweak)


##################3 트위터 연동##########################

install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")

library(twitteR)
library(ROAuth)
library(RCurl)

credential <- OAuthFactory$new(consumerKey = "1oOphkaTQunRAwXVE5TtGCrCz", consumerSecret = "QpSqEGGwlqfGKaoP3uX7XpMmmLAgwldBmBQ1ap3hJen4FLTWIx", requestURL = "https://api.twitter.com/oauth/request_token", accessURL="https://api.twitter.com/oauth/access_token", authURL = "https://api.twitter.com/oauth/authorize")
credential

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
credential$handshake(cainfo="cacert.pem")


setup_twitter_oauth(credential$consumerKey,credential$consumerSecret, credential$oauthKey,credential$oauthSecret)

searchWord <- iconv("#박지성","CP949","UTF-8")
searchWord
tweetList <- searchTwitter(searchWord, n=500)
write(searchTwitter(searchWord, n=500), "tweetList.RData" )
load("tweetList.RData")
head(tweetList)

tail(tweetList)

length(tweetList)
str(head(tweetList,1))

tweetList[[1]]

tweetDf <- do.call("rbind", lapply(tweetList, as.data.frame))

dim(tweetDf)

names(tweetDf)

str(tweetDf)

##리스트 데이터를 데이터프레임으로 합칠 필요가 있는 경우,
##do.call함수를 사용하지 않으면 에러가 납니다. 그 차이를 비교해 보실수 있습니다.
tweetDfErr <- rbind(lapply(tweetList, as.data.frame))
tweetDfErr
