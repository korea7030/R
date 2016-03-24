rm(list=ls())
library(dplyr)
## data loading 
customerDb <- read.csv("customerDb.csv", stringsAsFactors=F)
head(customerDb)
## 고객아이디, 성별, 나이, 사는도시

basketData <- read.csv("basketData.csv", stringsAsFactors=F)
head(basketData)
## 고객아이디, 주문번호, 지점아이디, 사용일자, 사용시간, 카테고리, 카테고리별분류, 구매개수, 금액

## 1. 가장 소비를 많이하는 고객Id는?
# custId별로 정렬
costcust <- group_by(basketData, custId)
head(costcust)

# amount(금액)의 합계로 symmarize 
loyalCustomer <- summarize(costcust, sumAmount = sum(amount))

# amount(금액)의 합계 내림차순으로 arrange
loyalCustomer <- arrange(loyalCustomer, desc(sumAmount))

head(loyalCustomer)
## C0689 고객이 가장 소비를 많이 한다. 
###################################

## 2. 가장 매출이 많은 branch는?
head(basketData)
# branchId별로 정렬 
branchData <- group_by(basketData, branchId)

# amount(금액)의 합계로 summarize
loyalBranch <- summarize(branchData, sumAmount = sum(amount))
# amount(금액)의 합계 내림차순으로 arrange
loyalBranch <- arrange(loyalBranch, desc(sumAmount))

head(loyalBranch)
## branch_03지점의 매출이 가장 많다.
################################

## RFM분석
## Recency- 거래의 최근성: 고객이 얼마나 최근에 구입했는가?
## Frequency- 거래빈도: 고객이 얼마나 빈번하게 우리 상품을 구입했나?
## Monetary- 거래규모: 고객이 구입했던 총 금액은 어느 정도인가?.

basketData$date <- as.Date(as.character(basketData$date), format="%Y%m%d")
## 크기를 비교하기위해 다시 숫자로
basketData$date <- as.numeric(basketData$date)
# basketData와 customerDb left_join 
# left_join(조인당할 대상, 조인할 대상): left_join이니 왼쪽으로 join 시킨다라는 의미
basketCustom <- left_join(basketData, customerDb)
head(basketCustom)
# basketCustom을 custId와 date로 group_by 
groupbyDummy <- group_by(basketCustom, custId, date)
head(groupbyDummy)

## 날짜별 고객 사용빈도 현황
userFreq <- summarize(groupbyDummy, N=n())
head(userFreq)

## 고객별 마트이용횟수
userF <- summarize(userFreq, frequency=n())
userF

groupbyDummy <- group_by(basketCustom, custId)
head(groupbyDummy)

## 마트사용량을 기간별로 나타내라 . 
userRFM <- summarize(groupbyDummy, 
                     minRecency=min(date), # 최초방문일? 
                     recency=max(date), # 최근방문일
                     momentary=sum(amount),  # 거래규모
                     period=max(date)-min(date)) # 기간(최초방문일 - 최근방문일)

userRFM
# 기간별 마트사용량과 고객별 마트이용횟수 left_join
userRFM <- left_join(userRFM, userF)

head(userRFM)

## 마트사용금액합계별로 내림차순
arrange(userRFM, desc(momentary))

## 최초방문일, 최근방문일을 date형으로 나타내기 위해 추가
userRFM$minDate <- as.Date(userRFM$minRecency, origin="1970-01-01")
userRFM$maxDate <- as.Date(userRFM$recency, origin="1970-01-01")

## 히스토그램
hist(userRFM$maxDate, breaks=10)

str(userRFM)
## userRFM중에 maxDate가 중복이 아닌 전체길이
length(unique(userRFM$maxDate))
## 거래규모 히스토그램 
hist(userRFM$momentary, breaks=100)
range(userRFM$momentary)

## 마트를 가장많이 사용하는 날짜 
plot(table(userRFM$maxDate), main="Guests Recency")

## 마트를 사용하는 손님 빈도수 
plot(table(userRFM$frequency), main="Guests Frequency")

# 거래규모에 대한  사분위수를 따로 구한다. 
quantile(userRFM$momentary, c(0.2, 0.4, 0.6, 0.8))

# 최근방문일을 분위수로 계산 
quantile(as.numeric(userRFM$maxDate), c(0.2,0.4,0.6,0.8,0.9))

# RFM별로 상위 20%가 차지하는 총 매출액 대비 비중을 구한다. 
## userRFM$momentary > quantile(userRFM$momentary, 0.8) : 거래규모가 0.8 보다 큰 즉, 상위 20%를 차지하는 비율
## 에 대한 momentary의 합계를 sumM으로 구해라 
sumM <- sum(userRFM$momentary[userRFM$momentary > quantile(userRFM$momentary, 0.8)])

# 위의 구한 비중에 대한 전체거래규모 비율(거래규모 상위 20%의 합계 / 전체 거래규모합계)
sumM/sum(userRFM$momentary) ## 약 65%로 가장크다 

## 빈도수 
sumF <- sum(userRFM$frequency[userRFM$frequency > quantile(userRFM$frequency, 0.8)])
## 전체빈도수에서 위의 빈도수에 대한 비율 
sumF/sum(userRFM$frequency) ## 59%로 중간수준이다. 

as.Date(quantile(userRFM$recency,0.8), origin="1970-01-01")
head(as.numeric(userRFM$maxDate))
str(userRFM)

## 최근방문일의 20%에 대한 거래규모 합계
sumR <- sum(userRFM$momentary[userRFM$recency > quantile(userRFM$recency, 0.8)])
sumR/sum(userRFM$momentary) # 43%로 가장 적다.

## R, F , M 별로 비율구하기
weightR <- sumR/(sumR+sumF+sumM)
weightF <- sumF/(sumR+sumF+sumM)
weightM <- sumM/(sumR+sumF+sumM)

## RFM 지수 = weightR * Recency 점수 + weightF * Frequency점수 + weightM * Monetary 점수
quantM <- quantile(userRFM$momentary, c(0,0.2,0.4,0.6, 0.8, 1))
quantR <- as.Date(quantile(as.numeric(userRFM$maxDate), c(0,0.2,0.4,0.6,0.8,1)), origin="1970-01-01")
quantF <- quantile(userRFM$frequency, c(0,0.2,0.4,0.6,0.8,1))

?parse
ColumnName <- paste0("userRFM","$","frequency")
# 문자열조합으로 데이터프레임의 열을 찾는 방법
eval(parse(text=ColumnName))[2] 
## userRFM$frequency[2] 와 같다. 

head(userRFM$frequency)
# userRFM의 maxDate, frequency, momentary 별 quantR, quantF, quantM 을 활용해 R,F,M 점수를 지정하는 함수

# 등급을 숫자로 나타내기
# parameter : 등급을 나타낼 데이터, 데이터명, RFM명, 분위수데이터
intervalGrade <- function(mainData, fileName, rfmName, quantileData) {
  forLength <- dim(mainData)[1]
  
  results <- rep(0, forLength) # 0을 mainData의 길이만큼 0으로 미리 채워둠
  
  for(i in 1:forLength) {
    data <- eval(parse(text=paste0(fileName,"$",rfmName)))[i]
    
    if(data >= quantileData[1] && data < quantileData[2]) {
      results[i] <- 1
    }else if(data >= quantileData[2] && data < quantileData[3]) {
      results[i] <-2
    }else if(data >= quantileData[3] && data < quantileData[4]) {
      results[i] <-3
    }else if(data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- 4
    }else { results[i] <- 5}
  }
  
  return(results)
  print(results)
}

userRFM$R <- intervalGrade(userRFM, "userRFM", "maxDate", quantR )
userRFM$F <- intervalGrade(userRFM, "userRFM", "frequency", quantF )
userRFM$M <- intervalGrade(userRFM, "userRFM", "momentary", quantM )

head(userRFM)

userRFM$score <- (weightR * userRFM$R + weightF*userRFM$F + weightM*userRFM$M) * 100/5
hist(userRFM$score)

# 스코어에 대한 분위수
(quantS <- quantile(userRFM$score,c(0,0.2,0.4,0.6,0.8,1)))

# userRFM에 대한 score점수를 quantS를 통해 점수등급을 구하는 함수
# parameter : 등급구할 데이터, 데이터파일명, RFM명, 분위수데이터
finalGrade <- function(mainData, fileName, rfmName, quantileData) {
  
  forLength <- dim(mainData)[1]
  
  results <- rep(0, forLength)
  
  
  for (i in 1:forLength) {
    
    data <- eval(parse(text=paste0(fileName,"$",rfmName)))[i]
    
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- "E"
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- "D"
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- "C"
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- "B"
    } else { results[i] <- "A" }
  }
  
  return(results)
}

userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS)
head(as.data.frame(userRFM))

str(userRFM)

userRFM <- arrange(userRFM, desc(grade))

head(userRFM)

############### Chain rule 관련 연습. 
head(basketData)
unique(basketData$branchId)
filterDummy <- filter(basketData, branchId == "branch_01")
groupbyDummy <- group_by(filterDummy, custId, date)
summarizeDummy <- summarize(groupbyDummy, frequency=n(), monetary=sum(amount))
summarizeDummy
frequencyDummy <- summarize(summarizeDummy, frequency=n())
frequencyDummy

basketData %>% 
  filter(branchId == "branch_01") %>%
  group_by(custId, date) %>%
  summarize(frequency=n(), monetary=sum(amount)) %>%
  summarize(frequency=n())

frequencyDummy <- basketData %>% 
  filter(branchId == "branch_02") %>%
  group_by(custId, date) %>%
  summarize(frequency=n(), monetary=sum(amount)) %>%
  summarize(frequency=n())
frequencyDummy
############################################################

############# ploting 
library(reshape)
library(dplyr)
head(basketData)
names(basketData)
head(names(basketData[,-c(1)])

tblgrade <- table(userRFM$grade)
plot(tblgrade)
plot(tblgrade, type="l")
title(main="My first plot image")

plot(tblgrade, type="l", xaxt="n", ylab="")
title(main="My second plot image")

axis(1, at=1:length(tblgrade), label=c("최우수고객","우수고객","보통고객","평균고객","불만고객"))
title(xlab="고객구분", col.lab="red")
title(ylab="고객수",  col.lab="blue")

userRFM$gradeFac <- factor(userRFM$grade, levels=c("A","B","C","D","E"), ordered=T)
boxplot(userRFM$score~userRFM$gradeFac)
boxplot(userRFM$score~userRFM$grade, notch=T, col=c("red","blue", rep("black",3)))

userRFM <- data.frame(userRFM)

## basketData에 userRFM의 custid, grade를 leftjoin 
basketGrade <- left_join(basketData, userRFM[,c("custId","grade")])
head(basketGrade)

graphOut <- basketGrade %>% 
            group_by(grade, custId, branchId) %>%
            summarize(avgPurchase=mean(amount))

dim(graphOut) # 2097 * 4

## 등급별 평균금액
boxplot(graphOut$avgPurchase~graphOut$grade, notch=T)

head(basketGrade,1)



## 등급별 branch현황 
graphOut2 <- basketGrade %>%
  group_by(grade, custId, branchId) %>%
  summarize(avgPurchase=mean(amount)) %>%
  group_by(grade, branchId) %>%
  summarize(N=n())

graphDummy <- data.frame(graphOut2)
head(graphDummy)

## graphDummy에서 grade를 행으로, branchId를 열로 Number의 sum값을 cast하라.
graphCast <- cast(graphDummy, grade ~ branchId, value="N", fun.aggregate=sum)
head(graphCast)

# 각 x값 별로 비교를 하기위한 plot 
matplot(graphCast)
matplot(graphCast, type="o")
# pch : 점모양 
matplot(graphCast, type="o", pch=16)
matplot(graphCast, type="o", pch=c(16,20,21,22), col=c("blue", "yellow", "magenta", "red"))

##범례 추가
legend("topright", 95, c("branchId_1", "branchId_2", "branchId_3", "branchId_4"), 
       lty=1:2,
       cex=0.75,
       lwd=c(1,1,1,1),
#        ncol=1,
       col=c("blue","yellow", "magenta","red"))

# ylim : y축 값 범위 
# 수업시간에 뭐 그래프가 안나오니 어쩌니 그런건... 
# ylim 범위를 30,3000으로 지정했기 때문이라는...
matplot(matrix(1:12, 4), type="c", lty=1, lwd=10, lend=lends) 


matplot(graphCast, type="o", pch=c(16,20,21,22), col=c("blue", "yellow","magenta","red"), ylim=c(1,50000))

?matplot

legend("topright",
       c("branchId_1","branchId_2","branchId_3","branchId_4"),
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       cex=1,
       lwd=c(5, 5, 5,5),
       col=c("blue", "yellow","magenta","red"))


text(x=matp, y=graphCast*1.05, labels=paste(graphCast,"건"))

graphmat <- as.matrix(graphCast)
head(graphmat)

example(matplot)
?matplot
pdfpngplot(graphmat, "pdfpngplot_01", "Customers rating points Status", "total number of customers")
?par

## pdf 와 png 그래프를 만들기 위한 함수 
## parameter : 그래프그를 data, 파일명, 그래프제목, y축 label 제목
pdfpngplot <- function(data, filename, maintitle, labtitle) {
  # pdf 그래프 출력 샘플 -------
  # 15 vs 11.25, 30 vs 22.5
  
  pdf(paste0(filename,".pdf"), width=10, height=7)
  
  par(mar=c(5,7,10,1))
  
  matplot(data, type="l", xaxt="n", yaxt="n",
          col=c("blue", "yellow","magenta","green","red"),
          lwd=c(3,3,3,3),ylab="")
  title(main=maintitle, 
        col.main="gray", cex.main=3.2)
  title(ylab=labtitle, 
        col.lab="black", cex.lab=3)
  
  axis(1,at=1:dim(data)[1],labels=row.names(data), las=2, cex=0.5)
  axis(2, cex.axis=1.5)
  
  legend("topright",
         c("branchId_1","branchId_2","branchId_3","branchId_4"),
         lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
         cex=1,
         lwd=c(5, 5, 5,5),
         col=c("blue", "yellow","magenta","red"))
  dev.off()
  
  # png 그래프 출력 샘플 -------
  png(paste0(filename,".png"), width=10, height=7, units = "in", res=400)
  
  par(mar=c(5,7,10,1))
  
  matplot(data, type="l", xaxt="n", yaxt="n",
          col=c("blue", "yellow","magenta","red"),
          lwd=c(3,3,3,3), ylab="")
  
  title(main=maintitle, 
        col.main="gray", cex.main=3.2)
  
  title(ylab=labtitle, 
        col.lab="black", cex.lab=3)
  
  axis(1,at=1:dim(data)[1],labels=row.names(data), las=2, cex=0.5)
  axis(2, cex.axis=1.5)
  
  legend("topright",
         c("branchId_1","branchId_2","branchId_3","branchId_4"),
         lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
         cex=1,
         lwd=c(5, 5, 5,5),
         col=c("blue", "yellow","magenta","red"))
  dev.off()
}



#blank plot으로 시작하는 방법 : 참고하라는 용인듯...
plot(1, type="n", axes=F, xlab="", ylab="")
plot(3, type="n", axes=T, xlab="", ylab="")
matplot(graphCast, type="n")

