### data loading -----
customerDb <- read.csv("customerDb.csv", stringsAsFactors=F)
head(customerDb)
basketData <- read.csv("basketData.csv", stringsAsFactors=F)
head(basketData)
str(basketData)

### 기본 EDA--------------

#1. 가장 소비를 많이하는 고객Id는?

#2. 가장 매출이 많은 branch는?


## RFM 분석--------

# RFM 분석이 무엇인지는 알고 계신가요?


head(basketData)
head(customerDb)
basketData$date <- as.Date(as.character(basketData$date), format="%Y%m%d")
basketData$date <- as.numeric(basketData$date)


##RFM의 의미 이해 필요
library(dplyr)

groupbyDummy <- group_by(basketData, custId, date)
head(groupbyDummy)

userFreq <- summarize(groupbyDummy, N=n())
userFreq
userF <- summarize(userFreq,frequency=n())
userF

groupbyDummy <- group_by(basketData, custId)  
head(groupbyDummy)

userRFM <- summarize(groupbyDummy, minRecency=min(date),
                             recency=max(date),
                             monetary=sum(amount),
                             period=max(date)-min(date))
userRFM
userRFM <- left_join(userRFM, userF)



head(userRFM);tail(userRFM)

arrange(userRFM, desc(monetary))

## Chain rule 연습------
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


##RFM연습 다시 시작-------
userRFM$minDate <- as.Date(userRFM$minRecency, origin="1970-01-01")
userRFM$maxDate <- as.Date(userRFM$recency, origin="1970-01-01")


hist(userRFM$maxDate, breaks=10)

str(userRFM)
length(unique(userRFM$maxDate))
hist(userRFM$monetary, breaks=100)
range(userRFM$monetary)

plot(table(userRFM$maxDate), main="Guests Recency")

plot(table(userRFM$frequency), main="Guests Frequency")


# 분위수를 따로 구한다.
quantile(userRFM$monetary, c(0.2,0.4,0.6,0.8))

quantile(as.numeric(userRFM$maxDate), c(0.2,0.4,0.6,0.8,0.9)) # 날짜를 numeric으로 바꾸면 분위수 계산이 가능해 진다.

# RFM별로 상위 20%가 차지하는 총 매출액 대비 비중을 구한다.
sumM <- sum(userRFM$monetary[userRFM$monetary > quantile(userRFM$monetary, 0.8)])
sumM/sum(userRFM$monetary) # 65%로 가장 크다.
sumF <- sum(userRFM$monetary[userRFM$frequency > quantile(userRFM$frequency, 0.8)])
sumF/sum(userRFM$monetary) # 58%로 중간 수준이다.

as.Date(quantile(userRFM$recency,0.8), origin="1970-01-01")
head(as.numeric(userRFM$maxDate))
str(userRFM)

sumR <- sum(userRFM$monetary[userRFM$recency > quantile(userRFM$recency, 0.8)])
sumR/sum(userRFM$monetary) # 43%로 가장 적다.

weightR <- sumR/(sumR + sumF + sumM)
weightF <- sumF/(sumR + sumF + sumM)
weightM <- sumM/(sumR + sumF + sumM)

# RFM지수 = weightR * Recency 점수 + weightF * Frequency점수 + weightM * Monetary 점수
quantM <- quantile(userRFM$monetary,c(0,0.2,0.4,0.6,0.8,1))
length(quantM)
quantM
quantR <- as.Date(quantile(as.numeric(userRFM$maxDate),c(0,0.2,0.4,0.6,0.8,1)),origin="1970-01-01")
quantR
quantF <- quantile(userRFM$frequency,c(0,0.2,0.4,0.6,0.8,1))
quantF



# parse 함수 활용방법
columnName <- paste0("userRFM","$","frequency")
eval(parse(text=columnName))[2] # 문자열 조합으로 데이터프레임의 열을 찾는 방법
# ?parse


head(userRFM$frequency)


intervalGrade <- function(mainData, fileName, rfmName, quantileData) {
  
  forLength <- dim(mainData)[1]
  
  results <- rep(0, forLength)
  
  
  for (i in 1:forLength) {
    
    data <- eval(parse(text=paste0(fileName,"$",rfmName)))[i]
    
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- 1
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- 2
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- 3
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- 4
    } else { results[i] <- 5 }
  }
  
  return(results)
}


userRFM$R <- intervalGrade(userRFM, "userRFM", "maxDate", quantR )
userRFM$F <- intervalGrade(userRFM, "userRFM", "frequency", quantF )
userRFM$M <- intervalGrade(userRFM, "userRFM", "monetary", quantM )

head(userRFM)

userRFM$score <- (weightR * userRFM$R + weightF * userRFM$F + weightM * userRFM$M)*100/5

hist(userRFM$score)


dim(userRFM)

(quantS <- quantile(userRFM$score,c(0,0.2,0.4,0.6,0.8,1)))

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

userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS )
head(userRFM)
str(userRFM)

