
library(dplyr)

sub2
merge_data2 <- merge(demo2, sub2, by="ID")
head(merge_data2)

head(merge_data2)

strptime(as.numeric(merge_data2$구매일자),
         format="%Y%m%d")
merge_data2$구매일자 <- as.Date(as.character(merge_data2$구매일자), format="%Y%m%d")
merge_data2$구매일자 <- as.numeric(merge_data2$구매일자)

head(merge_data2)

groupbyDummy <- group_by(merge_data2, ID, 구매일자)
head(groupbyDummy)
conflicts()

## 날짜별 사용자 현황
userFreq <- summarize(groupbyDummy, N=n())
userFreq

## 고객별 마트이용횟수
userF <- summarize(userFreq,frequency=n())
userF

groupbyDummy <- group_by(merge_data2, ID)  
head(groupbyDummy)

## 마트사용량을 date기간별로 나타내라.###################3
userRFM <- summarize(groupbyDummy, minRecency=min(구매일자),
                     recency=max(구매일자),
                     monetary=sum(구매금액),
                     period=max(구매일자)-min(구매일자))
#########################################################

## 고객 이용횟수와 RFM data frame을 join 
userRFM
userRFM <- left_join(userRFM, userF)

###############################################################

# head(userRFM)
# arrange(userRFM, desc(monetary))

userRFM$minDate <- as.Date(userRFM$minRecency, origin="1970-01-01")
userRFM$maxDate <- as.Date(userRFM$recency, origin="1970-01-01")

hist(userRFM$maxDate, breaks=10)

## 마트를 가장많이 사용하는 날짜
plot(table(userRFM$maxDate), main="Guests Recency")


## 마트를 사용하는 손님 빈도수
plot(table(userRFM$frequency), main="Guests Frequency")

quantile(userRFM$monetary, c(0.2,0.4,0.6,0.8))
#   20%     40%     60%     80% 
#   290000  741400 1531000 3066400
quantile(as.numeric(userRFM$maxDate), c(0.2,0.4,0.6,0.8,0.9)) # 날짜를 numeric으로 바꾸면 분위수 계산이 가능해 진다.
# 20%   40%   60%   80%   90% 
# 16359 16400 16418 16429 16433 

# RFM별로 상위 20%가 차지하는 총 매출액 대비 비중을 구한다. ################################
## M의 비중
sumM <- sum(as.numeric(userRFM$monetary[userRFM$monetary > quantile(userRFM$monetary, 0.8)]))
sumM/sum(as.numeric(userRFM$monetary)) # 64%로 가장 크다.

## F의 비중
sumF <- sum(as.numeric(userRFM$monetary[userRFM$frequency > quantile(userRFM$frequency, 0.8)]))
sumF/sum(as.numeric(userRFM$monetary)) # 50%로 중간 수준이다.

## R의 비중
sumR <- sum(as.numeric(userRFM$monetary[userRFM$recency > quantile(userRFM$recency, 0.8)]))
sumR/sum(as.numeric(userRFM$monetary)) # 33%로 가장 적다.
##############################################################################################

## 가중치 변수 생성
weightR <- sumR/(sumR + sumF + sumM)
weightF <- sumF/(sumR + sumF + sumM)
weightM <- sumM/(sumR + sumF + sumM)

# RFM지수 = weight$ * Recency점수 + weightF*Frequency점수 + weightM+Monetary점수
quantM <- quantile(userRFM$monetary,c(0,0.2,0.4,0.6,0.8,1))
length(quantM)
# Recency 사분위지점
quantR <- as.Date(quantile(as.numeric(userRFM$maxDate),c(0,0.2,0.4,0.6,0.8,1)),origin="1970-01-01")
quantR
# Frequency 사분위지점
quantF <- quantile(userRFM$frequency,c(0,0.2,0.4,0.6,0.8,1))
quantF

## 가중치 값 구하기 함수
## R,F,M 의 가중치 값을 구하기 위해 5분위수(quantM, quantR, quantF)를 활용하여 가중치 구함
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

## 위에서 구한 R,F,M 각각의 변수와 가중치값을 계산하여 RFM점수로 환산
userRFM$score <- (weightR * userRFM$R + weightF * userRFM$F + weightM * userRFM$M)*100/5

hist(userRFM$score)
(quantS <- quantile(userRFM$score,c(0,0.2,0.4,0.6,0.8,1)))

## 등급 매기는 함수
## R, F , M 각 등급의 5분위지점(0, 0.2, 0.4, 0.6 , 0.8 ,1)에 대해 등급을 매김
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

## 등급 변수 추가(A,B,C,D,E)
userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS )

str(userRFM)
# head(as.data.frame(userRFM))
save(userRFM, file="RFM_data.RData")

## 원본 데이터와 join
userRFM.with.data <- left_join(userRFM, merge_data2)
head(userRFM)

write.csv(userRFM.with.data, file="lpoint_주제2_RFM.csv")
