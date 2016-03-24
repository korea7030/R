rm(list=ls())
library(dplyr)
trainData <- read.csv("trainData.csv")

uniqIdAggr <- group_by(trainData, touristID)
uniqIdSumm <- summarise(uniqIdAggr, x=length(dateChar))
head(uniqIdSumm)

uniqIdArrange <- arrange(uniqIdSumm, desc(x))

head(uniqIdArrange)

######################################################
IdNationGroup <- group_by(trainData, touristID, nation)
IdNationSumm <- summarise(IdNationGroup, x= length(city))

IdNationTable <- table(IdNationSumm$nation)

head(IdNationTable)

hist(IdNationTable)
## China
## arrange 로 정렬할 경우, group_by의 기준 컬럼이 한개여야만 정렬이 된다. 
IdNationArrange <- arrange(IdNationSumm, desc(x))

## 정렬하기 위해 order 함수 사용
head(IdNationSumm[order(IdNationSumm$x, decreasing=T),],20)

IdNationSumm <- IdNationSumm[order(IdNationSumm$x, decreasing=T),]

head(IdNationSumm)
?order ## 정렬된 순서를 반환

NationAggr <- group_by(IdNationSumm, nation)
NationSumm <- summarise(NationAggr, Count = sum(x))

head(NationSumm)

NationArrange <- arrange(NationSumm, desc(Count))
head(NationArrange)

## 1등 - 3등 : 95428 - 12025 

## 날짜 합치기
trainData$timestamp <- strptime(paste0(trainData$dateChar,trainData$timeChar), format="%Y/%m%d %H%M-%S")

head(trainData$timestamp)
## 일자
trainData$wday <- trainData$timestamp$wday  # 0 : 일요일 , 6 : 토요일

trainData$wday <- factor(trainData$wday, levels = c(1:6,0), ordered = T, labels = c("월","화","수","목","금","토","일"))
## 시간
trainData$hour <- trainData$timestamp$hour


trainData$timestamp <- as.character(trainData$timestamp)

IdWdayGroupBy <- group_by(trainData, wday)
IdWdaySumm <- summarise(IdWdayGroupBy, x= length(timestamp))

IdWdayArrange <- arrange(IdWdaySumm, desc(x))

## 금요일 

##################################################

timeGroupBy <- group_by(trainData, hour)
timeSumm <- summarise(timeGroupBy, x=length(timestamp))

timeArrange <- arrange(timeSumm, desc(x))

head(timeArrange)

## 15 ~ 17시 사이

library(reshape)

IdWday<- group_by(trainData, touristID, wday)
IdWdaySumm <- summarise(IdWday, x = length(timestamp))
head(IdWdaySumm)

IdWdayCast <- cast(IdWdaySumm, touristID~wday, value="x", fun.aggregate = sum)

head(IdWdayCast)


IdWdayCast$WeekDayCall <- apply(IdWdayCast[,2:6],1,sum)
## 1 row   
## 2 column 
## apply(array or matrix, 1:row / 2:column, FUN)

?apply

IdWdayCast$WeekendCall <- apply(IdWdayCast[,7:8],1, sum)

head(IdWdayCast)

######################################################

cityNationGroup <- group_by(trainData, city, nation)
cityNationSumm <- summarise(cityNationGroup, x = length(timestamp))

head(cityNationSumm)

cityNationCast <- cast(cityNationSumm, city~nation, value="x", sum)

head(cityNationCast)

## apply를 안할 경우 각 나라의 합계가 출력된다.
cityNationCast$sum <- apply(cityNationCast[,2:11], 1, sum)
head(cityNationCast)
cityNationCastPercent <- data.frame(cityNationCast$city, cityNationCast[,2:11] / cityNationCast$sum)

#######################################################

wdayCount <- function(x) {
  count = 0
  for (i in 1:length(x) ) {
    if(x[i] > 0) count=count+1
  }
  return(count)
}

## Error 나는 유형
IdWdayCast$wdayCount <- apply(IdWdayCast[,2:8], 1,wdayCount(x))

## 정확한 유형
IdWdayCast$wdayCount <- apply(IdWdayCast[,2:8], 1,function(x) wdayCount(x))

head(IdWdayCast)
tail(IdWdayCast)
