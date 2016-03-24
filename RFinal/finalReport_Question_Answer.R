library(dplyr)
install.packages("reshape")
library(reshape)
install.packages("jpeg")
library(jpeg)
trainData <- read.csv("trainData.csv")
head(trainData)
## 문제1 #####

mtrushBMP <- readJPEG("finalreport.jpg")
str(mtrushBMP)
class(mtrushBMP)
dim(mtrushBMP)
range(mtrushBMP[1,,1])
max(mtrushBMP)
min(mtrushBMP)

for (x in 1:dim(mtrushBMP)[1]) {
  if ((x %% 100 == 0) || (x %% 101 == 0) || (x %% 102 == 0) || (x %% 103 == 0)) {
    mtrushBMP[x,,1] <- 1
    mtrushBMP[x,,2] <- 1
    mtrushBMP[x,,3] <- 1
  }
}

for (y in 1:dim(mtrushBMP)[2]) {
  if ((y %% 100 == 0) || (y %% 101 == 0) || (y %% 102 == 0) || (y %% 103 == 0)) {
    mtrushBMP[,y,1] <- 1
    mtrushBMP[,y,2] <- 1
    mtrushBMP[,y,3] <- 1
  }
}


writeJPEG(mtrushBMP,target="finalreport_whiteline.jpg")

##################

## 문제2 #####
loyalCustomGroup <- group_by(trainData, touristID)
head(loyalCustomGroup)

loyalCustomSummary <- summarize(loyalCustomGroup, CallCount = length(dateChar))
head(loyalCustomSummary)

loyalCustomer <- arrange(loyalCustomSummary, desc(CallCount))
head(loyalCustomer)
## 상위 5명의 touristID
## tour000125  100 
## tour001313  87
## tour003314  77
## tour002587  74
## tour005763  65

###############


## 문제3 #### 
TopVisitGroup <- group_by(trainData,  nation)
TopVisitSummary <- summarize(TopVisitGroup, visitCount = length(dateChar))

head(TopVisitSummary)

TopVisitNation <- arrange(TopVisitSummary, desc(visitCount))

head(TopVisitNation)

## 가장 많이 방문한 나라는 China
##############

## 문제 4 ####
## 1등국가 - 3등 국가 
## China - Canada 
visitDiff <- TopVisitNation[1,2] - TopVisitNation[3,2]
visitDiff  ## 83403 
##############

## 문제 5 ####
# 날짜 합치기
trainData$Timestamp <- strptime(paste0(trainData$dateChar," ", trainData$timeChar), format="%Y/%m%d %H%M-%S")
#시간 
trainData$hour <- trainData$Timestamp$hour
#요일 
trainData$wday <- trainData$Timestamp$wday
trainData$wday <- factor(trainData$wday, levels=c(1:6,0), ordered=T, labels=c("월","화","수","목","금","토","일"))

trainData$Timestamp <- as.character(trainData$Timestamp)
timeGroup <- group_by(trainData,  hour)
head(as.data.frame(timeGroup))

timeGroupSummary <- summarize(timeGroup, timeCount = length(touristID))

head(timeGroupSummary)

MaxTimeRange <- arrange(timeGroupSummary, desc(timeCount))
head(MaxTimeRange)

## 가장 많이 타는 시간대는 15시 ~ 17시 사이이다.   
##############

## 문제6 ####
MaxDaycountGroup <- group_by(trainData, wday)

head(MaxDaycountGroup)

MaxDaycountSummary <- summarize(MaxDaycountGroup, wdayCount = length(touristID))

MaxDayCount <- arrange(MaxDaycountSummary, desc(wdayCount))
head(MaxDayCount)
## 금요일에 통화건수가 가장 많다 
#############

## 문제7 ####
WeekDayCallCountGroup <- group_by(trainData, touristID, wday)

weekDayCallCountSummary <- summarize(WeekDayCallCountGroup, Count=length(wday))

## touristID 별 wday cast 
weekDayCallCountCast <- cast(weekDayCallCountSummary, touristID ~ wday, value="Count", fun.aggregate=sum)

head(weekDayCallCountCast)

## 주중 통화량
weekDayCallCountCast$weekdayCall <- apply(weekDayCallCountCast[,2:6],1,sum)

## 주말 통화량 
weekDayCallCountCast$weekendCall <- apply(weekDayCallCountCast[,7:8],1,sum)

head(weekDayCallCountCast)
#############

## 문제8 ####
NationRateGroup <- group_by(trainData, city, nation)

NationRateSummary <- summarize(NationRateGroup, nationCnt = length(nation))

NationRateCast <- cast(NationRateSummary, city ~ nation, value="nationCnt", fun.aggregate= sum)

NationRateCast$sum <- apply(NationRateCast[,2:11],1,sum)
NationRateCastPercent <- data.frame(city=NationRateCast$city, NationRateCast[,2:11]/NationRateCast$sum)

## 통화건수 비중
head(NationRateCastPercent)

#############

## 문제 9 ####

# 1. 날짜에 대해서 날짜형식으로 변환 
trainData$dateChar <- strptime(trainData$dateChar, format="%Y/%m%d")

head(trainData)

# 2. 문자열로 변환 후 gsub을 통해 -를 제거
trainData$dateChar <- as.character(trainData$dateChar)
trainData$dateChar <- gsub("-", "", trainData$dateChar)

# 3. 날짜를 계산하기 위해 numeric으로 변환 
trainData$dateChar <- as.numeric(trainData$dateChar)

# 4. touristID 별로 group_by 한 후 max(최근통화일), min(최초통화일), period(차이) 로 summary
lengthOfVisitGroup <- group_by(trainData, touristID)
lengthOfVisitSummary <- summarize(lengthOfVisitGroup, maxRecency=max(dateChar), minRecency=min(dateChar), count = sum(touristID), period = max(dateChar) - min(dateChar))

head(lengthOfVisitSummary)

# 5. 4에서 구한 summary에 최근 통화일, 최초 통화일을 날짜형식으로 출력
lengthOfVisitSummary$maxDate <- as.Date(as.character(lengthOfVisitSummary$maxRecency), format="%Y%m%d")
lengthOfVisitSummary$minDate <- as.Date(as.character(lengthOfVisitSummary$minRecency), format="%Y%m%d")

head(lengthOfVisitSummary)

head((lengthOfVisitSummary$maxDate - lengthOfVisitSummary$minDate)/7)

# 6. 평균체류기간(7일)을 기준으로 방문일수 구하기
lengthOfVisitSummary$lengthOfVisitCnt <- round((lengthOfVisitSummary$maxDate - lengthOfVisitSummary$minDate)/7,0)

head(lengthOfVisitSummary)

library(plyr)
head(trainData)

# 7. merge를 위한 데이터를 summary(touristID, lengthOfVisitCnt 를 기준으로 summary)
LengthOfVisitSum <-summarize(lengthOfVisitSummary, touristID, lengthOfVisitCnt)

# 8 data를 merge한다.
mergeData <- merge(weekDayCallCountCast, LengthOfVisitSum,  by = "touristID")

head(mergeData)


# 9. lengthOfVisitCnt(방문횟수) column의 위치를 바꾸기 위한 작업 시작
## 9-1 lengthOfVisitCnt 의 위치 구하기 
col_idx <- grep("lengthOfVisitCnt", names(mergeData))

## 9-2 위의 9-1에서 구한 위치를 통해 mergeData의 Column위치 변경하기
mergeVisitData <- mergeData[, c(1,col_idx, 3:col_idx-1)]

## 데이터를 어디에 붙여넣을지 몰라서 기존 통화량 계산한 data에 붙여 넣었습니다. 

head(mergeVisitData)
tail(mergeVisitData)

save(list=ls(), file="finaltest_이재현_U2015046.RData") 

# testgroup <- group_by(lengthOfVisitSummary, touristID, lengthOfVisitCnt)
# head(summarize(testgroup, touristID, lengthOfVisitCnt, cnt = length(touristID)))

################