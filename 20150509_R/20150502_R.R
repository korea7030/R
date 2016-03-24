trainData <- read.csv("data/trainData.csv", stringsAsFactors = F, na.strings="NA")

head(trainData,20)
## 2. 통화량이 많은 관광객 상위 5명 ##
Top5touristID_List <- aggregate(trainData$dateChar , by=list(trainData$touristID), FUN=length)
head(Top5touristID_List)

Top5Order <- order(Top5touristID_List$x, decreasing=TRUE)
head(Top5touristID_List[Top5Order,],5)
## tour000125 , tour001313 tour003314 , tour002587, tour005763 
######################################

## 3. 우리나라를 가장 많이 방문한 나라의 이름은? 1등방문객수 국가과 2등 방문객수 국가의 방문객수 차이는? 
touristCount_List <- aggregate(trainData$dateChar, by=list(tourist=trainData$touristID, nation=trainData$nation), FUN=length)
head(touristCount_List)
nationCount <- table(touristCount_List$nation)
nationCount 
## China 

tourOrder <- order(touristCount_List$x, decreasing=TRUE)
head(touristCount_List[,,tourOrder],20)

diffCount <- aggregate(touristCount_List$x, by=list(touristCount_List$nation), FUN=sum)
head(diffCount)

diffCount[order(diffCount$x, decreasing=T),]

## 35780 

#######################################

## 4. 해외여행객들의 통화건수가 가장 많은 요일은 무슨 요일
trainData$timestamp <- strptime(paste0(trainData$dateChar," ", trainData$timeChar),
                                format="%Y/%m%d %H%M-%S")

trainData[,"wday"] <- trainData$timestamp$wday
trainData[,"hour"] <- trainData$timestamp$hour
trainData$wday <- factor(trainData$wday, levels=c(1:6,0), ordered=T, labels = c("월","화","수","목","금","토","일"))
head(trainData)

maxCallCount <- aggregate(trainData$timestamp, by=list(trainData$wday), FUN=length)
head(maxCallCount)

maxOrder <- order(maxCallCount$x, decreasing=T)
maxCallCount[order(maxOrder, decreasing=T),]

## 금요일
#######################################

## 5. 해외여행객들의 통화가 가장 많이 발생하는 시간대는? 
hourCall <- aggregate(trainData$timestamp,by=list(hour=trainData$hour),FUN=length)
hourCall
plot(hourCall, type="l")
## 5~6 사이
######################################

## 6. 관광객 개인별 주중통화량과 주말통화건수를 합산한 컬럼을 포함하는 데이터프레임 작성
touristCall <- aggregate(trainData$timestamp, by=list(ID=trainData$touristID, wday=trainData$wday), FUN=length)
head(touristCall)

library(reshape)
dataFrameCall <- cast(touristCall, ID ~ wday, value="x", fun.aggregate=sum) 
head(dataFrameCall)
# weekdayCall 붙이기
dataFrameCall[,"weekdayCall"] <- apply(dataFrameCall[,2:6],1,sum)
# weekendCall 붙이기
dataFrameCall[,"weekendCall"] <- apply(dataFrameCall[,7:8],1,sum)
head(dataFrameCall)

save(dataFrameCall, file="./data/weekData.RData")

######################################
## 7 . 국내행정구역별로 해외방문객의 국가별 통화건수 비중을 데이터프레임으로 작성
nationBycall <- aggregate(trainData$timestamp, by=list(city=trainData$city, nation=trainData$nation), FUN=length)
head(nationBycall)
dataframetest$citySum <- cast(nationBycall, city~nation, value="x", fun.aggregate=sum) 
head(dataframetest)

dataframetest$Percent <- data.frame(city=dataframetest$city,dataframetest[,2:11]/dataframetest$citySum)
head(dataframetest)

dataframetest <- cast(nationBycall, city~nation, value="x", fun.aggregate=sum) 
head(dataframetest)

######################################
## order(wdayStat4Cast$timeSum, decreasing = TRUE)
## aggregate(bikeDataFull$ride.time, by=list(wday=bikeDataFull$ride.wday), FUN=mean)

## 번외 

wdayCount <- function(x) {
  count = 0
  for(i in 1:length(x)) {
    if(x[i] > 0) count = count +1 
  }
  return(count)
}

dataFrameCall$wdayCount <- apply(dataFrameCall[,2:8], 1, function(x) wdayCount(x))
dataFrameCall
##