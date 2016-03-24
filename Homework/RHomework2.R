fermentedData <- basket[basket$division == "fermented milk", ]
dim(fermentedData)
fermentedDataGroup <- group_by(fermentedData, custId, category, division)
fermentedDataSum <- summarize(fermentedDataGroup, sumAmount = sum(amount))
fermentedDataArrange <- arrange(fermentedDataSum, desc(sumAmount))

##fermented milk 513 
dim(fermentedDataArrange)

##### 퍼먼티드 밀크 중 베이비 프로덕트 사지 않은 사람 빼기 시작 ###############
fermentedDataArrangeTmp <- left_join(fermentedDataArrange, ferment_baby_rmNA)

dim(fermentedDataArrangeTmp)

fermentedDataArrangeNotBPro <- fermentedDataArrangeTmp[is.na(fermentedDataArrangeTmp$baby),]


dim(fermentedDataArrangeNotBPro)  ## 퍼먼티드 밀크 중 베이비 안산사람 : 495명
###########################################

##fermented milk 평균(베이비 프로덕트를 사지 않는 사람의 평균금액)
fermentedMean <- sum(fermentedDataArrangeNotBPro$sumAmount) / 495

# 퍼먼트 밀크를 사고 베이비 프로덕트를 사지 않는 사람의 평균 : 25458.67
fermentedMean  


names(babyPurchaseTot) <- c("custId", "daily", "baby", "amt")
ferment_baby <- left_join(fermentedDataArrange, babyPurchaseTot)
ferment_baby

ferment_baby_rmNA <- ferment_baby[!is.na(ferment_baby$daily), ]
## 베이비 물건 사면서 퍼먼티드 밀크 사는 사람 : 18명
dim(ferment_baby_rmNA)

## 18명의 평균 fermented milk 구매금액 평균 :  40600
ferment_baby_rmNA

babyfermean <- sum(ferment_baby_rmNA$sumAmount)/ 18