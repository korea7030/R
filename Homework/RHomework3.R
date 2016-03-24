##밀크 구매하는 데이터 구하기
lanData <- basket[basket$division=="milk",]

lanDataGroup <- group_by(lanData, custId, category, division) 
lanDataSum <- summarize(lanDataGroup, sum=sum(amount))
lanDataArrange <- arrange(lanDataSum, desc(sum))

head(lanDataArrange)

##밀크 , 622명
dim(lanDataArrange)


lan_baby <- left_join(lanDataArrange, babyPurchaseTot)
head(lan_baby)

lan_baby_rmNA <- lan_baby[!is.na(lan_baby$daily), ]
## 베이비 중 밀크 사는 사람 : 20명
dim(lan_baby_rmNA)
head(lan_baby_rmNA)

## 밀크 중 베이비 산사람 빼기 시작 ###################################
lanDataArrangeTmp <-left_join(lanDataArrange, lan_baby_rmNA) 

dim(lanDataArrangeTmp)

lanDataArrangeNotPro <- lanDataArrangeTmp[is.na(lanDataArrangeTmp$baby),] 

dim(lanDataArrangeNotPro) ## 밀크 중 베이비 안산 사람 602명 
head(lanDataArrangeNotPro)
######################### 밀크 중베이지 산사람 빼기 끝#############

## 밀크 중 베이비 안산 사람 평균 
milkMean <- sum(lanDataArrangeNotPro$sum) / 602
milkMean  ## 30268.24  

##베이비 밀크 평균
lan_baby_rmNAMean <- sum(lan_baby_rmNA$sum) / 20
lan_baby_rmNAMean ## 67332 


?data.frame

meanData <- data.frame(milkMean, lan_baby_rmNAMean)
is.data.frame(meanData)

data <- c("우유", "아기용품")
data2 <- c(milkMean, lan_baby_rmNAMean)

names(data2) <- c("우유", "아기용품")
dataTot <- rbind(data, data2)

df <- data.frame(data, data2)
barplot(df$data,df$data2)
bplot = barplot(data2, col=rainbow(5), xlab = "구매용품", ylab="평균금액", main="우유 평균 금액  vs 아기용품 구매 평균금액")

text(x=bplot, y = data2*1.05,  labels=paste("(",data2[1],"원",")"), col="black", cex=0.7)


data3 <- c(602/2462, 20/38)
names(data3) <- c("우유", "아기용품")

barplot(data3, col=rainbow(5), xlab="구매용품", ylab="구매비율", main="우유 구매 비율 vs 아기용품 구매 비율")
