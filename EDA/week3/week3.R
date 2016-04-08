library(RColorBrewer)
sales <- read.csv("sales.csv", header = T)
head(sales)

tab1 <- table(sales$CT1)
tab1 <- sort(tab1, decreasing = TRUE)

sales_names <- paste(names(tab1), ": ",round(tab1/sum(tab1)*100,2),"%", sep="")
pie(tab1, labels = sales_names, col=brewer.pal(11,"RdBu"), main="카테고리 구매내역(%)")

mosaicplot(~Location1+CT1,sales, las=2)
mosaicplot(~CT1+Location1, sales, las=2)

library(dplyr)

library(plyr)

sales_summary <- ddply(sales, .(CT1, month), summarize, sum_price = sum(Price))

ggplot(sales_summary, aes(x=month, y=sum_price, group=CT1, colour = CT1))+
  geom_line(size=1.0)+geom_point()+
  scale_colour_brewer(palette = "RdYlBu", breaks = levels(sales_summary$CT1), name="대분류")+
  ylab("총 구매금액")+
  scale_x_continuous(breaks=1:12)

ggplot(sales_summary, aes(x=month, y=sum_price, group=CT1, fill = CT1))+
  geom_area()+
  scale_x_continuous(breaks=1:12)+
  scale_fill_brewer(palette="RdYlBu", breaks=levels(sales_summary$CT1), name="대분류")

sales_summary3 <- ddply(sales[sales$CT1=="가전제품",], ~CT2+month, summarize, sum_price=sum(Price))
unique(sales_summary3$CT2)

ggplot(sales_summary3, aes(x=month, y=sum_price, group=CT2, colour = CT2))+
  geom_line(size=1.0)+geom_point()+
  scale_colour_brewer(palette = "RdBu", breaks = levels(sales_summary$CT2), name="중분류")+
  ylab("총 구매금액")+
  scale_x_continuous(breaks=1:12)

## 트리맵
## 분류 분포 시각화, 위계 구조가 데이터나 트리 구조의 데이터 시각화
## 색깔과 크기를 통해 형태 확인 가능
posts <- read.csv("http://datasets.flowingdata.com/post-data.txt")

# install.packages("portfolio")
library(portfolio)
map.market(id=posts$id, area = posts$views, group = posts$category, color = posts$comments, main="FlowingData Map")

GNI2010 <- read.csv("GNI2010.csv", header = T)
head(GNI2010)

map.market(id=GNI2010$iso3, area = GNI2010$population, color = GNI2010$GNI-mean(GNI2010$GNI), lab=c("group"=TRUE, "id"=TRUE), group = GNI2010$continent)

## 누적 연속 그래프
## 시간에 따른 범주형 변수 분포 시각화 
library(ggplot2)
pop <- read.csv("us-population-by-age.csv", header=T)
head(pop)
pop2 <- reshape(pop, direction="long", varying = list(names(pop)[2:6]), times = names(pop)[2:6])
head(pop2)
summary(pop2)

pop2$time <- factor(pop2$time, levels=c("Under.5", "X5.to.19", "X20.to.44", "X45.to.64", "X65."))
## geom_area
ggplot(pop2, aes(x=Year, y=Under.5, group = factor(time), fill=factor(time)))+geom_area(position="fill")+ylab("Propotion")+scale_fill_brewer(palette = "Blues", name = "Ages", breaks = levels(pop2$time))
## geom_line
ggplot(pop2, aes(x=Year, y=Under.5, group = time, color=time))+geom_line(size=1.5)+ylab("Propotion")+scale_colour_brewer(palette = "RdYlBu", name = "Ages", breaks = levels(pop2$time))

## 양적 변수의 시각화
## 두 변수 사이의 관계를 보기 위함

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv")
plot(crime$murder, crime$burglary)

identify(crime$murder, crime$burglary, labels=crime$state)

round(cor(crime[,-1]),2)

crime2 <- crime[-c(10,1),]
plot(crime2$murder, crime2$population)

identify(crime2$murder, crime2$population)
crime2[1,]

# install.packages("psych")
library(psych)
pairs.panels(crime2)

############################## Practice3 #####################################
sales_plus <- subset(sales, (Price > 0))
summary(sales_plus)
head(sales_plus)

demo <- read.csv("silver_demo.csv", header = T )
head(demo)

sales_ddply <- ddply(sales_plus, .(ID), summarize, 총구매수량 = sum(Amount), 총구매금액= sum(Price), 구매횟수 = length(ID), 평균구매금액=mean(Price))
head(sales_ddply)

merge_sales <- merge(sales_ddply, demo, by="ID")
head(merge_sales)

plot(merge_sales$구매횟수, merge_sales$총구매금액/10^8, xlab="구매횟수", ylab = "총구매금액", ylim = c(0,1.5), main = "구매횟수에 따른 총구매금액")
mtext("(단위 : 금액 1억)", line = 1, at=0)

identify(merge_sales$구매횟수, merge_sales$총구매금액/10^8, label = merge_sales$ID)

merge_sales[1203,]

plot(merge_sales$총구매수량, merge_sales$총구매금액/10^8, xlab="총구매수량", ylab = "총구매금액", ylim = c(0,1.5), main = "총구매수량에 따른 총구매금액")
mtext("(단위 : 금액 1억)", line = 1, at=0)
identify(merge_sales$총구매수량, merge_sales$총구매금액/10^8, label = merge_sales$ID)
merge_sales[792,]

plot(merge_sales$평균구매금액, merge_sales$총구매금액/10^8, xlab="평균구매금액", ylab = "총구매금액", ylim = c(0,1.5), main = "평균구매금액에 따른 총구매금액")
mtext("(단위 : 금액 1억)", line = 1, at=0)

identify(merge_sales$평균구매금액, merge_sales$총구매금액/10^8, label = merge_sales$ID)
merge_sales[533,]
merge_sales[2141,]

barplot(table(merge_sales$Age), ylim = c(0,500))

plot(merge_sales$구매횟수, log(merge_sales$총구매금액), xlab="구매횟수", ylab = "총구매금액", main = "구매횟수에 따른 총구매금액")
mtext("(단위 : 금액 1억)", line = 1, at=0)

plot(merge_sales$총구매수량, log(merge_sales$총구매금액), xlab="총구매수량", ylab = "총구매금액",  main = "총구매수량에 따른 총구매금액")
mtext("(단위 : 금액 1억)", line = 1, at=0)
