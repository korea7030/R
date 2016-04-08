crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv", header=T)
head(crime)
crime2 <- crime[-c(1,10),]
pairs(crime2)

## 버블차트(x,y,circles)
symbols(crime2$murder, crime2$burglary, circles = crime2$population, inches = 0.5)
symbols(crime2$murder, crime2$burglary, circles = sqrt(crime2$population), inches = 0.3,
        fg="white", bg = "skyblue")

text(crime2$murder, crime2$burglary, crime2$state, cex = 0.5)

library(rgl)
plot3d(crime2$murder, crime2$burglary,crime2$population, type = "s", col="blue",xlab = "Murder",ylab = "Burglary", zlab = "Population")

############### practice 4 #####################################
sales <- read.csv("sales.csv", header = T)
sales2 <- sales[sales$Price>0, ]
library(plyr)
head(sales2)
sales2_CT1 <- ddply(sales2, .(CT1), summarize, sum_amount = sum(Amount), sum_price = sum(Price/1000), sales_cnt = length(Price), mean_price = mean(Price/1000), cnt_cus = length(unique(ID)))
sales2_CT2 <- ddply(sales2, .(CT2), summarize, sum_amount = sum(Amount), sum_price = sum(Price), sales_cnt = length(Price), mean_price = mean(Price), cnt_cus = length(unique(ID)))

head(sales2_CT1)
head(sales2_CT2)

symbols(sales2_CT1$sum_amount, sales2_CT1$sales_cnt, circles = sqrt(sales2_CT1$mean_price), inches = 0.5, las=2, xlab= "Amount", ylab = "Freq")
text(sales2_CT1[,2], sales2_CT1[,4]+5000, sales2_CT1[,1])
symbols(sales2_CT2$sum_amount, sales2_CT2$sales_cnt, circles = sqrt(sales2_CT2$mean_price), inches = 0.5, las=2, xlab= "Amount", ylab = "Freq")
text(sales2_CT2[,2], sales2_CT2[,4]+5000, sales2_CT2[,1])

sales2_idct <- ddply(sales2, .(ID,CT1), summarize, 총구매수량 = sum(Amount), 총구매금액= sum(Price), 구매횟수 = length(ID), 평균구매금액=mean(Price))
summary(sales2_idct)
histogram(~log(총구매수량)|CT1, data=sales2_idct, layout = c(2,5))
histogram(~log(총구매금액)|CT1, data=sales2_idct, layout = c(2,5))
histogram(~log(구매횟수)|CT1, data=sales2_idct, layout = c(2,5))
histogram(~log(평균구매금액)|CT1, data=sales2_idct, layout = c(2,5))

?histogram

update(h, index.cond = list(c(41:50, 31:40, 21:30, 11:20, 1:10))) ## index 조정
####

sales_ddply <- ddply(sales2, .(ID), summarize, 총구매수량 = sum(Amount), 총구매금액= sum(Price), 구매횟수 = length(ID), 평균구매금액=mean(Price))
hist(log(sales_ddply$총구매수량), 20)
lines(density(log(sales_ddply$총구매수량)))

library(MASS)
attach(geyser)
## grid point 25개 
density1 <- kde2d(waiting, duration, n=100)
str(density1)
image(density1, xlab="waiting", ylab="duration")

contour(density1, xlab="waiting", ylab="duration")
points(waiting, duration, col=2)

persp3d(density1, col="skyblue", xlab="waiting", ylab="duration")

####
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv", header = T)
head(birth)
hist(birth$X2008, xlab="Birth", main = NA)
hist(birth$X2008, breaks = 20, xlab="Birth", main = NA)

birth2 <- reshape(birth, varying = list(2:50), direction = "long", idvar = "Country", v.names = "birth", timevar = "year")
birth2$year = birth2$year+1959
birth2$year <- as.character(birth2$year)
h <- histogram(~birth|year, data=birth2, layout = c(10,5))
update(h, index.cond = list(c(41:50, 31:40, 21:30, 11:20, 1:10))) ## index 조정

