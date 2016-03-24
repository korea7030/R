## 집단이 3개 이상인 경우 : 적합도 검정

install.packages("TeachingDemos")
library(TeachingDemos)
library(dplyr)
library(reshape)

x <- c(30,20,35,15)
chisq.test(x, p=c(0.25,0.25,0.25,0.25))

## 독립성 검정
chisq.test(matrix(c(18,12,6,14,19,16,12,3), ncol=4))


## HW 3-1 
NYreform <- read.csv("NYReform.csv")
#  Pay.Cut 1 : Y, 2 : N 
#  Lob  1 : Y, 2 : N
# Term.Limits 1:Y , 2:N
head(NYreform) 

#### a ~ d 번
## PayCut에 대한 분할표
xtabs(~Party+Pay.Cut, NYreform)
chisq.test(xtabs(~Party+Pay.Cut, NYreform))

## Lobi 에 대한 분할표
xtabs(~Party+Lobbyists, NYreform)
chisq.test(xtabs(~Party+Lobbyists, NYreform))
## Term.limit에 대한 분할표
DummyTable <- xtabs(~Party+Term.Limits, NYreform)
chisq.test(xtabs(~Party+Term.Limits, NYreform))

######################
#### e : 두집단의 비율이 뭐가 큰지 
# 공화당비율 : 0.71, 민주당비율 : 0.47
head(DummyTable)
DummyTable <- DummyTable[-2,]
DummyTable

prop.test(DummyTable, alter="less")
prop.test(c(32,17), c(45,36), alter="greater")

######################

#### f : 하나의 집단에 대한 비율이 0.5 인지
prop.test(21,36)
######################


##  상관계수 
attitude
names(attitude)
str(attitude)

plot(attitude[,1], attitude[,2])
pairs(attitude)

cor.test(attitude$rating, attitude$privileges)
cor.test(attitude$rating, attitude$advance)
cor(attitude)


cars
summary(cars)
plot(dist~speed, cars)
out <- lm(dist~speed, cars)
out
summary(out)

anova(out)

#### HW 3 1번
sonataData <- read.csv("sonata.csv")
head(sonataData)

## a. 
summary(sonataData$Odometer)
summary(sonataData$Price)

## b
plot()
plot(sonataData$Price ~ sonataData$Odometer)


cor(sonataData$Price, sonataData$Odometer)

## c
out2 <- lm(Price ~ Odometer, sonataData)
abline(out2)

plot(out2)

summary(out2)

