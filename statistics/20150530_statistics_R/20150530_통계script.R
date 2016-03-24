model = lm(dist~speed+0, data=cars)
model
summary(model)
plot(model)

cars
head(cars)
cars[c(23,35,49), ]

plot(sqrt(dist)~speed, cars)
model1 <- lm(sqrt(dist)~speed+0, cars)
new <- data.frame(speed=c(10,30))
new

predict(model1, new)

## 예측구간
predict(model1, new, interval="prediction")

## 신뢰구간
predict(model1, new, interval="confidence")

cbind(speed, fitted(model1))

plot(cars$speed, fitted(model1))

########## 4-1  
 

sonata <- read.csv("sonata.csv")
head(sonata)

out = lm(Price ~ Odometer+0,  sonata)
plot(out)

summary(out)
## 잔차도 분석
plot(lm(log(Price)~Odometer, sonata))

new <- data.frame(Odometer = c(36))
new

## 3600 마일일 경우
predict(out, new, interval="confidence")

new2 <- data.frame(Odometer = c(50))
## 5000 마일일 경우 -> 5000마일에 대한 데이터가 없으므로, 3600 으로 예측구간을 구한다. 
predict(out, new2, interval="prediction")

head(sonata)
# sonata[sonata$Odometer == c(36.0,50.0),]


##############################

salaryData <- read.csv("salary.csv")
head(salaryData)

pairs(salaryData)
cor(salaryData)

salaryOut <- lm(salary~experience+score, salaryData)
salaryOut2 <- lm(salary~experience, salaryData)

summary(salaryOut)

##############################

laquintaData <- read.csv("laquinta.csv")

head(laquintaData)

## 산점도
pairs(laquintaData)

## 회귀식 추정(2번의 회귀계수 추정 및 해석, 회귀식 다 구할수 있음)
laquintaDataOut <- lm(Margin~Number+Nearest+Office.Space+Enrollment+Income+Distance, laquintaData)
summary(laquintaDataOut)

## 잔차분석
plot(laquintaDataOut)

head(laquintaDataOut)

## 2번의 g번
predict(laquintaDataOut, data.frame("Number" = 3815, "Nearest" = 0.9, "Office.Space"=476, "Enrollment" = 24.5, "Income" = 37, "Distance"= 11.2))

install.packages("leaps")
library(leaps)

## 2번의 h
## 모든 변수의 subset에 대해 size당 nbest개의 모형을 저장할 겻인가 설정 
leaps2 =regsubsets(Margin~., data=laquintaData, nbest=5)
# * : 모형에 들어가 있는 설명변수를 의미 
summary(leaps2)

## 이 그래프를 통해 가장 까만 부분의 설명변수를 찾고
plot(leaps2,scale="adjr2")

summary.out=summary(leaps2)
## 가장 최적의 모델 모형의 변수 위치를 찾아서
which.max(summary.out$adjr2)

summary.out$which[26,]

leapOut <- lm(Margin~. , data=laquintaData)
## 요약을 하면 회귀식을 구할 수 있음.
summary(leapOut)
######################
summary(attitude)

attitudeOut <- lm(rating~., attitude)
summary(attitudeOut)

attitudeOut2 <- lm(rating~.-critical, attitude)
summary(attitudeOut2)
backward <- step(attitudeOut, direction="backward")
backward

forward <- step(attitudeOut, direction="forward")

both <- step(attitudeOut, direction="both")

install.packages("leaps")
library(leaps)

## 모든 변수의 subset에 대해 size당 nbest개의 모형을 저장할 겻인가 설정 
leaps =regsubsets(rating~., data=attitude, nbest=5)

# * : 모형에 들어가 있는 설명변수를 의미 
summary(leaps)

plot(leaps)
plot(leaps,scale="adjr2")
plot(leaps, scale="Cp")

