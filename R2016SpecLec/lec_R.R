library(reshape)
summary(tips)
attach(tips)
## 방문한 손님들의 기록
# 'data.frame':	244 obs. of  7 variables:
# $ total_bill: num  17 10.3 21 23.7 24.6 ...
# $ tip       : num  1.01 1.66 3.5 3.31 3.61 4.71 2 3.12 1.96 3.23 ...
# $ sex       : Factor w/ 2 levels "Female","Male": 1 2 2 2 1 2 2 2 2 2 ...
# $ smoker    : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ day       : Factor w/ 4 levels "Fri","Sat","Sun",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ time      : Factor w/ 2 levels "Dinner","Lunch": 1 1 1 1 1 1 1 1 1 1 ...
# $ size      : int  2 3 3 2 4 4 2 4 2 2 ...

tips2 <- tips 
tips2$size <- factor(tips2$size)
summary(tips2)

tips2$tip[1]  <- 100
summary(tips2$tip)
quantile(tips$tip)

IQR(tips2$tip)
IQR(tips$tip) # 자료의 중간 50% 범위

boxplot(tips$tip, horizontal = TRUE)

tips$day = factor(tips$day , levels = c("Thur", "Fri", "Sat", "Sun"))

qqnorm(tips$tip)
qqline(tips$tip)

table(tips$day)
barplot(table(tips$day))

lbl <- paste(names(table(tips$day)), ",", round(table(tips$day)/sum(table(tips$day)) * 100,2),"%", sep="")
pie(table(tips$day), labels = lbl)

mytable2 <- xtabs(~sex+day, tips)
mytable2

barplot(mytable2, beside = TRUE, legend.text = c("Female", "Male"))

mosaicplot(mytable2)
mosaicplot(t(mytable2))

boxplot(tip~day, data= tips, ylab = "tips", xlab = "day")

plot(tip~total_bill)

###
movie <- read.csv("data/movie_MBA2.csv", header=T)
head(movie)
attach(movie)


boxplot(movie$total_seen, horizontal = T)
library(dplyr)
boxplot(total_seen~rating)

aggsales <- aggregate(total_sales, by=list(rating), mean)
par(las=2)
barplot(aggsales$x, names.arg = aggsales$Group.1)

summary(movie$total_seen)

par(las=2, mar=c(3,3,3,3))
boxplot(total_seen~rating, movie)

tab = xtabs(~genre+rating, movie)
mosaicplot(t(tab))


### 신뢰구간
t.test(tips$total_bill, conf.level = 0.99)

x <- c(15.5,11.21,12.67, 8.87, 12.15, 9.88, 2.06, 14.5, 0, 4.97)
x

t.test(x,mu = 8.1)

fifth_movie <- movie[movie$rating =='15세이상관람가',]
head(fifth_movie)

t.test(fifth_movie$total_seen)

t.test(fifth_movie$total_seen, mu = 1500000, alternative = 'greater')

## 12세 vs 15세
fif_twe_movie <- movie[movie$rating %in% c('15세이상관람가','12세이상관람가'),]
fif_twe_movie$rating <- factor(fif_twe_movie$rating, labels = c('15세이상관람가','12세이상관람가'))

head(fif_twe_movie)
boxplot(total_seen ~ rating, data=fif_twe_movie)
boxplot(log(total_seen) ~ rating, data=fif_twe_movie)

var.test(total_seen ~ rating, data = fif_twe_movie)
t.test(total_seen ~ rating, data = fif_twe_movie, alter = "greater")

var.test(log(total_seen) ~ rating, data = fif_twe_movie)
t.test(log(total_seen) ~ rating, data = fif_twe_movie, alter = "greater", var.equal = T)
## 평균적으로 관객수가 같다 

### earnings 
earnings <- read.csv("data/Earnings.csv")
head(earnings)

with(earnings, shapiro.test(Actual-Predicted)) # 정규분포

with(earnings , boxplot(Actual-Predicted))
with(earnings , t.test(Actual-Predicted))

### 
dental <- read.csv("data/dental.csv")
head(dental)

boxplot(resp~treatment, data=dental)

var.test(log(resp)~treatment, data=dental)

t.test(log(resp)~treatment, data=dental, var.equal=T)

###
FT <- read.csv("data/FT.csv")
head(FT)

shapiro.test(FT$Postwt - FT$Prewt)

with(FT, boxplot(Postwt - Prewt))
with(FT, hist(Postwt - Prewt))
with(FT, t.test(Postwt - Prewt, alternative = "greater"))

### 모비율 
prop.test(67,120, p=0.5)
# prop.test((인지한수),(전체))
prop.test(c(60,120),c(150,250),alter = "less")

# 범주형 자료에 대해 summary 한 데이터를 넣어야함(분할표)
chisq.test(matrix(c(33,28,5,67,122,45),3,2)) # 관련이 없다는 귀무가설을 기각

###
NY = read.csv("data/NYReform.csv")
head(NY)
## Observation    Party Pay.Cut Lobbyists Term.Limits
#1           1 Democrat       1         2           2
#2           2 Democrat       1         1           2
#3           3 Democrat       2         1           2
#4           4 Democrat       1         2           1
#5           5 Democrat       2         2           2
#6           6 Democrat       2         1           1

tab1 <- xtabs(~Pay.Cut+Party, data=NY)
tab2 <- xtabs(~Lobbyists+Party, data=NY)
tab3 <- xtabs(~Term.Limits+Party, data=NY)

chisq.test(tab1)
chisq.test(tab2)
chisq.test(tab3)

mosaicplot(t(tab1))
mosaicplot(t(tab2))
mosaicplot(t(tab3))

tab2
prop.test(21,36)

attitude
pairs(attitude)

# install.packages("psych")
library(psych)

pairs.panels(attitude)

cor.test(attitude$rating, attitude$complaints)

cars_data <- read.csv('data/cars.csv')
head(cars_data)

plot(cars)

out = lm(dist~speed, data=cars)
summary(out)

round(6.356,1)

?round()
signif(6.35,1)

## SST : Y의 총 변동량(Y의 평균으로부터)
## SSE : 회귀식에 의해 설명되지 않는 변동량(잔차(기울기와 변수값))
## SSR : 회귀식에 의해 설명되는 변동량(y-y(hat) 의 값)
## 결정계수 : SSR의 비율이 얼만큼 되느냐를 나타냄

## y 절편이 없는 모형 
out2 = lm(dist~0+speed, data=cars)
summary(out2)

## 잔차는 정규분포를 따른다(평균 : 0 , 분산 : 1)
## 회귀식의 검증은 잔차도 
plot(out2)


#### 모형 수정
# 변수변환 작업 : 1) log(극단적인경우)  2) sqrt(1보다 덜 극단적인경우)
plot(log(dist)~speed, cars)
plot(sqrt(dist)~speed, cars) # sqrt 로 

# sqrt 값
out3 <- lm(sqrt(dist)~0+speed, data=cars)
summary(out3)
plot(out3)

## 추정과 예측 
new <- data.frame(speed=c(10,30)) ## 예측을 위한 값
predict(out3, new)^2 # 결과값을 가지고 역변환을 해야함

# 구간예측 
predict(out3, new, interval = "confidence")^2 # random한 자료를 반복적으로 수행했을때의 평균거리
#         fit       lwr       upr
# 1  15.74101  14.16407  17.40117
# 2 141.66909 127.47660 156.61049


# 새로운 자동차에 대한 제동거리가 궁금한 경우(하나에 대한)
# prediction이기 때문에 lwr와 upr 사이의 거리가 더 넓음(불확실성)
predict(out3, new, interval = "prediction")^2 
#         fit      lwr       upr
# 1  15.74101  2.60064  39.97196
# 2 141.66909 89.83238 205.26208

### fit을 기준으로 최소값(lwr)과 최대값(upr) 신뢰구간을 구해줌

# 관측치에대한 예측치(y(hat))
fitted(out3)
cbind(cars, fitted(out3))

## 이상점과 영향점
# 이상점 : 전체적으로 봤을 때 다른 추세를 나타내는 경우 
# 영향점 : 소수의 관측치들이 통계량에 영향을 미치는 경우
# 잔차도의 마지막 plot 을 말함(점선안에 관측치 값이 있는 경우)
plot(out3)

## 
sonata <- read.csv('data/sonata.csv')
head(sonata)
plot(sonata$Odometer, sonata$Price)
plot(log(sonata$Odometer), sonata$Price)
plot(sqrt(sonata$Odometer), sonata$Price)

sonata_out <- lm(Price~Odometer+0, data=sonata)
summary(sonata_out)
plot(sonata_out)

# 평균거리
new_so <- data.frame(Odometer = 36)
predict(sonata_out, new_so, interval = "confidence")
predict(sonata_out, new_so, interval = "prediction")

# 예측구간
new_so2 <- data.frame(Odometer = 50)
predict(sonata_out, new_so2, interval = "confidence")
predict(sonata_out, new_so2, interval = "prediction")

### 다중회귀분석 
salary <- read.csv("data/salary.csv")
head(salary)

pairs.panels(salary)
model <- lm(salary~0+., data=salary)
summary(model)

### 설명변수에 대한 패널티를 적용한 계수 : 수정다중결정계수(Adjusted R-squared)

# 잔차분석 
plot(model)
salary[2,]

## 색깔 지정
cl = rep(1,20)
cl[2] = 2 

pairs(salary, col = cl, pch = cl)

# 추정
predict(model, data.frame(experience=c(5,10), score=c(80,70)),interval = "confidence" )
predict(model, data.frame(experience=c(5,10), score=c(80,70)),interval = "prediction" )

summary(lm(rating~complaints+learning, attitude))

backward <- step(model, direction = 'backward')

library(leaps)
leaps <- regsubsets(rating~., data=attitude, nbest = 5) # nbest : 변수 선택을 위한 size 개수 지정
summary(leaps) # * 가 식에 들어가 있는 변수를 의미함

windows()
## 검은색깔일수록 좋은거
plot(leaps) # bic 기준(모형선택 기준이 되는 통계량)
plot(leaps, scale = 'adjr2') # adjusted R square

###
laquinta_data <- read.csv('data/laquinta.csv')
head(laquinta_data)
pairs.panels(laquinta_data)

quin_model <- lm(Margin~., data=laquinta_data)
summary(quin_model)

new <- data.frame(Number = 3815, Nearest = 0.9, Office.Space=476,Enrollment = 24.5, Income = 37, Distance= 11.2)
predict(quin_model, new, interval = "confidence")
predict(quin_model, new, interval = "prediction")

quin_leaps <- regsubsets(Margin~., data=laquinta_data, nbest = 5)
summary(quin_leaps)
windows()
plot(quin_leaps)
plot(quin_leaps, scale="adjr2")

### ANOVA
### k 개 이상집단에서 집단간의 평균이 같다 --> 회귀분석에서 F-Test
### 회귀분석으 경우 베타1에 대한 t-test 가 결국 두집단간의 평균이 같다는 것을 의미

### 그룹이 3개 이상
### 더미변수를 생성(변수의 개수 -1)
### 만든 더미변수를 가지고 회귀모형(다중회귀)에 넣는다. 

head(movie)
summary(movie)
movie_model <- lm(log(total_seen)~rating, data=movie)
summary(movie_model)
# B0 : 12세 관람가의 평균
# B1 : 15세 평균 - 12세 평균
# B2 : 전체 평균 - 12세 평균
# B3 : 청소년 평균 - 12세 평균
## F -Test 결과가 0.05 보다 작기 때문에 등급간 평균이 같지 않다고 보임

### 다중비교(Dunnett or Turkey 방법 사용)
# Dunnett : 변수-1 개수만큼에 대해 Test
# turkey : 모든 가능한 쌍들에 대해서 Test 
library(multcomp)
dunnett <- glht(movie_model, linfct=mcp(rating="Dunnett"))
summary(dunnett)
tukey = glht(movie_model, linfct=mcp(rating="Tukey"))
summary(tukey)

### 범주형 변수의 변환 - 목적에 따라 변환 가능
movie$rating2 <- movie$rating
levels(movie$rating2)
# 전체,청소년관람 , 나머지 
levels(movie$rating2) <- c(2,2,1,3)
summary(movie$rating2)

movie_model2 <- lm(log(total_seen)~rating2, data=movie)
levels(movie$rating2) # 참조 그룹으로 R에서 기준점을 정함
movie$rating2 <- relevel(movie$rating2, ref="1") # 전체관람가를 기준으로 나머지를 확인 하고 싶어
movie_model2 <- lm(log(total_seen)~rating2, data=movie)
summary(movie_model2) 

### 공분산분석(ANCOVA)
### 범주형+수치형 변수가 같이 들어간 분석
### anorexia 데이터에서 치료 전 몸무게가 같았을 때의 치료방법을 말함

amo_data <- read.csv("data/anorexia.csv")
class(amo_data)
str(amo_data)
summary(amo_data)

## CBT, Cont, FT 세 그룹에 대해 몸무게 차이 모형화 
ano_out <- lm(Postwt-Prewt~Prewt+Treat, data= amo_data) # Prewt값을 고정시켰을 때의 모형
summary(ano_out)
levels(amo_data$Treat)

boxplot(Prewt~Treat, data=amo_data)
## anova : Treat가 유의하다는 건 Prewt 를 제어했을 때 Treatment 그룹간의 차이가 유의하다고 볼 수 있음
# Analysis of Variance Table

# Response: Postwt - Prewt
# Df Sum Sq Mean Sq F value    Pr(>F)    
# Prewt      1  447.9  447.85  9.1970 0.0034297 ** 
# Treat      2  766.3  383.14  7.8681 0.0008438 ***
# Residuals 68 3311.3   48.70      
anova(ano_out)
## 공분산 다중비교(Dunnett 이나 Tukey)

fobes <- read.csv("data/Forbes500.csv")
windows()
boxplot(Sales~sector, fobes)
boxplot(log(Sales)~sector, fobes)

fobes$sector <- factor(fobes$sector)
fobes$sector <- relevel(fobes$sector, ref="HiTech") # Reference 를 hitech 로

fob_model <- lm(log(Sales)~sector, fobes)
summary(fob_model)
dunnett <- glht(fob_model, linfct = mcp(sector="Dunnett"))
summary(dunnett)

model3 <- lm(log(Sales)~Assets+sector, fobes)
summary(model3)
dunnett <- glht(model3, linfct = mcp(sector="Dunnett"))
summary(dunnett)
