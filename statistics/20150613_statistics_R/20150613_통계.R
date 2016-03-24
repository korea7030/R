forbes <- read.csv("Forbes500.csv")
boxplot(forbes)

summary(forbes$sector)

anorexia <- read.csv("anorexia.csv")
head(anorexia)

summary(anorexia)
anorexia$Treat<- relevel(anorexia$Treat, ref="Cont")
summary(anorexia)

## 몸무게의 차이를 치료전의 몸무게(양적변수)와 Treat(질적변수)로 나타내라
out <- lm(Postwt-Prewt~Prewt+Treat, anorexia)
## 그룹간의 비교는 Dunnet Test를 통해 비교
summary(out)

## Treat의 p-value 값이 0.05보다 작기 때문에 Treat가 동시에 효과가 있다.
## 가정은 prewt가 통제되어있다는 가정하에 
anova(out)
?glht

install.packages("multcomp")
library(multcomp)

dunnet = glht(out, linfct = mcp(Treat="Dunnet"))
summary(dunnet)


############# 부분 F 검정 #################
sbp
## +의 의미는 ~ 의 앞뒤로 넣을 경우 뒤의 설명변수의 개수를 추가함을 의미
model1 <- lm(sbp~age+gender+age*gender, sbp)
summary(model1)

model2 <- lm(sbp~age+gender, sbp)
summary(model2)

## 두 모델의 P value값 확인 (0.05보다 크면 귀무가설 채택, 0.05보다 작으면 귀무가설 기각)
anova(model2, model1)

## 성별마저 고려하기 싫다면?
model3 <- lm(sbp~age, sbp)

## anova 확인 시 0.05보다 p 값이 작기 때문에 성별은 무조건 고려를 해야 한다 
anova(model3, model1)


###########################################


## 5.1 
head(forbes)

boxplot(forbes$Sales, forbes$sector)
boxplot(log(forbes$Sales), forbes$sector)

####

## 5.2
forbesOut1 <- lm(log(Sales)~sector, forbes)

summary(forbesOut1)

####

## 5.3
plot(forbes$Sales, forbes$Assets)

plot(log(forbes$Sales),log(forbes$Assets))

## forbesOut2 <- lm(Sales~Assets, forbes)


####

## 5.4
model1 <- lm(log(Sales)~log(Assets)+sector+log(Assets)*sector, forbes)
summary(model1)

model2 <- lm(log(Sales)~log(Assets)+sector, forbes)

anova(model2, model1)  ## log(Assets)의 차이가 없음 

anova(model2)

summary(model2)

dunnet = glht(model2, linfct = mcp(sector="Dunnet"))
dunnet
summary(dunnet)
## Finance, Retail 

## 5.6
new <- data.frame(Assets=3000, sector="Finance")

predict(model2, new)

## log sales에 관한 범위 
predict(model2, new, interval="confidence")

exp(5.935671)  ## 378.2937

exp(6.427346) ## 618.5302 


####
####