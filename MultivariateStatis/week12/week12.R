turkey <- read.csv("turkey.csv")
turkey <- na.omit(turkey)
head(turkey)

## turkey의 각 부위별 길이가 type 별로 나타나 있음
#      ID HUM RAD ULN FEMUR TIN CAR D3P COR SCA TYPE
# 13 B710 153 140 147   142 151 817 305 102 128 WILD
# 14 B790 156 137 151   146 155 814 305 111 137 WILD
# 17 B819 158 135 151   146 152 790 289 111 125 WILD
# 19 B085 148 129 146   139 147 767 287 106 123 WILD
# 20 B089 157 140 154   140 159 818 301 116 136 WILD
# 21 B090 153 138 153   141 151 822 312 115 133 WILD

## 두개의 그룹으로 나뉘어지는지 확인
plot(HUM~ULN,data=turkey, col=TYPE)

# LDA
library(MASS) 
## 전체적인 것을확인 할 때, prior 옵션 사용
model1 <- lda(TYPE~HUM+ULN, data=turkey, prior=c(0.4,0.6))
model1

#QDA
model2 <- qda(TYPE~HUM+ULN, data=turkey)
model2
# Call:
#   lda(TYPE ~ HUM + ULN, data = turkey)
# 
# Prior probabilities of groups:  양쪽그룹에 들어갈 사전확률
#   DOMESTIC      WILD 
# 0.5757576 0.4242424 
# 
# Group means: 다변량 정규분포의 평균
#   HUM      ULN
# DOMESTIC 146.7368 143.7895
# WILD     153.0000 149.9286
# 
# Coefficients of linear discriminants:
#   LD1
# HUM 0.1469784
# ULN 0.1028563


predict(model1, data.frame("HUM"=c(145,150), "ULN"=c(150,145)))

# $class
# [1] DOMESTIC DOMESTIC
# Levels: DOMESTIC WILD
# 
# $posterior : 사후확률
# DOMESTIC      WILD
# 1 0.7139229 0.2860771
# 2 0.6392539 0.3607461
# 
# $x : 판변함수 값
# LD1
# 1 -0.27490816
# 2 -0.05429765

predict(model2, data.frame("HUM"=c(145,150), "ULN"=c(150,145)))
# 
# $class
# [1] WILD     DOMESTIC
# Levels: DOMESTIC WILD
# 
# $posterior : 사후확률
# DOMESTIC      WILD
# 1 0.00221008 0.9977899
# 2 0.68425690 0.3157431


plot(model1)  ## 선형식의 값이 양쪽 그래프로 나타남

install.packages("klaR")
library(klaR)


# LDA
## 그래프 확인 시 선형으로 두개의 그룹으로 나누어짐
## 두 그룹에 있는 점이 각 그룹의  평균점을 나타냄
partimat(TYPE~HUM+ULN, data=turkey, method="lda")

# QDA
## 곡선의 형태로 나누어짐
partimat(TYPE~HUM+ULN, data=turkey, method="qda")

## Cross validation 
model3 <- lda(TYPE~HUM+ULN, data=turkey, CV=TRUE)
str(model3)
model3$class
## 예측된 클래스
# [1] WILD     WILD     WILD     DOMESTIC WILD     WILD     WILD     WILD     WILD     DOMESTIC WILD    
# [12] WILD     WILD     DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC WILD     DOMESTIC DOMESTIC DOMESTIC
# [23] DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC DOMESTIC

xtabs(~turkey$TYPE+model3$class)

#             model3$class
# turkey$TYPE DOMESTIC WILD
# DOMESTIC       18    1
# WILD            3   11

## error rate 를 계산한다면 
4/33 # 0.1212121

##################################HW5 2번 #############################################
library(ISLR)

# default data, train vs test 
# [1] 10000     4
idx=1:dim(Default)[1]
idx # 1~10000 vector 생성

## ## sample(나타낼 수, 데이터범위, 반복여부, 각 숫자별 비율)
idx.train = sample(idx, trunc(length(idx)/2) , replace=FALSE)

## training set 생성
train <- Default[idx.train,]
## test set 생성
test <- Default[-idx.train,]

?sample

model4 <- lda(default~balance+income, data=train)
pred_default <- predict(model4, test)
str(pred_default)

xtabs(~test$default+pred_default$class)

# test$default   No  Yes
# No          4816   11
# Yes         133   40


#######################################################################################

## mba 자료(여러 그룹의 판별분석)
mba <- read.csv("mba.csv", header=T)

plot(GPA~GMAT, col=result, data=mba)
summary(mba)
# GPA             GMAT            result 
# Min.   :2.130   Min.   :313.0   accepted:31  
# 1st Qu.:2.600   1st Qu.:425.0   pending :26  
# Median :3.010   Median :482.0   rejected:28  
# Mean   :2.975   Mean   :488.4                
# 3rd Qu.:3.300   3rd Qu.:538.0                
# Max.   :3.800   Max.   :693.0  

boxplot(GPA~result, data=mba, ylab="GPA")
## 세 그룹의 결과에 따라 GPA 분포가 다름
boxplot(GMAT~result, data=mba,ylab="GMAT" )

# GMAT~result
# 귀무 가설 : 3개의 그룹간의 GMAT이 차이가 없다.
# 대립 가설 : 3개의 그룹간의 GMAT이 차이가 있다.
anova1 <- aov(GMAT~result, data=mba)
summary(anova1)

## 판별분석
model_mba <- lda(result~GPA+GMAT, data=mba, CV=TRUE)
model_mba$class

# [1] accepted pending  pending  accepted accepted accepted accepted accepted accepted accepted accepted
# [12] accepted accepted accepted accepted accepted accepted accepted accepted accepted accepted accepted
# [23] accepted pending  accepted accepted accepted accepted accepted accepted pending  rejected rejected
# [34] rejected rejected rejected rejected rejected rejected rejected rejected rejected rejected rejected
# [45] rejected rejected rejected rejected rejected rejected rejected rejected rejected rejected rejected
# [56] rejected rejected pending  pending  pending  pending  pending  pending  pending  pending  accepted
# [67] pending  pending  pending  pending  pending  pending  pending  pending  rejected pending  pending 
# [78] pending  pending  pending  pending  pending  pending  pending  pending

xtabs(~model_mba$class+mba$result)
#                   mba$result
# model_mba$class accepted pending rejected
# accepted       27       1        0
# pending         4      24        2
# rejected        0       1       26

## LDA 
partimat(result~GPA+GMAT, data=mba)
## 3개의 그룹에 대해 나누어지는 형태의 그래프가 나타남.

## QDA
partimat(result~GPA+GMAT, data=mba, method='qda')
