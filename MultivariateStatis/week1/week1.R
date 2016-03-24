install.packages("MVA")
library(MVA)

demo("Ch-MVA")

library(lattice)

measure

## 전체 데이터의 가슴,허리,엉덩이 둘레 의 공분산 
cov(measure[,1:3])

## 남자의 공분산 
cov(measure[measure$gender =="male", 1:3])

## 여자의 공분산
cov(measure[measure$gender =="female", 1:3])

## 상관계수 행렬 
cor(measure[,1:3])

##############################################
## 평균이 0 이고 표준편차가 1인 데이터로 만들어줌
cov(scale(measure[,1:3]))


# chest 1.0000000 0.6987336 0.4778004
# waist 0.6987336 1.0000000 0.4147413
# hips  0.4778004 0.4147413 1.0000000

cov(measure[,1:3])

## 거리 구하기 --> 가장 적은 거리의 관측치를 군집분석에 활용 가능
round(dist(scale(measure[,1:3])),2)



