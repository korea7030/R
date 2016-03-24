dm_data <- read.csv("DM_data.csv", header=T)
head(dm_data)
str(dm_data)

subset_dm <- subset(dm_data, 
                   select = c(T_ID,T_SEX,T_AGE,T_HTN,T_HTNAG,T_HTNCU,T_DM,T_DMAG,T_DMCU,T_LIP,T_LIPAG,T_LIPCU,T_FMFDM1,T_FMFDM2,T_FMFDM3,T_FMFDM4,T_DRINK,T_DRQYR,T_DRDU,T_SOJUFQ,T_BEERFQ,T_SOJUAM,T_BEERAM,T_EAT2,T_EAT3,T_EAT4,T_FRY,T_EXER,T_CHILDNO,BMI, T_SBP, T_DBP, T_HDL, T_LDL, T_SS01, T_SS02, T_SS03, T_SS04, T_SS05, T_SS10, T_SS24), 
                   subset= !is.na(T_LDL) &  !is.na(T_SBP) & !is.na(T_DBP))


# 비만, 혈압, 콜레스테롤 정보 get
subset_dm_1 <- subset(subset_dm, select = c(BMI, T_SBP, T_DBP, T_HDL, T_LDL))
is.na(subset_dm_1)

pairs(subset_dm_1)

pcm1 <- prcomp(subset_dm_1, scale=T)
pcm1

summary(pcm1)

# PC1 : 전체적으로 당뇨병에 걸릴 확률이 높으나 이중에서 혈압이 높은 사람이 당뇨병을 걸릴 확률이 높다고 설명 가능.(종합적)
# PC2 : 혈압(수축,이완), 좋은콜레스테롤, 나쁜콜레스테롤이 떨어져도 당뇨병이 발생한다. 
# PC3 : BMI(비만도)와 나쁜콜레스테롤이 떨어져도 당뇨병에 걸릴 확률에 대한 설명 가능
# PC4 : BMI(비만도)와 좋은콜레스테롤이 떨어져도 당뇨병에 걸릴 확률에 대해 설명 가능.

par(mfrow=c(1,4))
barplot(pcm1$rotation[,1], main ="PC1", col= c(2,3,4,5,6))  # 종합적으로 당뇨와 관련
barplot(pcm1$rotation[,2], main ="PC2", col= c(2,3,4,5,6))  # HDL, LDL : 음의방향 : 합병증 발생에 기여
barplot(pcm1$rotation[,3], main ="PC3", col= c(2,3,4,5,6))  # BMI, LDL : 강한 음의방향
barplot(pcm1$rotation[,4], main ="PC4", col= c(2,3,4,5,6))  # BMI, HDL : 강한 양의방향, LDL : 음의방향

par(mfrow=c(1,1))

biplot(pcm1, choices=c(1,2))
biplot(pcm1, choices=c(1,3))
biplot(pcm1, choices=c(1,4))

plot(pcm1, type="l")
screeplot(pcm1, type="l")

subset_dm_1$PC1 <- pcm1$x[,1]
subset_dm_1$PC2 <- pcm1$x[,2]
subset_dm_1$PC3 <- pcm1$x[,3]

plot(pcm1$x[,1], pcm1$x[,2])

# 회귀분석 결과
lm_pc1 <- lm(data = subset_dm_1, formula = PC1~BMI+T_SBP+T_DBP)
lm_pc2 <- lm(data = subset_dm_1, formula = PC2~BMI+T_HDL+T_LDL)
lm_pc3 <- lm(data = subset_dm_1, formula = PC3~BMI+T_LDL)

summary(lm_pc1) # BMI, T_SBP, T_DBP 
summary(lm_pc2) # BMI, T_HDL, T_LDL
summary(lm_pc3) # BMO, T_LDL

# lm_pc1의 회귀식 : -12.70 + 0.07292BMI + 0.04279T_SBP + 0.07101T_DBP 
# lm_pc2의 회귀식 : 1.132 + 0.1396BMI - 0.0648T_HDL - 0.0147LDL 
# lm_pc3의 회귀식 : 6.4983 - 0.1586BMI - 0.0228T_LDL

## 결론 : 비만도, 혈압(수축/이완), 콜레스테롤(H/L)에 대해 주성분2(PC2)에 대한 회귀식의 모형이 매우 유의하며 99.6%의 설명력을 가진다.
##        비만이면서 LDL이 낮더라도 HDL도 같이 낮게 되면 당뇨병에 걸릴 수 있다.

############################################################################################

test_data <- read.csv("test.csv", header=T)
head(test_data)

test_data <- na.omit(test_data)
str(test_data)

## 고혈압에 대한 흡연,음주,운동여부,영양소(탄수화물, 단백질, 지방) 상관여부 위한 데이터 get
dm_subset <- subset(test_data, 
                    select = c(T_HTNAG, T_FMFHT, T_SMK, T_DRIN, T_EXER,T_SS02,T_SS03, T_SS04 ), 
                    subset= T_HTN == 2 & T_HTNAG != 100)
head(dm_subset)


####### 요인분석
library(psych)

# 요인분석을 위한 요인개수 결정
fa.parallel(dm_subset) ## 실행결과 factor 4개로 

# 4개의 요인분석 수행
fal <- factanal(dm_subset, 4, scores = "regression")
fal

# factor 1 : 탄수화물, 단백질, 지방에 의해 요인 결정
# factor 2 : 고혈압 진단 나이 로 요인 결정
# factor 3 : 탄수화물로 요인 결정
# factor 4 : 흡연과 음주로 요인 결정

## 요인 변수 결과를 데이터셋에 지정
dm_subset$fa1 <- fal$scores[,1]
dm_subset$fa2 <- fal$scores[,2]
dm_subset$fa3 <- fal$scores[,3]
dm_subset$fa4 <- fal$scores[,4]

head(dm_subset)

## 회귀분석 수행
fa1_lm <- lm(data= dm_subset, formula = fa1~T_SS02+T_SS03+T_SS04 )
fa2_lm <- lm(data= dm_subset, formula = fa2~T_HTNAG )
fa3_lm <- lm(data= dm_subset, formula = fa3~T_SS02+T_SS04 )
fa4_lm <- lm(data= dm_subset, formula = fa4~T_SMK+T_DRIN )

## 회귀분석 결과
summary(fa1_lm)  # 1번 회귀식 : 98% 설명 가능
summary(fa2_lm)  # 2번 회귀식 : 99% 설명 가능
summary(fa3_lm)  # 3번 회귀식 : 62% 설명 가능
summary(fa4_lm)  # 4번 회귀식 : 92% 설명 가능

# 확인 결과 요인 2에 대한 회귀식이 99% 설명이 되므로, 고혈압은 나이와 상관있다.