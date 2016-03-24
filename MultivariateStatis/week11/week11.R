install.packages('ISLR')
library(ISLR)

boxplot(balance~default, data=Default)
# Default == Yes 일대 Balance가 더 높다. 

boxplot(income~default, data=Default)
# Default 값에 따라 income 이 별 차이 없다.(중요도가 떨어짐)

## logistic
# family= binomial 로 해야 logistic 분석인지를 앎
glm.fit <- glm(default~balance, data=Default, family="binomial") 
summary(glm.fit)
# Call:
#   glm(formula = default ~ balance, family = "binomial", data = Default)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2697  -0.1465  -0.0589  -0.0221   3.7589  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.065e+01  3.612e-01  -29.49   <2e-16 ***   B0에 대한 변수
#   balance      5.499e-03  2.204e-04   24.95   <2e-16 ***   balance 의 해석 : 카드잔고가 1 증가 시, 파산에 대한 odds(가능성) 1.005514(0.6%) 증가를 의미
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2920.6  on 9999  degrees of freedom
# Residual deviance: 1596.5  on 9998  degrees of freedom
# AIC: 1600.5
# 
# Number of Fisher Scoring iterations: 8

## balnace가 1000일때의 예측
a <- predict(glm.fit, data.frame(balance=1000))
## - 5.152414  P(X) = exp(B0 + B1X) <- 요값이 확률을 의미 / 1+exp(B0+B1X)

exp(a) / (1+exp(a))
## 0.005752145 

predict(glm.fit, data.frame(balance=1000), type="response") ## 위의exp 한  확률을 바로 얻고자 하는 경우

############ Multiple logistic Regression 
# student : 학생여부(1 : O / 0 : X)
glm.fit2 <- glm(default~balance+income+student, data=Default, family="binomial") 
summary(glm.fit2)
str(Default)
# 
# Call:
#   glm(formula = default ~ balance + income + student, family = "binomial", 
#       data = Default)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4691  -0.1418  -0.0557  -0.0203   3.7383  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.087e+01  4.923e-01 -22.080  < 2e-16 ***
#   balance      5.737e-03  2.319e-04  24.738  < 2e-16 ***
#   income       3.033e-06  8.203e-06   0.370  0.71152    
# studentYes  -6.468e-01  2.363e-01  -2.738  0.00619 **  -> factor중에서 Yes인 값의 dummy 변수를 의미
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2920.6  on 9999  degrees of freedom
# Residual deviance: 1571.5  on 9996  degrees of freedom
# AIC: 1579.5
# 
# Number of Fisher Scoring iterations: 8

exp(-6.468e-01)  ## 0.523718  
## 학생이면 파산확률이 반으로 줄어든다는 의미 인데 이런 해석은 X 
## 같은 수입에 잔고가 같은 경우에 50% 로 줄어든다는 해석이 맞는 의미다.

# student만 넣을경우
glm.fit3 <- glm(default~student, data=Default, family="binomial") 
summary(glm.fit3)

exp(0.40489) ## 이럴 경우 50% 증가하게 됨.
## 이 말은 학생의 잔고가 더 많다는 의미를 나타냄.

#위의 exp(0.40489)를 통해 balance 와 student 의 boxplot을 확인해보면
boxplot(balance~student, data=Default)
##학생이면은 balance가 높기때문에 파산 가능성이 높아짐.

## 모형비교
anova(glm.fit2, glm.fit, test="Chisq")
# income+balance가 default를 설명하는데 유의하다는 것을 알 수 있음. (3.904e-06)

# Analysis of Deviance Table
# 
# Model 1: default ~ balance + income + student
# Model 2: default ~ balance
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1      9996     1571.5                          
# 2      9998     1596.5 -2  -24.907 3.904e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coupon= read.csv("coupon.csv")
head(coupon)
## 1000개의 관측치를 5개의 행으로 summary 
?glm

## cbind(성공.실패)
glm.fit4 <- glm(cbind(N_redeemed, N-N_redeemed)~Price_reduc, data=coupon, family="binomial")
summary(glm.fit4)                


# Call:
#   glm(formula = cbind(N_redeemed, N - N_redeemed) ~ Price_reduc, 
#       family = "binomial", data = coupon)
# 
# Deviance Residuals: 
#   1        2        3        4        5  
# -0.8988   0.6677  -0.1837   0.7612  -0.5477  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -2.044348   0.160977  -12.70   <2e-16 ***
#   Price_reduc  0.096834   0.008549   11.33   <2e-16 ***  # 액면가가 유의하다
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 149.4627  on 4  degrees of freedom
# Residual deviance:   2.1668  on 3  degrees of freedom
# AIC: 33.793
# 
# Number of Fisher Scoring iterations: 3

exp(0.096834)
## 쿠폰의 액면가가 1불 증가 할 때, 쿠폰 사용확률이 10% 증가한다는 의미

exp(0.096834*5) ## 액면가가 5불 증가할 때
## 쿠폰의 액면가가 5불 증가 할 때, 쿠폰 사용확률이 62% 증가한다는 의미

## 오분류율, 민감도, 특이도
str(glm.fit2)
## default를 numeric 으로 변환
plot(as.numeric(default) -1~balance, data = Default)  ## balance에 대해 0이 defaul가 아닌거 1이 default인거를 나타냄
points(Default$balance, glm.fit2$fitted.values, col=2)  ## 추정한 값들의 예측이 그래프로 표시된다.

glm.fit2$fitted.values ## 각각의 관측에 대한 default의 확률 예측한 것

## default와 예측한 확률값(glm)을 넣음
pred <- data.frame(default = Default$default, fit = glm.fit2$fitted.values)
head(pred)
## Default의 cutoff를 0.5 보다 크면 파산 아니면 파산 아니다를 구하자
xtabs(~default+ (fit > 0.5), data=pred)

#       fit > 0.5
# default FALSE TRUE
# No   9627   40
# Yes   228  105

## 오분류율 계산
(40+228) / 10000 
## 민감도 
105 / (228+105)
## 특이도
9627 / (9627+40)

### 위의 결과 특이도가 굉장히 높고, 민감도가 낮음(cutree에 따라 달라진다.)

## ROCR Curve
install.packages("ROCR")
library(ROCR)

## 추정한 확률값 , 실제값
predob = prediction(pred$fit, pred$default)
## predicion obj , index
# tpr : true positive rate
# fpr : false positive rate
a <- performance(prediction.obj = predob, "tpr", "fpr" )
str(a)

# Formal class 'performance' [package "ROCR"] with 6 slots
# ..@ x.name      : chr "False positive rate"
# ..@ y.name      : chr "True positive rate"
# ..@ alpha.name  : chr "Cutoff"
# ..@ x.values    :List of 1 :   1-특이도
# .. ..$ : num [1:10001] 0 0 0 0 0 ...
# ..@ y.values    :List of 1 :   민감도 
# .. ..$ : num [1:10001] 0 0.003 0.00601 0.00901 0.01201 ...
# ..@ alpha.values:List of 1
# .. ..$ : num [1:10001] Inf 0.978 0.974 0.966 0.957 ...
plot(a)

## AUC 값을 구하고 싶다면
?performance 
performance(predob , "auc")
# An object of class "performance"
# Slot "x.name":
#   [1] "None"
# 
# Slot "y.name":
#   [1] "Area under the ROC curve"
# 
# Slot "alpha.name":
#   [1] "none"
# 
# Slot "x.values":
#   list()
# 
# Slot "y.values":
#   [[1]]
# [1] 0.9495581  : 요게 auc값
# 
# 
# Slot "alpha.values":
#   list()

plot(performance(predob ,  "err")) ## cutoff 증가에 따른 error율
e <- performance(predob ,  "err")
str(e)
## error rate 값
err <- e@y.values[[1]]
cut <- e@x.values[[1]]

## 에러를 작게 하는 cutoff 수 확인
cut[err==min(err)]  ## 0.4298679




############################### HW5 ##################################
head(Smarket)  ## 주가 수익률
# Lag 1 ~ 5 : 전일의 주가 수익률
# volumn : 전일 거래량 
# Today : 오늘 수익률 
# Direction : 시작이 상승 했는지 여부(Up/Down)

glm.smarket <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family="binomial")
summary(glm.smarket)  ## Up에 대한 summary로 보면 됨.
head(Smarket)
str(Smarket)
# 
# Call:
#   glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
#         Volume, family = "binomial", data = Smarket)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -1.446  -1.203   1.065   1.145   1.326  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|) = P-Value
# (Intercept) -0.126000   0.240736  -0.523    0.601  B0   
# Lag1        -0.073074   0.050167  -1.457    0.145  B1
# Lag2        -0.042301   0.050086  -0.845    0.398  B2
# Lag3         0.011085   0.049939   0.222    0.824  B3
# Lag4         0.009359   0.049974   0.187    0.851  B4
# Lag5         0.010313   0.049511   0.208    0.835  B5
# Volume       0.135441   0.158360   0.855    0.392  B6
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1731.2  on 1249  degrees of freedom
# Residual deviance: 1727.6  on 1243  degrees of freedom
# AIC: 1741.6
# 
# Number of Fisher Scoring iterations: 3
exp(0.010313 * 0.01)
exp(0.135441)

str(Smarket)

pred_smarket <- data.frame(Direction = Smarket$Direction, fit = glm.smarket$fitted.values)
xtabs(~Direction+(fit>0.5), data=pred_smarket)

(141+457) / 1250  # 오분류율
507 / (141+507) # 민감도
145 / (145+457) # 특이도
predsmark_ob <- prediction(pred_smarket$fit, pred_smarket$Direction)
smarket_perf <- performance(prediction.obj = predsmark_ob, "tpr" , "fpr")
plot(smarket_perf)

# 오류율
plot(performance(predsmark_ob ,  "err")) ## cutoff 증가에 따른 error율
e1 <- performance(predsmark_ob ,  "err")

err1 <- e1@y.values[[1]]
cut1 <- e1@x.values[[1]]

cut1[err1 == min(err1)][[1]]

## 0.5069883

###################################################################################