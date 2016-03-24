data <- read.csv("potthoffroy.csv")
head(data)

#data 변환
data <- reshape(data, idvar="id", varying=list(3:6), v.names="dist", direction="long")
head(data)

library(ggplot2)
ggplot(data, aes(x=time, y=dist, group=id, colour=sex))+geom_line(stat="identity")+geom_point()

model1 <- lm(dist~time, data=data)
summary(model1)

model1$fitted

data$fit1 <- model1$fitted

# 그룹이 아닌 하나의 패턴으로 추정을 원할 경우
ggplot(data, aes(x=time, y=fit1, group=id, colour=sex))+geom_line(stat="identity")+geom_point()


model2 <- lm(dist~time+sex, data=data)
summary(model2)

# Call:
#   lm(formula = dist ~ time + sex, data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.9882 -1.4882 -0.0586  1.1916  5.3711 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  19.3468     0.5968  32.416  < 2e-16 ***
#   time          1.3204     0.1955   6.753 8.25e-10 ***
#   sexM          2.3210     0.4449   5.217 9.20e-07 ***  남녀 구분을 위한 더비 변수 남자변수가 생성됨.
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.272 on 105 degrees of freedom
# Multiple R-squared:  0.4095,	Adjusted R-squared:  0.3983 
# F-statistic: 36.41 on 2 and 105 DF,  p-value: 9.726e-13

data$fit2 <- model2$fitted

# 남녀의 차이를 나타냄(y절편)
ggplot(data, aes(x=time, y=fit2, group=id, colour=sex))+geom_line(stat="identity")+geom_point()

model3 <- lm(dist~time+sex+sex:time, data=data)


data$fit3 <- model3$fitted

ggplot(data, aes(x=time, y=fit3, group=id, colour=sex))+geom_line(stat="identity")+geom_point()
summary(model3)

# Call:
#   lm(formula = dist ~ time + sex + sex:time, data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.6156 -1.3219 -0.1682  1.3299  5.2469 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  20.2500     0.8334  24.297  < 2e-16 ***
#   time          0.9591     0.3043   3.152  0.00212 ** 
#   sexM          0.7969     1.0827   0.736  0.46337    
#   time:sexM     0.6097     0.3953   1.542  0.12608    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.257 on 104 degrees of freedom
# Multiple R-squared:  0.4227,	Adjusted R-squared:  0.4061 
# F-statistic: 25.39 on 3 and 104 DF,  p-value: 2.108e-12


##################################
timber <- read.csv("timber.csv")
head(timber)
ggplot(timber, aes(x=slippage, y=loads, group=specimen))+geom_line(stat="identity")+geom_point()

model4 <- lm(loads~slippage+I(slippage^2), data=timber) 
summary(model4)

# Call:
#   lm(formula = loads ~ slippage + I(slippage^2), data = timber)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.21305 -0.42879 -0.00969  0.38713  1.75596 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.9434     0.1496   6.305 5.29e-09 *** 
#   slippage       19.8891     0.4038  49.259  < 2e-16 ***  b1 
#   I(slippage^2)  -5.4295     0.2209 -24.581  < 2e-16 ***  b2
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6673 on 117 degrees of freedom
# Multiple R-squared:  0.9869,	Adjusted R-squared:  0.9866 
# F-statistic:  4394 on 2 and 117 DF,  p-value: < 2.2e-16

timber$fit1 <- model4$fitted
ggplot(timber, aes(x=slippage, y=fit1, group=specimen))+geom_line(stat="identity")+geom_point()

model5 <- lm(loads~slippage+I(slippage^2)+specimen, data=timber)
summary(model5)

# Call:
#   lm(formula = loads ~ slippage + I(slippage^2) + specimen, data = timber)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.68398 -0.32520 -0.09023  0.33461  1.24506 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.77331    0.17535   4.410 2.42e-05 ***
#   slippage      19.88910    0.32194  61.778  < 2e-16 ***
#   I(slippage^2) -5.42945    0.17612 -30.829  < 2e-16 ***
#   specimenspec2  0.44867    0.19427   2.309  0.02278 *  
#   specimenspec3  0.03267    0.19427   0.168  0.86678    
# specimenspec4 -0.64200    0.19427  -3.305  0.00128 ** 
#   specimenspec5  0.39267    0.19427   2.021  0.04569 *  
#   specimenspec6  0.91067    0.19427   4.688 7.98e-06 ***
#   specimenspec7  0.16667    0.19427   0.858  0.39281    
# specimenspec8  0.05133    0.19427   0.264  0.79209    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.532 on 110 degrees of freedom
# Multiple R-squared:  0.9921,	Adjusted R-squared:  0.9915 
# F-statistic:  1544 on 9 and 110 DF,  p-value: < 2.2e-16

timber$fit2 <- model5$fitted
ggplot(timber, aes(x=slippage, y=fit2, group=specimen))+geom_line(stat="identity")+geom_point()

library(lme4)
# install.packages("lmerTest")
library(lmerTest)
model6 <- lmer(loads~slippage+I(slippage^2)+(1|specimen), data=timber)
summary(model6)

# Linear mixed model fit by REML ['lmerMod']
# Formula: loads ~ slippage + I(slippage^2) + (1 | specimen)
# Data: timber
# 
# REML criterion at convergence: 211.4
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.0335 -0.5492 -0.1410  0.5915  2.3797 
# 
# Random effects:  # 독립적인 데이터가 아니므로 Random effects 값이 필요
#   Groups   Name        Variance Std.Dev.
# specimen (Intercept) 0.1807   0.4251  
# Residual             0.2831   0.5320  
# Number of obs: 120, groups:  specimen, 8
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)     0.9434     0.1919    4.92
# slippage       19.8891     0.3219   61.78
# I(slippage^2)  -5.4295     0.1761  -30.83
# 
# Correlation of Fixed Effects:
#   (Intr) slippg
# slippage    -0.521       
# I(slippg^2)  0.435 -0.959

timber$fit3 <- fitted(model6)
ggplot(timber, aes(x=slippage, y=fit3, group=specimen))+geom_line(stat="identity")+geom_point()
# 나무들 간의 변동치를 예측하여 나타냄

 
model7 <- lmer(loads~slippage+I(slippage^2)+(1+slippage|specimen), data=timber)
timber$fit4 <- fitted(model7)
ggplot(timber, aes(x=slippage, y=fit4, group=specimen))+geom_line(stat="identity")+geom_point()


model8 <- lmer(dist~time+sex+sex:time+(1|id), data=data)
summary(model8)
data$fit8 <- fitted(model8)
ggplot(data, aes(x=time, y=fit8, group=id, colour=sex))+geom_line(stat="identity")+geom_point()


model9 <- lmer(dist~time+sex+sex:time+(1+time|id), data=data)
summary(model9)
data$fit9 <- fitted(model9)
ggplot(data, aes(x=time, y=fit9, group=id, colour=sex))+geom_line(stat="identity")+geom_point()
