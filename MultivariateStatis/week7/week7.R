app <- read.table(file="Applicant.TXT", header=T)

app <- app[,-1]
str(app)

## 15개의 변수 회사의 job position에 대해 지원자 48명의 명단
## 해당 position에 대한 business 능력을 평가
## 각각의 능력 점수는 10점 만점으로 평가


# $ X1.FL.   : int  6 9 7 5 6 7 9 9 9 4 ...
# $ X2.APP.  : int  7 10 8 6 8 7 9 9 9 7 ...
# $ X3.AA.   : int  2 5 3 8 8 7 8 9 7 10 ...
# $ X4.LA.   : int  5 8 6 5 8 6 8 8 8 2 ...
# $ X5.SC.   : int  8 10 9 6 4 8 8 9 8 10 ...
# $ X6.LC.   : int  7 9 8 5 4 7 8 9 8 10 ...
# $ X7.HON.  : int  8 9 9 9 9 10 8 8 8 7 ...
# $ X8.SMS.  : int  8 10 7 2 5 5 8 8 5 10 ...
# $ X9.EXP.  : int  3 5 4 8 8 9 10 10 9 3 ...
# $ X10.DRV. : int  8 9 9 4 5 6 8 9 8 10 ...
# $ X11.AMB. : int  9 9 9 5 5 5 10 10 9 10 ...
# $ X12.GSP. : int  7 8 8 8 8 8 8 9 8 10 ...
# $ X13.POT. : int  5 8 6 7 8 6 9 9 8 9 ...
# $ X14.KJ.  : int  7 8 8 6 7 6 8 9 8 3 ...
# $ X15.SUIT.: int  10 10 10 5 7 6 10 10 10 10 ...

## 어떤 잠재 요인이 15개의 변수에 대해 얼마나 영향을 주는지 파악

## factor의 개수를 4개로 했을 때
fal <- factanal(app, 4)

fal



# Call:
#   factanal(x = app, factors = 4)
# 
## Uniquenesses: 특정인자의 분산값(ui값) 
## 값이 커질수록 factor에 대해 설명이 가능하지 않음.
## 1- 각각의 분산값 = 설명가능 % 로 생각
## ex) X2.APP = 1- 0.685 = 0.315(31.5%) 만큼 설명이 가능하다.

# X1.FL.   X2.APP.    X3.AA.    X4.LA.    X5.SC.    X6.LC.   X7.HON.   X8.SMS.   X9.EXP.  X10.DRV. 
# 0.443     0.685     0.521     0.185     0.119     0.198     0.339     0.138     0.357     0.226 
# X11.AMB.  X12.GSP.  X13.POT.   X14.KJ. X15.SUIT. 
# 0.137     0.153     0.090     0.005     0.252 
# 
## 인자적재값(factor loading)
## 빈공간이 있는 경우는 0의 값이라고 생각
## -의 의미는 factor가 음의 영향을 준다는 의미
# Loadings:
#             l11      l12    l13     l14
#           Factor1 Factor2 Factor3 Factor4
# X1.FL.     0.129   0.717   0.113  -0.117 
# X2.APP.    0.458   0.142   0.243   0.164 
# X3.AA.             0.126           0.677 
# X4.LA.     0.231   0.239   0.838         
# X5.SC.     0.918           0.142         
# X6.LC.     0.838   0.111   0.291         
# X7.HON.    0.252  -0.216   0.742         
# X8.SMS.    0.885   0.258                 
# X9.EXP.            0.778           0.165 
# X10.DRV.   0.767   0.389   0.172         
# X11.AMB.   0.904   0.181                 
# X12.GSP.   0.792   0.275   0.351   0.148 
# X13.POT.   0.735   0.349   0.432   0.247 
# X14.KJ.    0.424   0.389   0.554  -0.598 
# X15.SUIT.  0.364   0.770           0.142 
# 
#                 Factor1 Factor2 Factor3 Factor4
# SS loadings      5.570   2.473   2.099   1.013
# Proportion Var   0.371   0.165   0.140   0.068
# Cumulative Var   0.371   0.536   0.676   0.744
# 
# Test of the hypothesis that 4 factors are sufficient.
# The chi square statistic is 84 on 51 degrees of freedom.
# The p-value is 0.00247 


print(fal, digits=2, sort=T)

# Call:
#   factanal(x = app, factors = 4)
# 
# Uniquenesses: 
# X1.FL.   X2.APP.    X3.AA.    X4.LA.    X5.SC.    X6.LC.   X7.HON.   X8.SMS.   X9.EXP.  X10.DRV. 
# 0.44      0.68      0.52      0.18      0.12      0.20      0.34      0.14      0.36      0.23 
# X11.AMB.  X12.GSP.  X13.POT.   X14.KJ. X15.SUIT. 
# 0.14      0.15      0.09      0.00      0.25 
# 
## 인자적재값 행렬
## 공통성 : SC = 0.92*0.92 + 0.14*0.14 
##        : LC = 0.84*0.84 + 0.29*0.29 = 0.802 이면 나머지 20%는 위의 uniqunesses 값을 의미
## factor1에 의해 설명가능한 변수 : SC, Lc, SM, DR, AM, GC, PO
## factor1과 SC의 상관계수가 0.92를 나타낸다.즉 factor1과 sc는 연관이 많이 되는 변수
# Loadings:

## 각각의 변수에 대해 factor1~4 가 얼마만큼 설명가능한지 
#          Factor1 Factor2 Factor3 Factor4
# X5.SC.     0.92            0.14          
# X6.LC.     0.84    0.11    0.29          
# X8.SMS.    0.88    0.26                  
# X10.DRV.   0.77    0.39    0.17          
# X11.AMB.   0.90    0.18                  
# X12.GSP.   0.79    0.28    0.35    0.15  
# X13.POT.   0.74    0.35    0.43    0.25  
# X1.FL.     0.13    0.72    0.11   -0.12  
# X9.EXP.            0.78            0.17  
# X15.SUIT.  0.36    0.77            0.14  
# X4.LA.     0.23    0.24    0.84          
# X7.HON.    0.25   -0.22    0.74          
# X3.AA.             0.13            0.68  
# X14.KJ.    0.42    0.39    0.55   -0.60  
# X2.APP.    0.46    0.14    0.24    0.16  
# 
#                 Factor1 Factor2 Factor3 Factor4
# SS loadings       5.57    2.47    2.10    1.01
# Proportion Var    0.37    0.16    0.14    0.07  (factor각각이 전체 분산의 몇% 만큼 설명이 가능한가)
# Cumulative Var    0.37    0.54    0.68    0.74
# 
# Test of the hypothesis that 4 factors are sufficient.
# The chi square statistic is 84 on 51 degrees of freedom.
# The p-value is 0.00247  ## 귀무가설 기각

## 귀무가설 : 4개의 factor가 충분하다. 
## 대립가설 : 4개의 factor가 충분하지 않다.

## factor1과 factor2의 값들을 plot
## 각 점은 원래 변수들을 의미
load <- fal$loadings
load

## 새창 띄움
windows()
## type = 'n' : 아무것도 안찍힘
plot(load, type='n')
text(load, labels=names(app), cex=0.7)
## 각 factor에 대해서 어떤 원변수가 설명을 잘해주는지 판단이 가능.

################### HW1 ###########################

stock_price <- read.csv("stock_price.csv", header=T)

head(stock_price)

str(stock_price)

stock_price <- stock_price[,-1]

str(stock_price)
# data.frame':	103 obs. of  6 variables:
## 주별 주가 정보
# $ Week           : int  1 2 3 4 5 6 7 8 9 10 ...
# $ JPMorgan       : num  0.01303 0.00849 -0.01792 0.02156 0.01082 ...   (금융기관)
# $ Citibank       : num  -0.00784 0.01669 -0.00864 -0.00349 0.00372 ... (금융기관)
# $ WellsFargo     : num  -0.00319 -0.00621 0.01004 0.01744 -0.01013 ... (금융기관)
# $ RoyalDutchShell: num  -0.0448 0.012 0 -0.0286 0.0292 ...             
# $ ExxonMobil     : num  0.00522 0.01349 -0.00614 -0.00695 0.04098 ...

stock_fal <- factanal(stock_price, factors = 2)

stock_fal_none <- factanal(stock_price, factors = 2, rotation = "none")

stock_fal

stock_fal_none

# 
# Call:
#   factanal(x = stock_price, factors = 2)
# 
# Uniquenesses:
#   JPMorgan        Citibank      WellsFargo RoyalDutchShell      ExxonMobil 
# 0.417           0.275           0.542           0.005           0.530 
# 
# Loadings:
#                Factor1 Factor2
# JPMorgan        0.763          
# Citibank        0.819   0.232  
# WellsFargo      0.668   0.108  
# RoyalDutchShell 0.113   0.991  
# ExxonMobil      0.108   0.677  
# 
#                 Factor1 Factor2
# SS loadings      1.725   1.507
# Proportion Var   0.345   0.301
# Cumulative Var   0.345   0.646
# 
# Test of the hypothesis that 2 factors are sufficient.
# The chi square statistic is 1.97 on 1 degree of freedom.
# The p-value is 0.16 

stock_load <- stock_fal$loadings
plot(stock_load, type="n")
text(stock_load, labels=names(stock_price), cex=0.7)

stock_none_load <- stock_fal_none$loadings
plot(stock_none_load, type="n")
text(stock_none_load, labes=names(stock_price), cex=0.7)
#######################################################################################

##### 에러남....
library(psych)
install.packages("GPArotation")
library(GPArotation)

fa3 <- fa(app, 4, rotation="quartimax")

plot(load[,1], load[,3], type="n")
text(fa3, load[,1], load[,3], cex=0.7)

#########################

##인자점수 추정 

fal4 <- factanal(app, 4, scores="regression")

fal4

marketing <- fal4$scores[,1]
marketing2 <- apply(app[,c(5,6,7,8,10:13)],1, sum)


