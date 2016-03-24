## 1988년 25명의 여자 선수의 올림픽 7종 경기 결과 
library(MVA)
getwd()

str(heptathlon)
# 'data.frame':	25 obs. of  8 variables:
# $ hurdles : num  12.7 12.8 13.2 13.6 13.5 ...
# $ highjump: num  1.86 1.8 1.83 1.8 1.74 1.83 1.8 1.8 1.83 1.77 ...
# $ shot    : num  15.8 16.2 14.2 15.2 14.8 ...
# $ run200m : num  22.6 23.6 23.1 23.9 23.9 ...
# $ longjump: num  7.27 6.71 6.68 6.25 6.32 6.33 6.37 6.47 6.11 6.28 ...
# $ javelin : num  45.7 42.6 44.5 42.8 47.5 ...
# $ run800m : num  129 126 124 132 128 ...
# $ score   : int  7291 6897 6858 6540 6540 6411 6351 6297 6252 6252 ...

head(heptathlon)
#                      hurdles highjump  shot run200m longjump javelin run800m score
# Joyner-Kersee (USA)   12.69     1.86 15.80   22.56     7.27   45.66  128.51  7291
# John (GDR)            12.85     1.80 16.23   23.65     6.71   42.56  126.12  6897
# Behmer (GDR)          13.20     1.83 14.20   23.10     6.68   44.54  124.20  6858
# Sablovskaite (URS)    13.61     1.80 15.23   23.92     6.25   42.78  132.24  6540
# Choubenkova (URS)     13.51     1.74 14.76   23.93     6.32   47.46  127.90  6540
# Schulz (GDR)          13.75     1.83 13.50   24.65     6.33   42.82  125.79  6411

## hurdles, run200m, run800m 의 경우는 작을수록 기록이 잘나오기 때문에 가장 큰값에서 빼주는 걸로
## 바꾸는 작업이 필요함. 

heptathlon$hurdles = with(heptathlon, max(hurdles) - hurdles)
heptathlon$run200m = with(heptathlon, max(run200m) - run200m)
heptathlon$run800m = with(heptathlon, max(run800m) - run800m)

head(heptathlon)
pairs(heptathlon)

## hurdles 가 최소인 선수
heptathlon[heptathlon$hurdles == min(heptathlon$hurdles), ]  ## Launa (PNG)

heptathlon_excep = heptathlon[heptathlon$hurdles>min(heptathlon$hurdles),]


head(heptathlon_excep)

pairs(heptathlon_excep)
rownames(heptathlon)

pairs(heptathlon, col = as.numeric(heptathlon$hurdles == min(heptathlon$hurdles))+1, pch = as.numeric(heptathlon$hurdles == min(heptathlon$hurdles))+1)

summary(heptathlon_excep)
## 모든 변수가 동일한 조건으로 하고자 할 때 상관계수로 구해야 함.
# hurdles         highjump          shot          run200m         longjump        javelin     
# Min.   :1.570   Min.   :1.680   Min.   :10.00   Min.   :0.000   Min.   :5.470   Min.   :35.68  
# 1st Qu.:2.373   1st Qu.:1.770   1st Qu.:12.54   1st Qu.:1.530   1st Qu.:6.072   1st Qu.:38.95  
# Median :2.690   Median :1.800   Median :12.91   Median :1.805   Median :6.265   Median :40.28  
# Mean   :2.688   Mean   :1.794   Mean   :13.17   Mean   :2.024   Mean   :6.205   Mean   :41.28  
# 3rd Qu.:2.973   3rd Qu.:1.830   3rd Qu.:14.20   3rd Qu.:2.757   3rd Qu.:6.370   3rd Qu.:44.39  
# Max.   :3.730   Max.   :1.860   Max.   :16.23   Max.   :4.050   Max.   :7.270   Max.   :47.50  
# run800m          score     
# Min.   :16.76   Min.   :5289  
# 1st Qu.:25.30   1st Qu.:5916  
# Median :29.11   Median :6154  
# Mean   :28.52   Mean   :6154  
# 3rd Qu.:31.38   3rd Qu.:6366  
# Max.   :39.23   Max.   :7291  

h_pca = prcomp(heptathlon_excep[,1:7], scale=TRUE)
h_pca

# Standard deviations:
#   [1] 2.0793370 0.9481532 0.9109016 0.6831967 0.5461888 0.3374549 0.2620420
# 
# Rotation:  값이 작을 수록 선수의 기록이 좋고 잘하는 것을 나타냄
#               PC1         PC2        PC3         PC4         PC5         PC6         PC7
# hurdles  -0.4503876  0.05772161 -0.1739345  0.04840598 -0.19889364  0.84665086 -0.06961672
# highjump -0.3145115 -0.65133162 -0.2088272 -0.55694554  0.07076358 -0.09007544  0.33155910
# shot     -0.4024884 -0.02202088 -0.1534709  0.54826705  0.67166466 -0.09886359  0.22904298
# run200m  -0.4270860  0.18502783  0.1301287  0.23095946 -0.61781764 -0.33279359  0.46971934
# longjump -0.4509639 -0.02492486 -0.2697589 -0.01468275 -0.12151793 -0.38294411 -0.74940781
# javelin  -0.2423079 -0.32572229  0.8806995  0.06024757  0.07874396  0.07193437 -0.21108138
# run800m  -0.3029068  0.65650503  0.1930020 -0.57418128  0.31880178 -0.05217664  0.07718616

## 주성분점수
plot(h_pca$x[,1], heptathlon_excep$score)
rownames(heptathlon_excep) <- abbreviate(gsub("\\(.*","", rownames(heptathlon_excep)))
h_pca = prcomp(heptathlon_excep[,1:7], scale=TRUE)

## 원변수가 7개 이기 때문에 7차원 그래프가 나옴.
## 분산을 1로 맞췄기 때문에, 7차원 그래프에서 볼 경우 길이는 다 같다.
biplot(h_pca)
## 각도가 벌어질 수록 상관관계가 낮다고 봄
## 원변수의 관계와, 주성분값(PC1, PC2)의 관계를 알 수 있음.

biplot(h_pca, choices = c(1,3))
