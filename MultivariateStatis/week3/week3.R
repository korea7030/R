## week3 

library(MVA)

## star plot 
## 각각의 그래프가 하나의 관측치를 의미함 
## 모든 관측치 들에 대해 상대적인 크기를 나타냄
## cex : 글씨를 작게함
## key.loc : 비교할 관측치의 위치를 지정
stars(USairpollution, cex=0.7, key.loc = c(15,2))

## 나이팅게일 차트
## draw.segments = TRUE 하면 starts 에서 나이티게일 차트로 나타냄
stars(USairpollution, cex=0.7, key.loc = c(15,2), draw.segments = TRUE)


##########################3 HW2 ###############################
baseball <- read.csv("baseball_201509.csv")
head(baseball)

## labels 지정 시 팀이름이 factor값으로 인식됨 
## 그래서 character로 변경
label = baseball$X
label_chr <- as.character(label)
str(label_chr)

## 그래프 그릴 시 X가 존재하기 때문에 그 값을 빼버림
rownames(baseball) = baseball[,1]
baseball <- baseball[,-1]

head(baseball)
## star Plot
stars(baseball, labels = label_chr,   cex=0.7, key.loc = c(5,2))
## 나이팅게일 차트
stars(baseball, labels = label_chr,   cex=0.7, key.loc = c(5,2), draw.segments = TRUE)

#################################################################

install.packages("aplpack")
library(aplpack)
## 체르노프 그래프
## 출력되는 각각의 요소가 얼굴형태를 결정하는 형태가 됨. 
## 얼굴형태가 같을 수록 관측치들의 차이가 없다는걸 의미
faces(baseball)



## 한꺼번에 그래프 그릴때
## 1. 파일명을 지정해주고 
## 2. 필요한 그래프 그리고 
## 3. 반드시 dev.off() 를 해주면 
## 4. 해당 파일명으로 그림이 그려짐
jpeg("baseball.jpeg")
faces(baseball)
dev.off()

head(USairpollution)

str(USairpollution)

##요약통계량
summary(USairpollution)
## 결과
#   SO2              temp            manu            popul             wind            precip     
# Min.   :  8.00   Min.   :43.50   Min.   :  35.0   Min.   :  71.0   Min.   : 6.000   Min.   : 7.05  
# 1st Qu.: 13.00   1st Qu.:50.60   1st Qu.: 181.0   1st Qu.: 299.0   1st Qu.: 8.700   1st Qu.:30.96  
# Median : 26.00   Median :54.60   Median : 347.0   Median : 515.0   Median : 9.300   Median :38.74  
# Mean   : 30.05   Mean   :55.76   Mean   : 463.1   Mean   : 608.6   Mean   : 9.444   Mean   :36.77  
# 3rd Qu.: 35.00   3rd Qu.:59.30   3rd Qu.: 462.0   3rd Qu.: 717.0   3rd Qu.:10.600   3rd Qu.:43.11  
# Max.   :110.00   Max.   :75.50   Max.   :3344.0   Max.   :3369.0   Max.   :12.700   Max.   :59.80  
# predays     
# Min.   : 36.0  
# 1st Qu.:103.0  
# Median :115.0  
# Mean   :113.9  
# 3rd Qu.:128.0  
# Max.   :166.0  


library(psych)

## summary의 업그레이드
## 요약통계량 및 편차 및 분산 등에 대해서 각변수를 자세히 나타냄
describe(USairpollution)
## se : 표준편차를 루트41로 나눈 값
## 추정치 +- 2se 를 통해 추정치가 얼마나 신뢰를 할 수 있느냐를 나타냄.
## ex) SO2에 대해 mean +- 2se 를 통해 SO2의 신뢰구간을 구할 수 있음.

# vars  n   mean     sd median trimmed    mad   min    max   range  skew kurtosis    se
# SO2        1 41  30.05  23.47  26.00   26.00  17.79  8.00  110.0  102.00  1.58     2.26  3.67
# temp       2 41  55.76   7.23  54.60   55.13   6.23 43.50   75.5   32.00  0.82     0.09  1.13
# manu       3 41 463.10 563.47 347.00  353.79 246.11 35.00 3344.0 3309.00  3.48    14.33 88.00
# popul      4 41 608.61 579.11 515.00  499.67 320.24 71.00 3369.0 3298.00  2.94    10.58 90.44
# wind       5 41   9.44   1.43   9.30    9.45   1.19  6.00   12.7    6.70  0.00     0.06  0.22
# precip     6 41  36.77  11.77  38.74   37.71   7.98  7.05   59.8   52.75 -0.69     0.50  1.84
# predays    7 41 113.90  26.51 115.00  115.09  19.27 36.00  166.0  130.00 -0.55     0.72  4.14

## 출산율 분포 
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
head(birth)
str(birth)
birth$Country

## 관측치를 20개로 나눔
## breaks를 통해 그래프를 좀더 자세히 나타낼 수 있음
hist(birth$X2008, breaks=20)

## 데이터 중에 na(결측치) 값을 없애줌
birth2008 = na.omit(birth$X2008)

birth2008

####################### 분포를 보고자 할 경우(단변량분포) #############################

## Frequency : 구간별 counting을 의미하기 때문에 위에 line을 그릴 수 없음
## freq = FALSE 할 경우 Frequency가 Density로 바뀜
## 즉 구간에서 밀도로 histogram 의 높이가 변경
hist(birth2008, freq = FALSE)

## 연속밀도함수 추정 : 확률을 나타냄(즉, 그래프의 총합은 1이다)
## adjust : 간격을 좀더 촘촘하게 나타낼 경우
## 군집분석 할 시 분포기반을 통해 비슷한 것끼리 묶을 수 있음
lines(density(birth2008), col=3)  
lines(density(birth2008, adjust = 0.5), col=2)

### x : 그리드, y : 높이를 나타냄
# a <- density(birth2008)
# Call:
#   density.default(x = birth2008)
# 
# Data: birth2008 (219 obs.);	Bandwidth 'bw' = 3.168
# 
# x                y            
# Min.   :-1.299   Min.   :6.480e-06  
# 1st Qu.:14.786   1st Qu.:1.433e-03  
# Median :30.870   Median :1.466e-02  
# Mean   :30.870   Mean   :1.553e-02  
# 3rd Qu.:46.954   3rd Qu.:2.646e-02  
# Max.   :63.039   Max.   :4.408e-02  
##################################################################################

############################### 이변량분포 #######################################
library(MASS)
geyser

## 분포 추정을 위한 function : kde2d 
density1 = kde2d(geyser$waiting, geyser$duration, n=100)
str(density1)
## x 그리드 : 100개 , y 그리드 : 100개 , z : x,y에 대한 높이를 나타내는 값
# $ x: num [1:100] 43 45.7 48.4 51.1 53.8 ...
# $ y: num [1:100] 0.833 1.026 1.218 1.41 1.603 ...
# $ z: num [1:100, 1:100] 9.07e-13 1.95e-11 3.10e-10 3.66e-09 3.26e-08 ...

## 밝을 수록 분포가 많이 되어 있음.
image(density1, xlab="waiting", ylab="duration")

## 등고선 모양으로 나타낼 때 
## 숫자는 density의 높이 값을 의미(등고선안의 표면이 확률?을 의미)
contour(density1)

install.packages("rgl")
library(rgl)

## 3차원 형태의 정규분포를 나타낼때
## 위의 그래프에서 색깔 밝기가 이 그래프에선 높이가 높음을 의미
persp3d(density1, back="lines", col="skyblue", xlab="waiting", ylab="duration")
##################################################################################