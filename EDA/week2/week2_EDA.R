hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv")
head(hotdogs)
## Year : 년도 , Winner : 우승자명 , Dogs.eaten : 먹은 개수, Country : 나라 이름
# Year                       Winner Dogs.eaten       Country New.record
# 1980 Paul Siederman & Joe Baldini       9.10 United States          0
# 1981              Thomas DeBerry       11.00 United States          0
# 1982               Steven Abrams       11.00 United States          0
# 1983                 Luis Llamas       19.50        Mexico          0
# 1984               Birgit Felden        9.50       Germany          0
# 1985             Oscar Rodriguez       11.75 United States          0
str(hotdogs)
# data.frame':	31 obs. of  5 variables:
# $ Year      : int  1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 ...
# $ Winner    : Factor w/ 18 levels "Birgit Felden ",..: 13 18 15 9 1 12 10 2 6 6 ...
# $ Dogs.eaten: num  9.1 11 11 19.5 9.5 ...
# $ Country   : Factor w/ 4 levels "Germany","Japan",..: 4 4 4 3 1 4 4 4 4 4 ...
# $ New.record: int  0 0 0 0 0 0 0 0 0 0 ...
summary(hotdogs)
colors = rep("grey", 31) ## 색깔 반복 지정(변수 개수 만큼)
colors[new.record==TRUE] = "blue" ## 미국은 회색으로 지정
## barplot(y축, names.arg = x축값 , xlab  = x축 이름 , ylab = y축 이름, main = 그래프의 제목, ylim = y축의 범위 변경 , xlim = x축의 범위 변경)
bp <- barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col=colors, border=NA, xlab="Year", ylab="Hot dogs and buns (HDB) eaten", main="Nathan's Hot Dog Eating Contest Results, 1980-2010", ylim= c(0,70))
## legend(배치위치 ,legend = 레전드명, fill = 넣을 색 지정, border = 선,title = 레전드의 제목)
legend("topleft", legend=c("United States", "Others"), fill=c("grey", "blue"), border=NA, title="winner's Nationality")
# hotdogs[hotdogs$New.record == 1,]
# x축의 위치를 찾기 위해선 barplot을 변수처리르 통해 처리 가능(http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm)
text(bp[new.record], hotdogs$Dogs.eaten[new.record]+1, hotdogs$Dogs.eaten[new.record], cex=0.7)

eaten.max = c(); eaten.max[1] = hotdogs$Dogs.eaten[1]
for (i in 2:31) {
  eaten.max[i] = max(hotdogs$Dogs.eaten[1:(i-1)])
  
}

eaten.max
new.record = eaten.max<hotdogs$Dogs.eaten

# check.names : 숫자앞에 X 빼기
hotdog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", check.names = FALSE)

head(hotdog_places)
#   2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010
# 1   25 50.0 50.5 44.5 53.5   49   54   66   59 68.0   54
# 2   24 31.0 26.0 30.5 38.0   37   52   63   59 64.5   43
# 3   22 23.5 25.5 29.5 32.0   32   37   49   42 55.0   37

## matrix 변경
hotdog_places <- as.matrix(hotdog_places)
## 자동으로 쌓아서 보여줌
barplot(hotdog_places, beside=T)
## year 두자리로 변수 생성
years <- paste("'", substr(colnames(hotdog_places), 3,4), sep="")

barplot(hotdog_places, names.arg = years)

#################################### practice 1 #############################################
library(XLConnect)
library(plyr)
install.packages("reshape")
library(reshape)

data <- read.csv("OctagonExcel.csv", check.names = FALSE)
rownames(data) <- data[,1]
data <- data[,-1]
data <- as.matrix(data)


years_pr <- paste("'", substr(colnames(data), 3,4), sep="")
years_pr

barplot(data, names.arg = years_pr, col=1:5) ## 전체적으로 이혼건수가 감소 - 결혼하는 수가 적기 때문인 듯?
legend("topright", legend=rownames(data), fill=c(1:5), border=NA, title="결혼년수")

head(data)
#### 비율로 변환(data를 분모의 매트릭스로)
data_rate <- data/matrix(1,5,1) %*% apply(data, 2, sum)
# 다 더한 값이 1인지 확인
apply(data_rate, 2, sum)
head(data_rate)
barplot(data_rate, names.arg = years_pr, col=1:5)
legend("topright", legend=rownames(data), fill=c(1:5), border=NA, title="결혼년수")
#############################################################################################
options(scipen=999)
population <- read.csv("http://datasets.flowingdata.com/world-population.csv")
plot(population$Year, population$Population, type="l", ylim=c(0,7*10^9))
points(population$Year, population$Population, pch=20, xlab="Year", ylab="Population")

######################
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv")
head(postage)
str(postage)

plot(postage$Year, postage$Price, type="s")
points(postage$Year, postage$Price, pch=20)

#################### time series
unemployment <- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv")
head(unemployment)
summary(unemployment)

# install.packages("zoo")
library(zoo)
## 1~10까지 3개의 수의 평균
rollmean(1:10,3)
## 
rollmean(1:10, 10)

ma50 <- rollmean(unemployment$Value, 50)
ma5 <- rollmean(unemployment$Value, 5)
plot(ma50, type="l", ylim=c(0,10))
lines(ma5, type="l")

unemployment.ts <- ts(unemployment$Value, start = c(1948,1), frequency=12)
plot(unemployment.ts, type="l", ylim=c(0,11), ylab="unemployment Rate", xlab="Year")
lines(lowess(unemployment.ts), col=4, lwd=1.5)
## f 자료의 점에서 몇분의 몇의 자료를 평활 할 것인가
lines(lowess(unemployment.ts, f=1/3), col=2, lwd=1.5)
lines(lowess(unemployment.ts, f=1/6), col=3, lwd=1.5)


## 계절형 시계열 분해 
export <- read.table("Export_1988.txt", header= T)
head(export)
# time-series object 지정
series <- ts(export$Series/1000, start=c(1988,1), frequency=12) 
plot(series)

# log 취함(변동성이 시간의 흐름에 따라 커지는 경향이 있는 경우에 취함)
# 경제의 지표의 경우도 취함(왜냐하면 증가분이 비율로 나타나기 때문)
log.series <- log(series)
plot(log.series)

decomp.out = decompose(log.series) # time-series 분해 function
str(decomp.out)
# $ x       : Time-Series [1:215] from 1988 to 2006: 8.28 8.32 8.47 8.44 8.47 ... (원본자료)
# $ seasonal: Time-Series [1:215] from 1988 to 2006: -0.1384 -0.1145 0.0347 -0.0019 0.0126 ... (계절성)
# $ trend   : Time-Series [1:215] from 1988 to 2006: NA NA NA NA NA ... (추세성)
# $ random  : Time-Series [1:215] from 1988 to 2006: NA NA NA NA NA ... (에러)
# $ figure  : num [1:12] -0.1384 -0.1145 0.0347 -0.0019 0.0126 ...
# $ type    : chr "additive"
# - attr(*, "class")= chr "decomposed.ts"

plot(decomp.out$trend) # 추세 패턴(계절성과 random error 를 제거)
plot(decomp.out$seasonal) # 계절마다의 패턴

plot(decomp.out$x- decomp.out$seasonal) # 검은색 선(trend와 random error를 포함한 그래프)
lines(decomp.out$x, col=2) # 빨간선(seasonal+trend+random error가 포함)
lines(decomp.out$trend, col=3)

## 90~ 현재까지 국가별 월 수입액
income <- read.csv("income.csv", header = T)
head(income)

income.ts <- ts(income$계, start = c(1990,1), frequency=12)

plot(income.ts, type="l", ylab="income", xlab="Year")

lines(lowess(income.ts), col=4, lwd=1.5) # 파란색
lines(lowess(income.ts, f=1/3), col=2, lwd=1.5) # 빨간색
lines(lowess(income.ts, f=1/6), col=3, lwd=1.5) # 녹색

trend <- lowess(income.ts, f=1/6)
plot(income.ts - trend$y) ## 뒤로 갈 수록 변동성이 커짐
abline(0,0,col=2) # 선그리기(a+bx = y의 선) 

# log를 취함
log.income <- log(income.ts)
plot(log.income)

# 시계열 분해
decomp.income <- decompose(log.income)
plot(decomp.income$x)
lines(decomp.income$trend, col=2) # trend

# 큰 잡음이 있는 경우는 trend나 계절성으로도 잡을 수 없는 것으로 보임
plot(decomp.income$random) 

plot(decomp.income$x-decomp.income$random) # 계절성과 트렌드만 나타낸 그래프

## 자기상관계수는 서로 독립이어야 함 그렇기 때문에 0이 나오는게 맞는것임. 
## 회귀분석에서의 잔차분석 시 패턴이 없이 띠 모양으로 있는 경우 괜찮은 잔차도
## durbin-watson test : 잔차에 자기상관이 있는지 테스트

library(MASS)
attach(geyser)

acf(waiting)
acf(duration)

## 한번 waiting 혹은 duration이 길면 다음번은 짧음
## 현재시점과 다음 시점의 상관계수를 확인
## 파란점선은 정확도의 범위로 생각(이 안에 들어오게 되면 상관관계가 없다고 보면 됨)


## 교차상관함수
## 지금의 내 자료와 과거의 다른 자료의 상관계수를 나타냄
ccf(waiting, duration)
## lag= 0, 지금의 분출량과 지금의 분출되기까지의 시간은 음의 상관관계
## lag= 1, 지금의 분출량과 한시점 분출되기까지의 시간은 양의 상관관계

## 임의보행(random walk)
## 자기자신하고는 상관계수가 크지만 그 다음부터는 그 방향이 random 하게 나타나는 경우

## 삼성주가
samsung <- read.csv("samsung.csv", header=T)
head(samsung)

plot(samsung$종가, type="l")
## 차분한 값(Yt - Yt-1 = at, Yt = Yt-1+at)
plot(diff(samsung$종가) , type = 'l')
acf(diff(samsung$종가)) # 차분한 값의 자기상관함수(어제의 종가와 오늘의 주가 증가 폭이 상관관계가 없음)

## arima 모형의 가정은 자기상관은 시점에만 의존해야 함(정상성) -> 그렇기 때문에 자기상관함수를 통해 확인을 함.
## 시계열 예측을 위한 모형이 arima 모형

####################################### 범주형 변수 시각화 ########################################
library(RColorBrewer)
library(dplyr)
area <- c("Statistics", "Design", "Business", "Cartography", "Information Science", "Web Analytics", "Programming", "Engineering", "mathmematics", "Others")
votes <- c(172,136,135,101,80,68,50,29,19,41)

names <- paste(area,", ",round(votes/sum(votes)*100,2),"%", sep="")
pie(votes, labels = names, col=brewer.pal(10,"RdGy"))

sales <- read.csv("sales.csv", header = T)
head(sales)

tab1 <- table(sales$CT1)
tab1 <- sort(tab1, decreasing = TRUE)

sales_names <- paste(names(tab1), ": ",round(tab1/sum(tab1)*100,2),"%", sep="")
pie(tab1, labels = sales_names, col=brewer.pal(11,"RdBu"), main="카테고리 구매내역(%)")

## 2원 빈도표
## 2개의 범주형 변수의 결합 빈도
UCBAdmissions
str(UCBAdmissions)
# table [1:2, 1:2, 1:6] 512 313 89 19 353 207 17 8 120 205 ...
# - attr(*, "dimnames")=List of 3
# ..$ Admit : chr [1:2] "Admitted" "Rejected"
# ..$ Gender: chr [1:2] "Male" "Female"
# ..$ Dept  : chr [1:6] "A" "B" "C" "D" ...


tab1 <- UCBAdmissions[1,,] # Admit
tab2 <- UCBAdmissions[2,,] # Reject
tab <- tab1+tab2 # 총 지원자 수(Admit + Reject)

par(mar=c(6.5, 4,4,6),xpd=T) # margin 조정
barplot(tab, ylim = c(0,1000))
legend(7.5,900, legend=rownames(tab), fill=grey(c(0.3,0.9)), yjust=1)

## 비율계산
tab_rate <- tab/matrix(1,2,1) %*% apply(tab, 2, sum)

barplot(tab_rate, names.arg = names(tab_rate), main="남/녀 지원자 수 비율")
legend(7.5,1, legend=rownames(tab), fill=grey(c(0.3,0.9)), yjust=1)

## 남자 전체와 여자 전체의 그래프
barplot(t(tab), beside = T, main="과별 지원한 성별 비교")

## 합격률 비교
tab.m <- UCBAdmissions[,1,] # 남자의 합격률
tab.f <- UCBAdmissions[,2,] # 여자의 합격률

apply(tab.m,1,sum)/sum(tab.m)
apply(tab.f,1,sum)/sum(tab.f)

tab.m.c <- tab.m/(matrix(1,2,1) %*% apply(tab.m,2,sum))
tab.f.c <- tab.m/(matrix(1,2,1) %*% apply(tab.f,2,sum))

par(mfcol=c(1,2))
barplot(tab.m.c, main="Male")
legend(7.5,1,legend=rownames(tab.m.c), fill=grey(c(0.3,0.9)), yjust=1)

barplot(tab.f.c, main="FeMale")
legend(5,1, legend=rownames(tab.f.c), fill=grey(c(0.3,0.9)), yjust=1)

mosaicplot(~Dept+Gender, data= UCBAdmissions) # 과 안에서 남녀 비율 
mosaicplot(~Gender+Dept, data= UCBAdmissions) # 성별 안에서 과 비율 확인
mosaicplot(~Dept+Admit, data=UCBAdmissions) # 과 안에서 합격률 확인
mosaicplot(~Gender+Dept+Admit, data=UCBAdmissions)  # 성별 중에서 각 과의 합격/불합격 확인

#### sales 구매지역별 대분류 종류 확인(2원빈도표)
