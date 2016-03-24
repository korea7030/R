data <- read.csv("Balance.csv")
data

str(data)
# 표준편차
sd(data$balance)


x <- seq(-4,4,0.1)

## 정규분포에서의 높이값
plot(x,dt(x,84), 'l')

## t검정을 해주는 함수
t.test(data$balance)

## 99% 신뢰구간
t.test(data$balance, conf.level=0.99)

## 모비율 추정
## x : 성공의 개수
## n : 시도횟수
prop.test(0.53*1500, 1500, correct=FALSE)
## 99% 구간
prop.test(0.53*1500, 1500, correct=FALSE, conf.level=0.99)
?prop.test

## 
x <- c(15.5, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.5, 0, 4.97)
hist(x)
boxplot(x)

## 가정체크 
shapiro.test(x)

t.test(x)
t.test(x, mu=8.1)

## 단측검정 
t.test(x, mu=8.1, alter="greater")


## ### HW2 1번 
movie_MBA <- read.csv("movie_MBA2.csv")
movie_MBA
head(movie_MBA)

#15세 이상 관람가에 대한 데이터
movie_2 <- movie_MBA[movie_MBA$rating=="15세이상관람가", ]

head(movie_2)

## 15세이상 관람가의 관객수
t.test(movie_2$total_seen)

## 가설 검정
shapiro.test(movie_2$total_seen)



##################### 2번 한국에서 개봉한 15세 이상 관람가 영화의 평균이 1500000 보다 크다고 주장한다 사실이냐

## 15세이상 관람가에 대한 T검정
  t.test(movie_2$total_seen, mu = 1500000, alter="greater")



################### 3번 #########################
## 15세
head(movie_2)

## 12세
movie_3 <- movie_MBA[movie_MBA$rating=="12세이상관람가", ]

## 검정 체크
boxplot(movie_2$total_seen)
boxplot(movie_3$total_seen)

head(movie_3)

## 두 집단에 대해 row bind 처리
movie_cat <- rbind(movie_2, movie_3)
str(movie_cat)

## 분산
var.test(movie_cat$total_seen~movie_cat$rating, data=movie_cat)

## 독립표본
t.test(movie_cat$total_seen~movie_cat$rating, data=movie_3 , alter="less")

####################################################

######################### 4번#########################
earing <- read.csv("Earnings.csv")
head(earing)

## 기술통계량
summary(earing$Actual)

## 기술 통계량
summary(earing$Predicted)

## 두 값의 차이 컬럼 추가
earing$diff <- earing$Actual - earing$Predicted
head(earing)

## 문제의 가설검정
shapiro.test(earing$Actual - earing$Predicted)

## 차이에 대한 T검정
t.test(earing$diff)


######################################################

## 단일 집단의 모비율에 대한 검정
prop.test(67,120)

## 두 집단의 모비율간의 검정
prop.test(c(60,120), c(150,250), alter="less")
