install.packages("dplyr") 
install.packages("hflights")

library(dplyr)
library(hflights)

## 휴스턴에서 출발하는 비행기의 이착륙 기록 수(2011년)
dim(hflights)

hflights
str(hflights)

write.csv(hflights, file = "hflights.csv")

## dataframe을 console에 좀더 보여주기 쉽게 하기 위함
hflights_df <- tbl_df(hflights)
hflights_df

# 1월 1일 데이터추출
# filter(dataframe명, 조건1, 조건2)
filter(hflights_df, Month == 1, DayofMonth == 1)

# 1월 혹은 2월 데이터 추출
a <- filter(hflights_df, Month == 2 | Month == 1)
tail(a)

# 데이터를 ArrDelay, Month, Year로 정렬
b <- arrange(hflights_df, ArrDelay, Month, Year)

write.csv(b, file = "arrange.csv")

# Month의 큰 값으로 부터  작은값 순으로 정렬
arrange(hflights_df, desc(Month))

# Year, Month, DayOfWeek 열을 추출
select(hflights_df, Year, Month, DayOfWeek)

install.packages("sqldf")
library(sqldf)
example(sqldf)
# filter + select 
sqldf("select Year, Month, DayOfWeek from hflights_df where Month = 1 and DayOfWeek = 1")

sqldf("select DepDelay, case WHEN DepDelay = '0' THEN 'F' else DepDelay end from hflights_df where DepDelay = 0")
# Year부터 DayOfWeek 까지 추출
select(hflights_df, Year:DayOfWeek)

# Year부터 DayOfWeek를 제외한 나머지 열 추출
select(hflights_df, -(Year:DayOfWeek))

# 생성된 열 gain을 gain_per_hour의 계산에 사용할 수 있음
mutate(hflights_df, gain=ArrDelay - DepDelay, gain_per_hour = gain/(AirTime/60))

# 평균 출발지연시간 계산 
summarise(hflights_df, delay = mean(DepDelay, na.rm=TRUE))

filter(hflights_df, DepDelay == 0)
select(hflights_df, DepDelay)


# 비행편수가 20편 이상, 평균비행거리 2000마일 이상인 항공사별 평균 연착시간을 표현
planes <- group_by(hflights_df, TailNum)
delay <- summarise(planes, count=n(), dist=mean(Distance, na.rm=TRUE), delay = mean(ArrDelay, na.rm=TRUE))
delay1 <- summarise(planes, count=n())
delay <- filter(delay, count > 20, dist < 2000)

library(ggplot2)
ggplot(delay, aes(dist, delay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth()+scale_size_area()
