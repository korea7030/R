#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# R Data for Science part3(ggplot)
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy))

# class 별 색깔 지정
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy, color = class))

# class 별 크기 지정
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy, size = class))

# 투명도 지정
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# 모양 지정
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# 색 지정
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# class 별 색 지정
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy, color = class)) # aes 안에 쓸 경우는 데이터와 매핑을 할 때 쓴다.

# facet 
# 각각의 클래스 별로 그래프 출력
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
facet_wrap( ~ class, nrow = 2) # ~의 앞은 주로 y(세로), 뒤는 x(가로) 

# geometric 
# smooth : 회귀식을 나타낼 경우 사용
ggplot(data = mpg) +
geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv), se = F) # linetype : 선 종류 , se : 표준에러(잔차)를 나타냄(회색부분)

# 여러 그래프
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
geom_smooth(mapping = aes(x = displ, y = hwy), se = F)

# 클래스 색깔 별 그래프
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
geom_smooth()

# geom_bar : y축을 지정 안하면 x변수에 대한 count 
ggplot(data = diamonds) +
geom_bar(mapping = aes(x = cut))

# y를 지정하는경우
demo <- tribble(
  ~ a, ~ b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)

ggplot(data = demo) +
geom_bar(mapping = aes(x = a, y = b), stat = "identity") # identity를 지정하면 y에 대한 그래프를 그려라 라는 의미

# position 
ggplot(data = diamonds) +
geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") # 누적 형태

ggplot(data = diamonds) +
geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") # clarity 각각을 나타내는 막대그래프로

# jitter : 겹쳐보이는 그래프를 안겹치게
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# R Data for Science part5(dplyr)
library(tidyverse)
library(nycflights13)
summary(flights)
colnames(flights)

# filter : 검색
filter(flights, month == 1)
# 연산자
#-> 연산자가 하나일 경우(vector 간의 연산 수행 시 한개 사용)
c(1, 2, 3) > 2 | c(4, 5, 6) <= 4
#-> 연산자가 두개일 경우(vector 간의 연산이 아닌 값간의 연산, 벡터로 비교하면 각 벡터당 첫번째 값들만 연산 수행)
c(1, 2, 3) > 2 || c(4, 5, 6) <= 4
# NA : 없는 값/모르는 값

## ex1
filter(flights, arr_delay >= 120)

## ex2
head(flights$dest)
filter(flights, dest %in% c('IAH', 'HOU'))

## ex3
filter(flights, month %in% c(7,9))

## ex4
filter(flights, carrier %in% c('UA', 'DL', 'AA'))

## ex5
filter(flights, arr_delay > 120 & dep_delay <= 0)

## ex6
filter(flights, dep_delay >= 60 & arr_delay < dep_delay - 30)

## ex7
filter(flights, dep_time <= 600)

# arrange :정렬
arrange(flights, desc(arr_delay)) # desc : 내림차순

# select : 컬럼선택 
select(flights, year, month, day)
select(flights, year:day) # year~day 컬럼까지
select(flights, - (year:day)) # year~day 뺀 나머지

# -> starts_with : 변수의 이름 시작이 특정 문자열로 시작하는 것만 찾기
select(flights, starts_with('dep'))
# -> end_with : 변수의 이름 끝이 특정 문자열로 끝나는 것만 찾기
select(flights, ends_with('delay'))
select(flights, starts_with('dep'), ends_with('delay'))
# -> contains : 변수명에 특정 문자가 들어가있는 것 모두 찾기
select(flights, contains('d'))
# -> matches : 정규표현식을 활용한 찾기
select(flights, matches('d[aeiou]'))
# -> num_range : 특정 문자열의 변수명이 순번형태로 반복될 경우 사용
# -> rename : column명을 변경
select(flights, tail_num = tailnum)
# -> everything() : 모든컬럼 선택
select(flights, time_hour, air_time, everything())
# -> mutate : 새로운 컬럼 추가
mutate(flights, gain = arr_delay - dep_delay, speed = distance / air_time * 60))
# -> transmute : 새로운 컬럼만 나타내기
transmute(flights, gain = arr_delay - dep_delay, speed = distance / air_time * 60)

## pipe or chain (%>%)
filter(flights, month == 1) %>% arrange(day)
flights %>% 
    filter(month == 1) %>% 
    arrange(day)

## summarise : 통계 요약
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

## group_by : 그룹핑 
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm=TRUE))

flights %>% 
    group_by(year, month, day) %>% 
    summarise(delay = mean(dep_delay, na.rm = TRUE), count=n())

## n() : summarise 상에서 count 역할하는 함수
## upgroup() : group_by() 로 묶은 data의 그룹핑을 풀 때 사용
## slice() : row 선택 함수
## distinct() : 중복제거
## sample_n() : random 뽑기
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# R Data for Science part7(EDA)
## 탐색적 데이터분석(EDA) vs 확인적 데이터분석(CDA)
# -> EDA : 모델이 없음/데이터 검색 - 그래프/극단값 찾기
# -> CDA : 회귀분석, t-test, ANOVA ... / 모델 수행
# boxplot : 분포 확인 및 극단값 보기, 최근에는 잘 안쓰임., 히스토그램을 그리는게 나음

# boxplot의 outlier
ggplot(mpg, aes(class, hwy)) +
geom_boxplot() + # outlier 의 점을 투명하게
    geom_text(aes(data=filter(mpg, check_outlier(class, hwy)), label = rownames(mpg$hwy)))


summary(mpg)
limit <- mpg %>%
    group_by(class) %>%
    summarise(median = median(hwy),
        first = quantile(hwy, .25),
    third = quantile(hwy, .75)) %>%
mutate(IQR = third - first,
        upper = third + 1.5 * IQR,
        lower = first - 1.5 * IQR)

check_outlier = function(cls, value) {
    row = limit %>% filter(class == cls)
    return (value > row$upper[1] | value < row$lower[1])
}

## 집합 간의 차이 
setdiff(c(1, 2, 3), c(3, 4, 5))
## 비교하기
flights = flights %>% mutate(name= row_number())

flights$name = rownames(flights)
aa <- filter(flights, carrier == 'AA')
july <- filter(flights, month == 7)

 i <- setdiff(aa$name, july$name)
flights[i,]
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
