#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# outlier 계산
library(tidyverse)
filter.outlier <- function(df) {
    q1 = quantile(df$hwy, .25)
    q3 = quantile(df$hwy, .75)
    iqr = q3 - q1
    upper = q3 + 1.5 * iqr
    lower = q1 - 1.5 * iqr

    df %>% filter(hwy < lower | hwy > upper)
}

mpg <- mpg %>% mutate(name = row_number())

mpg_outlier <- mpg %>%
    group_by(class) %>%
    do(filter.outlier(.)) # do : 어떤 함수를 수행하라 / summarise 의 일반화


mpg %>% group_by(class) %>% do((.) %>% arrange(desc(hwy)) %>% head(3))

ggplot(mpg, aes(x = class, y = hwy)) +
    geom_boxplot() + geom_text(data = mpg_outlier, aes(label = name))

library(nycflights13)

# 날짜 패키지
library(lubridate)
daily = flights %>%
    group_by(month, day) %>%
    mutate(date = make_date(year, month, day), count = n())

ggplot(daily, aes(x = date, y = count)) +
geom_line() +
geom_smooth()

# 시계열
# -> trend (추세)
# -> seasonality(계절성 - 일정주기 반복)
# -> auto correlation (자기상관 계수) ex) 어제의 주가가 오늘의 주가에 영향을 미친다.

# 날짜(lubridate 패키지 관련)
today()
now()
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

ymd(20170131)

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

ymd(20170131, tz = 'UTC')

make_datetime_100 <- function(year, month, day, time) {
    make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>%
  ggplot(aes(dep_time)) +
geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

td = ymd('2017-01-22')
yday(td)
wday(td)

flights_dt %>%
mutate(wday = wday(dep_time, label = TRUE)) %>%
ggplot(aes(x = wday)) +
geom_bar()

flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
geom_line()

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# R for Data Science part22
library(modelr)
ggplot(sim1) + geom_point(aes(x, y))

models <- data.frame(
    a1 = runif(250, -20, 40),
    a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1 / 4) +
geom_point()

model1 <- function(a, data) {
    a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
    diff <- data$y - model1(mod, data)
    sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
    measure_distance(c(a1, a2), sim1)
}

models <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

models

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(sim1, aes(x, y)) +
geom_point() +
geom_abline(aes(intercept = a1, slope = a2, color = dist), data = models %>% arrange(dist) %>% head(10), alpha = 1 / 4, size = 3)

ggplot(models) +
geom_point(aes(a1, a2, color = dist), size = 4)

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

#
lm(price ~ carat, diamonds) %>% summary()
# -> R-squared 가 0~1 사이값이 나온다.
# -> Adjusted R squared : 다항회귀에서 설명력을 얘기할때 사용

lm(price ~ 0 + carat, diamonds) %>% summary() # intercept를 0 으로 고정(절편을 0)
ggplot(diamonds) + geom_point(aes(carat, price), alpha = .01)

lm(price ~ carat + z + carat:z, diamonds) %>% summary() # formula 에서 제곱 및 곱하기는 상호작용(= 두 변수가 있을때 두 변수의 곱간의 영향확인)
# 위의 lm은 다음과 같다
lm(price ~ carat * z, diamonds) %>% summary() # formula 에서 제곱 및 곱하기는 상호작용(= 두 변수가 있을때 두 변수의 곱간의 영향확인)

# 조절(moderation) 의 효과
# -> ex) 학생의 성적 = 소득  + 학교역량:소득
# -> 하나의 현상에 대해 특정 요인뿐만 아니라 다른 요인도 같이 작용하는지 확인할 때 사용

lm(price ~ I(carat ^ 2 )+ carat + z, diamonds) %>% summary() # 회귀식 변경하기 위해 I 사용

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# linear model

library(caret)
d <- read.csv('automobile.csv')
m = lm(symboling ~ width, data = d)
m
ggplot(d, aes(width, symboling)) + geom_point() + geom_smooth(method = 'lm')
symboling.p = predict(m, d)
ggplot() + geom_point(aes(symboling.p, d$symboling), alpha = .2) + geom_abline(slope = 1)

# 날짜 데이터의 경우 날짜로 data를 나누게 되면 모델 평가시 더 좋은 결과를 얻을 수 있다.
# 데이터의 양에 따라 partition 비율을 정함
index <- createDataPartition(
  y = d$symboling,
  p = 0.6,
  times = 1,
  list = F)

train = d[index,]
test = d[ - index,]

dim(train)
dim(test)

m = lm(symboling ~ width + length + engine_size, data = train)
mean((train$symboling - predict(m, train)) ^ 2) # 잔차
mean((test$symboling - predict(m, test)) ^ 2) # 예측

# 변수 선택
#  -> 학습할 수 없는 변수를 빼고 수행
names(d)

features = c("symboling", "fuel", "aspiration", "doors", "body", "wheels", "wheel_base",
             "length", "width", "height", "curb_weight", "engine_size",
             "bore", "stroke", "compression_ratio", "horsepower", "peak_rpm",
             "city_mpg", "highway_mpg", "price")

train = d[index, features]
test = d[ - index, features]

# hyperparameter 설정 : 여러가지 option을 통해 crossvalidation 
# k-fold cross validation : k 수 만큼 random하게 데이터를 나눔(train data/validation data)
# repeat : k-fold를 통해 나눈 데이터를 다시 나눔

controlObject <- trainControl(
  method = 'repeatedcv',
  repeats = 1,
  number = 5)

# elastic net
elst.Grid <- expand.grid(   # vector의 조합을 만든다
  .alpha = seq(0, 1, 0.2), # 0~1 까지 0.2 간격
  .lambda = c(0.001, 0.01, 0.1))    # 0.001 ~ 0.01 까지 0.1 간격

# LM의 정규화
# -> lasso 최소화(L1) : |W|^1을 최소화 / 회귀계수를 0으로 / 변수 선택의 기능이 있음
# -> Ridge 최소화(L2) : |W|^2을 최소화 / W를 부드럽게 줄여줌 / lasso보다 예측력이 좋음
# -> elastic net : L1+L2 / lambda 와 alpha 값이 있음 / lambda : 정규화의 강도(모델 단순화) / alpha : 0~1 사이 값(L1과 L2의 수행 비율 결정)

elst.Model <- train( # caret::train function 
  symboling ~ .,
  data = train,
  method = 'glmnet',
  tuneGrid = elst.Grid,
  preProc = c('center', 'scale'), # 고정값
  metric = 'RMSE', # 예측모델은 RMSE를 최적화
  trControl = controlObject)

elst.Model # RMSE가 최소값인 것을 고른다

best = as.numeric(rownames(elst.Model$bestTune))
elst.Model$finalModel$beta[, best]

y = predict(elst.Model, test, na.action = na.pass)
mean((test$symboling - y) ^ 2, na.rm = T)

# knn : 특정 점에서 다른 점까지의 거리의 최소값을 예측분류
knn.Model <- train(
  symboling ~ .,
  data = train,
  method = 'knn',
  preProc = c('center', 'scale'),
  metric = 'RMSE',
  tuneGrid = data.frame(.k = 2:5), # 2,3,4,5 4가지
  trControl = controlObject)

knn.Model

# svm
# -> linear regression 과 흡사
# -> 좋은 형태를 찾는데 중점
# -> 분류하는 경계선 중 margin이 가장 큰 경계선을 찾는 것
# -> 예외 케이스의 경우는 penalty를 줌
# -> kernal trick : kernal(데이터 간의 유사성을 계산)을 적용 / 원형 데이터의 분류 시 다른 공간으로 옮긴 것 처럼 보이게 하는 것 

library(kernlab)
sigmaRange <- sigest(as.matrix(train[8:20])) # 연속변수만 가능

svm.Grid <- expand.grid(
  .sigma = sigmaRange[1], # RBF의 특성을 결정
  .C = 2 ^ (seq(-5, 5, 2))) # penalty를 결정(크기가 커질수록 모델의 복잡성이 높아짐)

svm.Model <- train(
  symboling ~ .,
  data = train,
  method = 'svmRadial', # RBF 커널
  tuneGrid = svm.Grid,
  preProc = c('center', 'scale'),
  metric = 'RMSE',
  trControl = controlObject)

test.c = test[complete.cases(test),]

svm.Model
y = predict(svm.Model, test)
mean((test.c$symboling - y) ^ 2)

# Neural Network(NN)
nnet.Grid <- expand.grid(
  .decay = c(0.001, 0.01, 0.1), # lambda 와 같음
  .size = seq(3, 11, by = 2), # hidden layer
  .bag = FALSE) # 모델마다 다른 데이터를 줘 평균을 내서 예측률을 높이는 것

nnet.Model <- train(
  symboling ~ .,
  data = train,
  method = 'avNNet',
  tuneGrid = nnet.Grid,
  preProc = c('center', 'scale'),
  maxit = 200, # 모델수행 횟수
  trControl = controlObject)

# decision tree
rpart.Model <- train(
  symboling ~ .,
  data = train,
  method = 'rpart',
  tuneLength = 10,
  trControl = controlObject)

rpart.Model

y = predict(rpart.Model, test)
mean((test.c$symboling - y) ^ 2)

# random forest
rf.Grid <- expand.grid(.mtry = c(4, 6, 8, 10))

rf.Model <- train(
  symboling ~ .,
  data = train,
  method = 'rf',
  tuneGrid = rf.Grid,
  ntrees = 10,
  importance = T,
  trControl = controlObject)

y = predict(rf.Model, test)
mean((test.c$symboling - y) ^ 2)

# gradient boosting tree
# -> 데이터를 통해 tree 하나를 생성
# -> 실제 = 예측1+오차1
# -> 오차1 = 예측2 + 오차2
# -> 오차2 = 예측3 + 오차3 .. 이런식으로 치환

allResamples <- resamples(
  list('Elastic Net' = elst.Model,
       'K-NN' = knn.Model,
       'svm' = svm.Model,
       'Neural network' = nnet.Model,
       'tree' = rpart.Model,
       'Random Forest' = rf.Model))
summary(allResamples)


#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■