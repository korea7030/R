install.packages("ggmap")
install.packages("ggplot2")
install.packages("stringr")

library(ggmap)
library(ggplot2)
library(stringr)

# data load
loc <- read.csv("./sample/서울_강동구_공영주차장_위경도.csv", header=T)

head(loc)

# 1. 지도를 가져온다.
# get_map("위치", 크기비율, 맵유형)
# 맵유형 : ("terrain(지역)", "terrain-background(지역배경)", "satellite(위성)", "roadmap(거리지도)", "hybrid(혼합)")
kd <- get_map("Amsa-dong", zoom=13, maptype="hybrid")

# 2. 가져온 지도 위에 loc데이터의 위도(LON), 경도(LAT) point를 찍어준다.
kor.map <- ggmap(kd)+geom_point(data=loc, aes(x=LON, y=LAT), size=3, alpha=1.0, color="red") 
# 3. point의에 text를 loc데이터의 주차장명으로 찍어줌
kor.map+geom_text(data=loc, aes(x=LON, y=LAT+0.001, label=주차장명), size=3)
# 그림 저장
ggsave("kd.png", dpi=500)

## 시,구 구분해서 표시
# 1. 시,구 문자만 뽑는다.
# str_sub(뽑을문자열, 시작위치, 끝위치)
# 시작위치나 끝위치나 -가 붙게되면 뒤에서부터 몇자리를 봅는다는 의미
substr(loc$주차장명, -1,-2 )

?substr
loc2 <- str_sub(loc$주차장명, start=-2, end = -2)
loc2
?str_sub
str_sub(loc$주차장명, start=3, end = -1)

# 2. 색깔을 넣을 color vector생성
colors <- c()



# 3. 구면 red, 시면 blue 처리
for(x in 1:length(loc2)) {
  if(loc2[x] == "구") {
    colors <- c(colors,"red")
  }
  else {
    colors <- c(colors, "blue")
  }
}

# 4. 지도 부르기
kd <- get_map("Amsa-dong", zoom=13, maptype="roadmap")
# 5. 지도위에 포인트 추가
kor.map <- ggmap(kd)+geom_point(data=loc, aes(x=LON, y=LAT), size=3, alpha=0.7, color=colors)
# 6. 지도위 포인트에 명칭추가
kor.map+geom_text(data=loc, aes(x=LON, y=LAT+0.001, label=주차장명), size=3)
######################################################################################

# ##### 지역별 인구현황 
library(grid)

pop <- read.csv("./sample/지역별인구현황_2014_4월기준.csv", header=T)

head(pop)
pop
dim(pop)

lon <- pop$LON
lat <- pop$LAT

data <- pop$총인구수
# 지역위치별 총인구수를 구하기 때문에 데이터 가공
  df <- data.frame(lon,lat, data)
head(df)
# 1. 맵을 가져옴
map1 <- get_map("Korea", zoom=7, maptype="roadmap")
# 2. 가져온 맵을 ggmap으로 올림

map1 <- ggmap(map1)

# 3. ggmap에 point추가(색깔은 총인구수, 크기도 총인구수를 기준으로 표시)
map1+geom_point(aes(x=lon, y=lat, colour=data, size=data), data=df)
###########################################################
## 서울시 구청의 위치 
gooData <- read.csv("./sample/서울시구청위치정보_new.csv", header=T)

head(gooData)

kd <- get_map("seoul", zoom=11, maptype="hybrid")

seoul.map <- ggmap(kd)+geom_point(data=gooData, aes(x=LON, y=LAT), color="green", alpha=1, size=5)
seoul.map+geom_text(data=gooData, aes(x=LON, y=LAT+0.001, label=name), size=5)

#############################################################
## 서울시 장애인도서관과 장난감 도서관의 위치
loc1 <- read.csv("./sample/지역별장애인도서관정보.csv", header=T)

head(loc1)
colnames(loc1)

loc2 <- read.csv("./sample/서울시장난감도서관위치현황.csv", header=T)

map1 <- get_map("seoul", zoom=11, maptype="roadmap")

dic.map <- ggmap(map1)+geom_point(data=loc1, aes(x=LON, y=LAT), color="red", size=4, alpha=0.7) + geom_point(data=loc2, aes(x=LON, y=LAT), color="blue", size=4, alpha=1)

dic.map+geom_text(data=loc1, aes(x=LON, y=LAT+0.001, label=자치구명), size=3)

####################################################################################

## 2,3호선 위치 
subway3 <- read.csv("./sample/서울지하철3호선역위경도정보.csv", header=T)
subway2 <- read.csv("./sample/서울지하철2호선위경도정보.csv", header=T)

map <- get_map("seoul", zoom=11, maptype="roadmap")

sub.map <- ggmap(map) + geom_point(data=subway2, aes(x=LON, y=LAT), color="green", size=3, alpha=0.7) + geom_point(data=subway3, aes(x=LON, y=LAT), color="red", size=3, alpha=0.7)

sub.map + geom_text(data=subway2, aes(x=LON, y=LAT+0.01, label=역명), size=3) + geom_text(data=subway3, aes(x=LON, y=LAT+0.01, label=역명), size=3)

##############################################

jeju <- read.csv("./sample/제주도여행코스_1일차.csv", header=T)

jejumap <- get_map("Hallasan", zoom=11, maptype="hybrid")
jejumap

path.map <- ggmap(jejumap)+geom_point(data=jeju, aes(x=LON, y=LAT), color="red", size=3, alpha=0.5)

path.map +geom_text(data=jeju, aes(x=LON, y=LAT+0.01, label=장소), size=3) + geom_path(data=jeju, aes(x=LON, y=LAT), size=1, linetype=2, col="blue")
