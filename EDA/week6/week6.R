eq <- read.table("http://dl.dropbox.com/u/8686172/eq.csv", sep="\t", header = T)
head(eq)

eq <- eq[eq$latitude != "-",]
## 위도,경도 숫자만 나오도록 변경

eq$latitude <- as.character(eq$latitude)
eq$longitude <- as.character(eq$longitude)

eq$latitude <- as.double(unlist(strsplit(eq$latitude, " "))[seq(1,nrow(eq), 2)])
eq$longitude <- as.double(unlist(strsplit(eq$longitude, " "))[seq(1,nrow(eq), 2)])

## date 컬럼을 시간으로 변환

eq$year <- as.numeric(substr(eq$date, 1,4))
eq$date <- as.POSIXct(eq$date, format="%Y-%m-%d %H:%M")
str(eq)

library(ggmap)
library(ggplot2)
ggmap(get_googlemap(center="korea", zoom=6, maptype = "terrain", extent = "device"))+
  geom_point(data = eq, aes(x=longitude, y=latitude, size= power, color = year), alpha = 0.7)+
  geom_density2d(aes(x=longitude, y=latitude), data = eq, alpha=0.5)

## 날짜 변환 
date <- data.frame(x=c(3,1,6,7), date = c("15/06/2014", "17/06/2014", "17/06/2014", "18/06/2014"))
str(date)
date$date <- strptime(as.character(date$date), format = "%d/%m/%Y")

a <- date$date
weekdays(a)
format(a, format = "%Y") # 해당 날짜의 연도 표시
format(a, format = "%m") # 해당 날짜의 월만 표시

strptime("2016 2  월", "%Y %W %a")

difftime(date$date[1], date$date[2], unit = "weeks") # 주 차이를 알고 싶을 때

## 지하철 노선 승하차 인원
subway <- read.csv("subway.csv", header = T)

subway$income_date <- strptime(as.character(subway$income_date), format = "%Y%m%d")

subway$weekday <- format(subway$income_date, "%a")
subway$month <- format(subway$income_date, "%m")
subway$year <- format(subway$income_date, "%Y")
head(subway)

table(subway$year)
unique(subway$month[subway$year == "2014"]) ## 2014 년에 몇월까지 데이터가 있는지
subway2 <- subway[subway$year != "2014", ]
table(subway2$year)

unique(subway2$stat_name)
tmp <- strsplit(as.character(subway2$stat_name), "\\(")
tmp <- sapply(tmp, function(x) x[1])
subway2$stat_name <- tmp

subname <- read.csv("subway_latlong2.csv", header = T)
subname$STATION_NM[442] <- "노량진"

str(subname)
str(subway2)

subway2 <- merge(subway2, subname[,c(2,3,8,9)], by.x = "stat_name", by.y = "STATION_NM")
head(subway2)

subway1_8 <- subway2[subway2$LINE_NUM %in% unique(subway2$LINE_NUM)[c(1:8)],  ]
unique(subway1_8$LINE_NUM)

library(dplyr)
library(plyr)
cum_on_tot <- ddply(subway1_8, .(stat_name), summarize, sum_tot = sum(on_tot), line_num = LINE_NUM[1])
head(cum_on_tot)

cum_on_tot$stat_name <- factor(cum_on_tot$stat_name, levels = cum_on_tot$stat_name[order(cum_on_tot$line_num)])

ggplot(cum_on_tot, aes(x=stat_name, y=sum_tot, fill = line_num))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("탑승객수")+scale_x_discrete("지하철역")+
  scale_fill_discrete(name="노선")

## 상위 10개 역
idx_top10 <- cum_on_tot$sum_tot >= sort(cum_on_tot$sum_tot, decreasing = T)[10]
cum_on_tot[idx_top10,]

sum_on_top10 <- subset(cum_on_tot, subset = idx_top10)

ggplot(sum_on_top10, aes(x=stat_name, y=sum_tot, fill = line_num))+
  geom_bar(stat="identity")+
  ylab("탑승객수")+scale_x_discrete("지하철역")+
  scale_fill_discrete(name="노선")

## 상위 10개역의 2013년 월별 탑승인원
## 2010~2013 년 까지의 월별 누적 탑승인원을 누적그래프로
## 특정 날짜 (2012년 5월 8일의 각 역 별 탑승인원을 표시)

