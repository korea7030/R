rm(list=ls())

loc <- read.csv("서울시구청위치정보_new.csv", header=T, sep=",")
loc

hoffice <- gvisMap(loc, "LATLON", "name", options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE, mapType='normal', useMapTypeControl=TRUE, width=1000, height=400))

plot(hoffice)


#######################################################

install.packages("ggmap")
library(ggmap)

ft_placelst <- function(place) {
  temp <- geocode(place)  ## 위도 경도를 임시변수에 지정
  place <- gsub("제주 ", "", place) 
  df_lst <- cbind(place,temp) ## 여행지 이름, 위도, 경도인 dataframe생성
  
  return (df_lst)
}
df_placelst <- ft_placelst(readLines("제주여행코스.txt"))

head(df_placelst)

latlong <- paste(df_placelst$lat, ":", df_placelst$lon)
latlong <- gsub(" ","", latlong)

df_placelst <- cbind(df_placelst, latlong)

df_placelst

placePath <- gvisMap(df_placelst, "latlong", "place", options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE, mapType="hybrid", useMapTypeControl=TRUE, width=800, height=1000))

plot(placePath)

###########################################################

CityPopularity

ex1 <- gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500, greenTo = 800, yellowFrom=300, yellowTo=500, redFrom = 0, redTo = 300, width=400, height=300))
plot(ex1)

#############################################################

fruits <- data.frame(month=c("JAN", "FEB", "MAR"), 
                    apple=c(30,10,20), 
                    orange=c(20,40,30))

line <- gvisLineChart(fruits)
plot(line)

## 양쪽에 y축 표시
line2 <- gvisLineChart(fruits, "month", 
                       c("apple", "orange"), 
                       option=list(series="[{targetAxisIndex:0}, {targetAxisIndex:1}]", 
                                   vAxes="[{title:'apple'},{title:'orange'}]"))

plot(line2)

## 선모양 변경
line3 <- gvisLineChart(fruits, xvar="month", yvar=c("apple", "orange"), options=list(series="[{color:'blue', targetAxisIndex:0, linewidth:1, lineDashStyle:[1,10,1,10,1,10]}, {color:'red', targetAxisIndex:1, linewidth:2, lineDashStyle:[4,1]}]", vAxes="[{title:'apple'},{title:'orange'}]"))

plot(line3)

##########################################################

## google Bar Chart ##

gbar <- gvisBarChart(fruits)

plot(gbar)


##########################################################
## google pie chart

pie1 <- gvisPieChart(CityPopularity, options=list(width=400, height=300))

plot(pie1)

#########################################################
## google candle stick chart

candle <- gvisCandlestickChart(OpenClose, options=list(legend='none', height=400, width=500))

plot(candle)

########################################################
## google bubble chart 

a <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses", colorvar="Year", sizevar="Profit", options=list(height=500, weight=800, hAxis='{minValue:75, maxValue:125}'))

plot(a)
########################################################
## google column chart

korean <- read.csv("학생별회차별성적__국어_new.csv", header=T)

kor <- gvisColumnChart(korean
                    ,options=list(title="학생별 성적비교", height=400, weight=500))

plot(kor)

#######################################################
## google area chart

area <- gvisAreaChart(korean, options=list(height=400, weight=500))

plot(area)

#######################################################

## google combo chart

combo <- gvisComboChart(korean, options=(seriesType = "bars" , 
                                         height=400, weight=500, series='{3 : {type:"line"}}'))

########################################################
## 계층도  gvisOrgChart

name <- c('Angela Bassett', 'Jessica Lange', 'Winona Ryder', 'Michelle Pfeiffer')
pemp <- c(NA, 'Angela Bassett', 'Jessica Lange','Winona Ryder')

pay <- c(100,200,300,400)

emp <- data.frame(NAME=name, PNAME=pemp, PAY=pay)

Org <- gvisOrgChart(emp, idvar="NAME", parentvar="PNAME", options=list(width=600, height=250, size='middle', allowCollapse=TRUE))

plot(Org)
