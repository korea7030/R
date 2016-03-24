## google차트 
install.packages("googleVis")
library(googleVis)

google_data <- read.csv("./sample/2013년_서울_구별_주요과목별병원현황_구글용.csv", header=T)

?googleVis
vignette("googleVis")

plot(gvisColumnChart(google_data, options=(list(title="지역별 병원현황", height=400, weight=500))))

########################################

## 의원 청구현황
count <- read.csv("./sample/연도별요양기관별보험청구건수_2001_2013_세로.csv", stringsAsFactors = FALSE)

head(count)

colname <- count$년도

v1 <- count[,2]/100000
v2 <- count[,3]/100000
v3 <- count[,4]/100000
v4 <- count[,5]/100000
v5 <- count[,6]/100000
v6 <- count[,7]/100000
v7 <- count[,8]/100000
v8 <- count[,9]/100000
v9 <- count[,10]/100000
v10 <- count[,11]/100000

plot(v1, xlab="", ylab="", ylim=c(0,6000), axes=FALSE, col="violet", type="o", lwd=2, main=paste("연도별 요양 기관별 보험 청구 건수(단위:십만건)", "\n", "출처:건강보험심사평가원"))

axis(1,at=1:10, label=colname, las=2)
axis(2, las=1)

drawLine <- function(data, colName) {
  lines(data, col=colName, type="o", lwd=2)
}

drawLine(v2, "blue")
drawLine(v3, "red")
drawLine(v4, "black")
drawLine(v5, "orange")
drawLine(v6, "green")
drawLine(v7, "cyan")
drawLine(v8, "yellow")
drawline(v9, "brown")
drawLine(v10, "pink")
###########################################

## 의원제외한 청구현황
count2 <- read.csv("./sample/연도별요양기관별보험청구건수_2001_2013_세로_의원제외.csv", stringsAsFactors=FALSE)

colname <- count2$년도

v1 <- count2[,2]/10000
v2 <- count2[,3]/10000
v3 <- count2[,4]/10000
v4 <- count2[,5]/10000
v5 <- count2[,6]/10000
v6 <- count2[,7]/10000
v7 <- count2[,8]/10000
v8 <- count2[,9]/10000
v9 <- count2[,10]/10000


head(count2)

plot(v1, xlab="", ylab="", ylim=c(0,10000), axes=FALSE, col="violet", type="o", lwd=2, main=paste("연도별 요양 기관별 보험 청구 건수 - 의원제외(단위:십만건)", "\n", "출처:건강보험심사평가원"))

axis(1,at=1:10, label=colname, las=2)
axis(2, las=1)


drawLine2 <- function(data, colName) {
  lines(data, col=colName, type="o", lwd=2)
}

drawLine2(v2, "blue")
drawLine2(v3, "red")
drawLine2(v4, "black")
drawLine2(v5, "orange")
drawLine2(v6, "green")
drawLine2(v7, "cyan")
drawLine2(v8, "yellow")
drawLine2(v9, "brown")

abline(h=seq(0,10000,1000), v=seq(1,100,1), lty=3, lwd=0.2)

col <- names(count[1,2:10])
colors <- c("violet", "blue", "red","black","orange", "green", "cyan", "yellow", "brown")
legend(1,10000,col,cex=0.8, col=colors, lty=1, lwd=2, bg="white")
?legend

#### 년도별 기관별 보험청구금액 현황 그래프 

data3 <- read.csv("./sample/연도별요양기관별보험청구금액_2004_2013_세로.csv", stringsAsFactors=FALSE)

head(data3)

colname <- data3$년도

v1 <- data3[,2]/1000000
v2 <- data3[,3]/1000000
v3 <- data3[,4]/1000000
v4 <- data3[,5]/1000000
v5 <- data3[,6]/1000000
v6 <- data3[,7]/1000000
v7 <- data3[,8]/1000000
v8 <- data3[,9]/1000000
v9 <- data3[,10]/1000000
v10 <- data3[,11]/1000000


plot(v1, xlab="", ylab="", ylim=c(0,10000), axes=FALSE,  type="o", lwd=2, main=paste("연도별 요양 기관별 보험 청구 금액(단위:십만건)", "\n", "출처:건강보험심사평가원"))

axis(1,at=1:10, label=colname, las=2)
axis(2, las=1)


drawLine3 <- function(data, colName) {
  lines(data, col=colName, type="o", lwd=2)
}

drawLine3(v2, "blue")
drawLine3(v3, "red")
drawLine3(v4, "black")
drawLine3(v5, "orange")
drawLine3(v6, "green")
drawLine3(v7, "cyan")
drawLine3(v8, "yellow")
drawLine3(v9, "brown")
drawLine3(v10, "navy")

abline(h=seq(0,10000,1000), v=seq(1,100,1), lty=3, lwd=0.2)

col <- names(data3[1,2:10])
colors <- c("violet", "blue", "red","black","orange", "green", "cyan", "yellow", "brown")
legend(1,10000,col,cex=0.8, col=colors, lty=1, lwd=2, bg="white")
