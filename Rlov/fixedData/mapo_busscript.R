rm(list=ls())

mapodata <- read.csv("마포09번이용현황.csv", sep=",", stringsAsFactors= FALSE)
head(mapodata)

mapodataframe <- data.frame(승차인원=mapodata$승차인원, 하차인원=mapodata$하차인원)

## 승차인원 밑그림만 
plot(mapodataframe$승차인원, axes=FALSE, xlab="", ylab="", ylim=c(0,42000), main="마포09번 이용 승객수(단위:명) - 2014년 1월 기준",type="o",  col="red")

## x축 label
axis(1,at=1:32, label=mapodata$정류소명, las=2)
## y축(위의 0~42000) 
axis(2, las=1)

## 모눈종이 효과
abline(h=seq(0,40000,1000), v=seq(1,32,1), lty=3, lwd=0.2)

## 하차인원 추가
lines(mapodataframe$하차인원,col="blue",type="o")

## 범례 
legend(22,30000, c("승차인원","하차인원"), cex=0.8, col=c("red","blue"), lty=2, lwd=2, bg="white")


