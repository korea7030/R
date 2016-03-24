bus <- read.csv("버스노선별이용현황합계.csv")

head(bus)

str(bus)

busdata <- data.frame(승차인원수=bus$승차인원, 하차인원수=bus$하차인원)

head(busdata)

busdata2 <- t(busdata)

bplot <- barplot(busdata2/1000, ylim=c(0,310), main="서울 주요 마을 버스 이용 승객 현황(2014년 1월)", col=c("green","yellow"), cex.names=0.7, las=2, ylab="이용 승객수(단위:천명)", xlab="노선명", beside=T, names.arg=bus$버스노선번호)
text(x=bplot, y=busdata2/1000*0.95, labels=busdata2, col="red",cex=0.7)
abline(h=seq(10,350,10), col="white", lty=2, lwd=0.5)

legend(55,300,c("승차인원수","하차인원수"), cex=0.8, col=c("green", "yellow"))

