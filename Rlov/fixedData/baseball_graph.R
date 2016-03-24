BaseBall <- read.csv("./sample/주요선수별성적-2013년.csv")
head(BaseBall)

bp <- barplot(BaseBall$연봉대비출루율, main=paste("선수별 연봉대비 출루율 분석","\n","(밥값여부계산)"), col=rainbow(25),  names.arg=BaseBall$선수명, ylim=c(0,50))
title(ylab="연봉대비출루율", col.lab="red")

aver<- 0

for(i in 1:length(BaseBall$연봉대비출루율)) {
  aver <- aver + BaseBall$연봉대비출루율[i]
}

aver

aver <- aver/length(BaseBall$연봉대비출루율)
aver

abline(h=aver, col="blue")
text(x=aver-11, y=14.5, col="black", cex=0.8, labels=paste(aver,"%","(평균출루율)"))
text(x=bp*1.01, y=BaseBall$연봉대비출루율*1.05, col="black", cex=0.7, labels=paste(BaseBall$연봉대비출루율,"%"))


### 나이팅게일 차트 
row.names(BaseBall) <- BaseBall$선수명

BaseBall2 <- BaseBall[,c(7,8,11,12,13,14,17,19)]
stars(BaseBall2, flip.labels=FALSE, draw.segment=TRUE, frame.plot=TRUE, full=TRUE, main="야구선수별 주요 성적분석-2013년")

label <- names(BaseBall2)
val <- table(label)
color <- c("black", "red", "green", "blue", "cyan", "violet", "yellow", "grey")
## 범례용 그래프
pie(val, labels=label, col=color, radius=0.1, cex=0.6)

BaseBall3 <- BaseBall[,c(2,21,22)]
head(BaseBall3)

line1 <- BaseBall$연봉대비출루율
line2 <- BaseBall$연봉대비타점율
par(mar=c(5,4,4,4)+0.1)
plot(line1, type="o", axes=F, ylab="", xlab="", ylim=c(0,50), lty=2, col="blue", main="한국프로야구선수별 기록분석-2013년", lwd=2)
axis(1, at=1:25, lab=BaseBall$선수명, las=2)
axis(2, las=1)

par(new=T)
plot(line2, type="o", axes=f, ylab="", xlab="", ylim=c(0,50), lty=2, col="red")
axis(4,las=1)
mtext(side=4, line=2.5, "연봉대비 타점율")
mtext(side=2, line=2.5, "연봉대비 출루율")
abline(h=seq(0,50,5), v=seq(1,25,1), col="gray", lty=2)
legend(18,50, names(BaseBall[21:22]), cex=0.8, col=c("red","blue"), lty=1, lwd=2, bg="white")
