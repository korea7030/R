jobs <- read.csv("2000-2013년 연령별실업율_연령별평균_세로.csv")

str(jobs)

x20 <- jobs[,2]
x30 <- jobs[,3]
x40 <- jobs[,4]
x50 <- jobs[,5]
x60 <- jobs[,6]

par(new=T)
plot(x20, xlab="", ylab="", ylim=c(0,11), axes=FALSE, col="violet", type="o", lwd=2, main="연령별 실업률 현황(단위 : %) 출처:통계청")

axis(1, at=1:14, label=colname, las=2)
axis(2,las=1)

lines(x30, col="blue", type="o", lwd=2)
lines(x40, col="red", type="o", lwd=2)
lines(x50, col="black", type="o", lwd=2)
lines(x60, col="orange", type="o", lwd=2)

abline(h=seq(0,10,0.5), v=seq(1,14, 1), lty=2, lwd=0.2)

col<- names(jobs[2:6])
col2 <- gsub("X","", col)
colors <- c("violet", "blue","red","black","orange")
legend(12,11,col2, cex=0.8, col=colors, lty=1, lwd=2, bg="white")
savePlot("total.png", type="png")
