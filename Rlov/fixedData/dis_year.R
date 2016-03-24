data_year <- read.csv("1군전염병발병현황_년도별.csv", sep=",", stringsAsFactors=FALSE)

head(data_year)

colname <- data_year$년도별

vdis_1 <- data_year$A형간염
vdis_2 <- data_year$콜레라
vdis_3 <- data_year$장티푸스
vdis_4 <- data_year$이질
vdis_5 <- data_year$대장균

head(vdis_5)

plot(vdis_1,  xlab="", ylab="", ylim=c(0,6000), axes=FALSE, col="violet", type="o", lwd=2, main="1군 전염병 발병현황-년도별(단위:건수) 출처:통계청")

axis(1, at=1:11, label=colname, las=2)
axis(2, las=1)

lines(vdis_2, col="blue", type="o", lwd=2)
lines(vdis_3, col="red", type="o", lwd=2)
lines(vdis_4, col="black", type="o", lwd=2)
lines(vdis_5, col="orange", type="o", lwd=2)

abline(h=seq(0,6000,100),v=seq(1,11,1), lty=2, lwd=0.2)

names <- names(data_year[2:6])

colors <- c("violet", "blue","red","black", "orange")
legend(1,6000, names, cex=0.8, col=colors, lty=1, lwd=2, bg="white")
