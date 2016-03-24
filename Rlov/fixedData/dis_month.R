dis_month <- read.csv("1군전염병발병현황_월별.csv", sep=",", stringsAsFactors=FALSE)

head(dis_month)

yearnames <- dis_month$월별

vdism_1 <- dis_month$A형간염
vdism_2 <- dis_month$콜레라
vdism_3 <- dis_month$장티푸스
vdism_4 <- dis_month$이질
vdism_5 <- dis_month$대장균

head(vdism_4)

plot(vdism_1, xlab="", ylab="", ylim=c(0,1500), axes=FALSE, col = "violet" , type="o" , main="1군 전염병 발병현황-월별(단위:건수) 출처:통계청")

axis(1,at=1:12, lty=2, lwd=0.2)
axis(2,las=1)

lines(vdism_2, col="blue", type="o", lwd=2)
lines(vdism_3, col="red", type="o", lwd=2)
lines(vdism_4, col="black", type="o", lwd=2)
lines(vdism_5, col="orange", type="o", lwd=2)

abline(h=seq(0,1500,100), v=seq(1,12,1))

div_dis <- names(dis_month[2:6])
head(div_dis)

colors <- c("violet", "blue", "red","black", "orange")

legend(11,1500, div_dis, cex=0.8, col=colors, lty=1, lwd=2 , bg="green")
