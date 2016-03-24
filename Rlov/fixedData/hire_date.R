hire_rate <- read.csv("고용형태별_취업자현황_근무일수.csv")

head(hire_rate)

v_1 <- hire_rate[,2]
v_2 <- hire_rate[,3]
v_3 <- hire_rate[,4]
v_4 <- hire_rate[,5]
v_5 <- hire_rate[,6]
v_6 <- hire_rate[,7]
v_7 <- hire_rate[,8]

plot(v_1, xlab="", ylab="", ylim=c(10,25),, axes=FALSE, col="violet", type="o", lwd=2, main="고용형태별 근무일수(단위:일) 출처:통계청")

axis(1, at=1:7, label=hire_rate$년도, las=2)
axis(2, las=1)

lines(v_2, col="blue", type="o", lwd=2)
lines(v_3, col="red", type="o", lwd=2)
lines(v_4, col="black", type="o", lwd=2)
lines(v_5, col="orange", type="o", lwd=2)
lines(v_6, col="pink", type="o", lwd=2)
lines(v_7, col="gold", type="o", lwd=2)

abline(h=seq(0,25,1),v=seq(1,7,1), lty=2, lwd=0.2)

col <- names(hire_rate[2:7])
colors <- c("violet","blue", "red","black","orange", "pink","gold")
legend(1,15,col,cex=0.8, col=colors, lty=1, lwd=2, bg="white")
