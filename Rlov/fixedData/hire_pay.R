hire_pay <- read.csv("고용형태별_취업자현황_월급현황.csv")

head(hire_pay)

col <- hire_pay$년도

vp_1 <- hire_pay$평균.천원.
vp_2 <- hire_pay$정규직근로자
vp_3 <- hire_pay$비정규직근로자
vp_4 <- hire_pay$파견.용역근로자
vp_5 <- hire_pay$일일근로자
vp_6 <- hire_pay$단시간근로자
vp_7 <- hire_pay$기간제근로자

head(vp_1)

plot(vp_1, xlab="", ylab="", ylim=c(500,3000),, axes=FALSE, col="violet", type="o", lwd=2, main="고용형태별 급여현황(단위:천원) 출처:통계청")

axis(1, at=1:7, label=col, las=2)
axis(2, las=1)

lines(vp_2, col="blue", type="o", lwd=2)
lines(vp_3, col="red", type="o", lwd=2)
lines(vp_4, col="black", type="o", lwd=2)
lines(vp_5, col="orange", type="o", lwd=2)
lines(vp_6, col="pink", type="o", lwd=2)
lines(vp_7, col="gold", type="o", lwd=2)

abline(h=seq(0,25,1),v=seq(1,7,1), lty=2, lwd=0.2)

col1 <- names(hire_pay[2:8])

colors <- c("violet","blue","red","black","orange", "pink","gold")

legend(1,3000, col1, cex=0.8, col=colors, lty=1, lwd=2, bg="white")
