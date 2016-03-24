##----------------------
## -- 데이터 입력
##----------------------
ctrl <- scan("data1.txt")
trtl <- scan("data2.txt")
trt2 <- scan("data3.txt")

##----------------------
## -- 각 데이터 별 평균값(mean)
##----------------------
ctrl.avg <- mean(ctrl)
trtl.avg <- mean(trtl)
trt2.avg <- mean(trt2)

##----------------------
## -- 각 데이터 별 중간값(median)
##----------------------
ctrl.med <- median(ctrl)
trtl.med <- median(trtl)
trt2.med <- median(trt2)


##----------------------
## -- 각 데이터 별 표준편차(sd)
##----------------------
ctrl.sd <- sd(ctrl)
trtl.sd <- sd(trtl)
trt2.sd <- sd(trt2)

##----------------------
## -- 각 데이터 별 요약(summary)
##----------------------
summary(ctrl)
summary(trtl)
summary(trt2)

##----------------------
## -- ctrl 그림그리기
##----------------------
plot(ctrl, main="Plant Yield", ylab="kg", type="b", lty=1, ylim=c(3.5, 6.5), lwd=2, col=1)
points(trtl, pch=2, type="b", lty=2, lwd=2, col=2)
points(trt2, pch=3, type="b", lty=3, lwd=2, col=3)
?par
