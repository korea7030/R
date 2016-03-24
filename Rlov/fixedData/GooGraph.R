data1 <- read.csv("./sample/2013년_서울_주요구별_병원현황.csv", header=T)
dir("./sample")
head(data1)

v1 <- data1[1:9,2]*0.1 # 강남
v2 <- data1[1:9,3]*0.1 # 강동
v3 <- data1[1:9,4]*0.1 #강서
v4 <- data1[1:9,5]*0.1 #관악
v5 <- data1[1:9,6]*0.1 #구로
v6 <- data1[1:9,7]*0.1 #도봉
v7 <- data1[1:9,8]*0.1 #동대문
v8 <- data1[1:9,9]*0.1 #동작
v9 <- data1[1:9,10]*0.1 #마포
v10 <- data1[1:9,11]*0.1 #서대문

par(mfrow=c(2,5))
name <- data1$표시과목
name

graphOut <- function (data, mainTitle) {
  gangnam <-barplot(as.matrix(data), main=mainTitle, beside=T,axes=F, ylab="병원수(단위10개)", xlab="", cex.names=0.85, las=2, ylim=c(0,40), col=rainbow(8), border="white", names.arg=name)
  
  axis(2,ylim=seq(0,25,10))
  abline(h=seq(0,35,5), lty=2)
}

graphOut(v1, "강남구 병원현황")
graphOut(v2, "강동구 병원현황")
graphOut(v3, "강서구 병원현황")
graphOut(v4, "관악구 병원현황")
graphOut(v5, "구로구 병원현황")
graphOut(v6, "도봉구 병원현황")
graphOut(v7, "동대문구 병원현황")
graphOut(v8, "동작구 병원현황")
graphOut(v9, "마포구 병원현황")
graphOut(v10, "서대문구 병원현황")
#
