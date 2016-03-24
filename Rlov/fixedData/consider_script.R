## excel package사용
install.packages("XLConnect")
library(XLConnect)

xls = loadWorkbook("./sample/청소년통계.xls", create=TRUE)
str(xls)

teenager2 <- readWorksheet(xls,sheet="고민문제")
teenager3 <- readWorksheet(xls,sheet="음주흡연율")
teenager4 <- readWorksheet(xls,sheet="학교생활만족도")
teenager5 <- readWorksheet(xls,sheet="학교폭력")
teenager6 <- readWorksheet(xls,sheet="직업선택요인")
teenager7 <- readWorksheet(xls,sheet="선호하는직장")

str(teenager7)

## 그래프 배치 위한 방법
par(mfrow=c(3,2))

## 그래프 그리기(주요 고민거리) 
bp2 <- barplot(as.matrix(teenager2), main="주요 고민거리", beside=T, axes=F, ylab="비율(%)", xlab="항목", cex.names=1.0, ylim=c(0,40), col=c("skyblue", "plum"), border="white", las=2, names.arg=c("외모","가정환경","용돈","성적","직업","친구","이성교제","흡연","기타","고민없음"))
## y축 범위 지정
axis(2, ylim=seq(0,40,10), las=1)
## 가로점선
abline(h=seq(0,40,5), lty=2)
## 퍼센트 표시 위한 
pct <- as.matrix(teenager2)
text(x=bp2, y=as.matrix(teenager2)*1.05, labels=paste(pct,"%"), col="navy", cex=0.7)

## 범례
legend("topright", c("남자","여자"), cex=0.9, fill=c("skyblue","plum"), bg="white")


## 흡연율/음주율 그래프 
bp3 <- barplot(as.matrix(teenager3), main="흡연율/음주율", beside=T, axes=F, ylab="비율(%)", xlab="연도(년)", cex.names=1.2, ylim=c(0,40), col=c("gray", "tan"), border="white", las=2, names.arg=c("2009","2010","2011","2012","2013"))

axis(2, ylim=c(0,10,20,30,40), las=1)
## 퍼센트 글자 표시
pct <- as.matrix(teenager3)
text(x=bp3, y=as.matrix(teenager3) * 1.05, labels=paste(pct,"%"), col="navy", cex=0.7)
## 범례 표시
legend("topright", c("흡연","음주"), cex=0.9, fill=c("gray", "tan"), bg="white")

## 학교생활 만족도 그래프  
bp4 <- barplot(as.matrix(teenager4), main="학교 생활 만족도", beside=T, axes=F, ylab="비율(%)", xlab="만족도", cex.names=1.2, ylim=c(0,40), col=c("gray", "tan"), border="white", las=2, names.arg=c("매우만족","만족","보통","불만족","매우불만족"))

axis(2, ylim=c(0,10,20,30,40,50,60), las=1)
## 퍼센트 글자 표시
abline(h=seq(0,60,5), lty=2)
pct <- as.matrix(teenager4)
text(x=bp3, y=as.matrix(teenager4) * 1.05, labels=paste(pct,"%"), col="navy", cex=0.7)
## 범례 표시
legend("topright", c("중학생","고등학생"), cex=0.9, fill=c("limegreen", "gold"), bg="white")



## 학교폭력사유   
bp5 <- barplot(as.matrix(teenager5), main="학교 폭력 사유", beside=T, axes=F, ylab="비율(%)", xlab="피해사유", cex.names=1.2, ylim=c(0,40), col=c("skyblue", "plum","limegreen", "gold"), border="white", las=2, names.arg=c("피해경험","이유없다","약해서","잘못해서","외모/장애","성격","금품요구불응","기타"))

axis(2, ylim=c(0,10,20,30,40,50,60), las=1)
## 퍼센트 글자 표시
abline(h=seq(0,60,5), lty=2)
pct <- as.matrix(teenager5)
text(x=bp5, y=as.matrix(teenager5) * 1.05, labels=paste(pct,"%"), col="navy", cex=0.7)
## 범례 표시
legend("topright", c("남자","여자","중학생","고등학생"), cex=0.9, fill=c("skyblue","plum","limegreen", "gold"), bg="white")

## 직업선택 요인 그래프
bp6 <- barplot(as.matrix(teenager6), main="직업 선택 요인", beside=T, axes=F, ylab="비율(%)", xlab="항목", cex.names=1.2, ylim=c(0,50), col=c("skyblue", "plum","limegreen", "gold"), border="white", las=2, names.arg=c("명예/명성","안정성","수입","적성/흥미","보람/자아성취","발전가능성","기타"))

axis(2, ylim=c(0,10,20,30,40,50), las=1)
## 퍼센트 글자 표시
abline(h=seq(0,50,5), lty=2)
pct <- as.matrix(teenager6)
text(x=bp6, y=as.matrix(teenager6) * 1.05, labels=paste(pct,"%"), col="navy", cex=0.7)
## 범례 표시
legend("topright", c("남자","여자","중학생","고등학생","대학생"), cex=0.9, fill=c("skyblue","plum","limegreen", "gold","blue"), bg="white")

## 직장 선호도 그래프 그리기 
bp7 <- barplot(as.matrix(teenager7), main="직장 선호도", beside=T, axes=F, ylab="비율(%)", xlab="기업 종류", cex.names=1.2, ylim=c(0,40), col=c("skyblue", "plum","limegreen", "gold","blue"), border="white", las=2, names.arg=c("국가기관","공기업","대기업","벤쳐","외국계","전문직","중소기업","해외","자영업","기타"))

axis(2, ylim=c(0,10,20,30,40), las=1)
## 퍼센트 글자 표시
abline(h=seq(0,40,5), lty=2)
pct <- as.matrix(teenager7)
text(x=bp7, y=as.matrix(teenager7) * 1.05, labels=paste(pct,"%"), col="navy", cex=0.7)
## 범례 표시
legend("topright", c("남자","여자","중학생","고등학생","대학생"), cex=0.9, fill=c("skyblue","plum","limegreen", "gold","blue"), bg="white")
