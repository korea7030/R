library(dplyr)
library(ggplot2)

head(basketData)
rm(list=ls())
## 등급별 손님수 확인
tblgrade <- table(userRFM$grade)

plot(tblgrade)

## type은 선모양을 나타내.
plot(tblgrade, type="l", xaxt="n", ylab="")
## plot의 main title지정
title(main="My Second plot image")
## length(tblgrade) : 5 길이 만큼 해서 x축 label 지정
axis(1, at=1:length(tblgrade), label=c("최우수","우수","보통","평균","불만"))

## col.lab 색깔지정 
## x축 y축 이름지정
title(xlab="고객구분",col.lab="red")
title(ylab="고객수", col.lab='blue')

## factor로 지정
userRFM$gradeFac <- factor(userRFM$grade, levels=c("A","B","C","D","E"))

## 등급별 RFM 점수 현황을 boxplot으로
boxplot(userRFM$score~userRFM$grade)

library(plyr)

basketGrade <- join(basketData, userRFM[,c("custId", "grade")])

## basketGrade <- basketGrade[,-(ncol(basketGrade)-1)]

head(as.data.frame(basketGrade))
names(basketGrade)

gradeOut <- basketGrade %>% group_by(grade, custId, branchId) %>% summarize(avgPurchase=mean(amount))

head(gradeOut)
dim(gradeOut)

## 등급별 구매평균금액
boxplot(gradeOut$avgPurchase~gradeOut$grade, notch=T)

library(reshape)
graphOut2 <- basketGrade %>%
  group_by(grade, custId, branchId) %>%
  summarize(avgPurchase=mean(amount)) %>%
  group_by(grade, branchId) %>%
  summarize(N=n())

# 지점별 실적
graphOut2

graphDummy <- data.frame(graphOut2)
graphCast <- cast(graphDummy, grade ~ branchId, value="N", fun.aggregate=sum)

matplot(graphCast, type="o")
matplot(graphCast, type="o", pch=c(16,20,21,22), col=c("blue", "yellow","magenta","red"))

## 범례
legend("topright",
       c("branchId_1","branchId_2","branchId_3","branchId_4"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       cex=1,
       lwd=c(5, 5, 5,5),
       col=c("blue", "yellow","magenta","red"))

## 카테고리 별 분류 
graphOut3 <- basketGrade %>%
  group_by(category, division) %>%
  filter(category == "livestock") %>%
  summarize(avgPurchase=mean(amount))
%>%
  group_by(grade, branchId) %>%
  summarize(N=n())

## 특정상품에 대한 분류의 퍼센테이지.
graphOut3$percentage <- graphOut3$avgPurchase/sum(graphOut3$avgPurchase)  

head(graphOut3, 10)

boxplot(graphOut3$avgPurchase ~ graphOut3$category)

graphDummy <- data.frame(graphOut2)
graphCast <- cast(graphDummy, grade ~ branchId, value="N", fun.aggregate=sum)


graphOut4 <- cast(graphOut3, grade ~ category, value="avgPurchase", fun.aggregate=sum)

head(graphOut4)

# 등급별 소비량 그래프
matplot(graphOut4, type="l")

graphmat <- as.matrix(graphCast)
head(graphmat)
pdfpngplot(graphmat, "graphOut", "First Png", "total Number of customers")

pdfpngplot <- function(data, filename, maintitle, labtitle) {
  # pdf 그래프 출력 샘플 -------
  # 15 vs 11.25, 30 vs 22.5
  
  pdf(paste0(filename,".pdf"), width=10, height=7)
  
  par(mar=c(5,7,10,1))
  
  matplot(data, type="l", xaxt="n", yaxt="n",
          col=c("blue", "yellow","magenta","green","red"),
          lwd=c(3,3,3,3),ylab="")
  title(main=maintitle, 
        col.main="gray8", cex.main=3.2)
  title(ylab=labtitle, 
        col.lab="black", cex.lab=3)
  
  axis(1,at=1:dim(data)[1],labels=row.names(data), las=2, cex=0.5)
  axis(2, cex.axis=1.5)
  
  legend("topright",
         c("branchId_1","branchId_2","branchId_3","branchId_4"),
         lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
         cex=1,
         lwd=c(5, 5, 5,5),
         col=c("blue", "yellow","magenta","red"))
  dev.off()
  
  # png 그래프 출력 샘플 -------
  png(paste0(filename,".png"), width=10, height=7, units = "in", res=400)
  
  par(mar=c(5,7,10,1))
  
  matplot(data, type="l", xaxt="n", yaxt="n",
          col=c("blue", "yellow","magenta","red"),
          lwd=c(3,3,3,3), ylab="")
  
  title(main=maintitle, 
        col.main="gray8", cex.main=3.2)
  
  title(ylab=labtitle, 
        col.lab="black", cex.lab=3)
  
  axis(1,at=1:dim(data)[1],labels=row.names(data), las=2, cex=0.5)
  axis(2, cex.axis=1.5)
  
  legend("topright",
         c("branchId_1","branchId_2","branchId_3","branchId_4"),
         lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
         cex=1,
         lwd=c(5, 5, 5,5),
         col=c("blue", "yellow","magenta","red"))
  dev.off()
}