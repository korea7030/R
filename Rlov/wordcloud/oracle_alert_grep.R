install.packages("KoNLP")
install.packages("rJava")
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(rJava)

ora_alert <- readLines("oracle_alert_testdb.log")
head(ora_alert)

ora_alert <- gsub(" ", "_" , ora_alert)

ora_alert_unlist <- unlist(ora_alert)
ora_alert_unlist <- Filter(function(x) { nchar(x) >= 5}, ora_alert_unlist)
ora_alert_unlist




head(alert)
tail(alert)

alert2 <- grep("^ORA-+",ora_alert_unlist, value=T )
alert2

alert3 <- substr(alert2,5,9)
alert3

alert3 <- gsub("[A-z]", "", alert3)
alert3
write(unlist(alert3) , "oracle_log.txt")

error_log <- read.table("oracle_log.txt")
nrow(error_log)

wordcount <- table(error_log)

head(sort(wordcount, decreasing=T), 20)

windowsFonts(malgun=windowsFont("맑은 고딕"))

palete <- brewer.pal(8,"Set2")

wordcloud(names(wordcount), freq=wordcount, scale=c(5,0,5), rot.per=0.25, min.freq=3, random.order=T, random.color=T, colors=palete, family = "malgun")

error_top10 <- head(sort(wordcount, decreasing=T), 10)

bplot <- barplot(error_top10, main="오라클 에러코드 건별 출력", col=rainbow(10), cex.names=0.8, las=2,  ylim=c(0,4000))
pct <- round(error_top10/sum(error_top10)*100,1)

pct

text(x=bplot, y=error_top10*1.05, labels=paste("(",pct,"%",")"), col="black", cex=0.7)
text(x=bplot, y=error_top10*1.05, labels=paste(error_top10,"건"), col="black", cex=0.7)

## 선형 
## 첫 시작은 x축, y축을 잡지않고 선만..
plot(error_top10, xlab="", ylab="", ylim=c(0,4000), axes=FALSE, type="o", col="red", main="많이 발생하는 오라클 에러-선그래프", lwd=2)

## x축..
axis(1, at=1:10, lab=names(error_top10), las=2)

## y 축
axis(2, las=1)

## 뒤에 격자 무늬 추가
abline(h=seq(0,4000,100), v=seq(1,10,1), col="gray", lty=2)


## 선형 + 막대 
bplot <- barplot(error_top10, main="오라클 에러코드 종합", col=rainbow(10), cex.names=0.8, las=2,  ylim=c(0,4000))
pct <- round(error_top10/sum(error_top10)*100,1)

text(x=bplot, y=error_top10*1.05, labels=paste("(",pct,"%",")"), col="black", cex=0.7)
text(x=bplot, y=error_top10*1.0, labels=paste(error_top10,"건"), col="black", cex=0.7)



par(new=T)
plot(error_top10, xlab="", ylab="", ylim=c(0,4000), axes=FALSE, type="o", col="blue", lwd=1)

axis(4, las=1)
mtext("Line Chart", side=4)

abline(h=seq(0,4000,200), v=seq(1,10,1), col="gray", lty=2)


