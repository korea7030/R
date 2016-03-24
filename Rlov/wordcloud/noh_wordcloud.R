getwd()
noh <- readLines("noh.txt")
noh

noh_sentence <- sapply(noh, extractNoun, USE.NAMES=F)
noh_sentence 

noh_sentence1 <- unlist(noh_sentence)
noh_sentence1 <- Filter(function(x) { nchar(x) >= 2}, noh_sentence1)
noh_sentence1

res <- gsub("\\d+","",noh_sentence1 )
res <- gsub("이번","",res )
res <- gsub("최초","",res )
res <- gsub("하게","",res )
res <- gsub("시작","",res )
res <- gsub("사상","",res )
res <- gsub("국민","",res )
res <- gsub("모두","",res )
res <- gsub("하기","",res )
res <- gsub("기대","",res )
res <- gsub("여러분","",res )
res <- gsub("대통령","",res )
res

write(res, "noh_1.txt")

word <- read.table("noh_1.txt")
word

wordcount <- table(word)
head(sort(wordcount, decreasing=T))

windowsFonts(malgun=windowsFont("맑은 고딕"))

wordcloud(names(wordcount), freq=wordcount, scale=c(5,0.5),rot.per=0.25, min.freq=1, random.order=T, random.color=T, colors=palete, family = "malgun")

## pie 차트 
noh_top10 <- head(sort(wordcount, decreasing=T),10)
pie(noh_top10)
pie(noh_top10,  radius=1)

pct <- round(noh_top10/sum(noh_top10)*100, 1)
names(noh_top10)
lab <- paste(names(noh_top10),"\n",pct,"%")
pie(noh_top10,main="노무현 전 대통령님 연설문 분석", cex=0.8, labels=lab)
pie(noh_top10,main="노무현 전 대통령님 연설문 분석", col=rainbow(10), cex=0.8, labels=lab)


## bar 차트 
bplot <- barplot(noh_top10, main="노무현 전 대통령님 연설문 분석", col=rainbow(10), cex.names=0.8, las=2,  ylim=c(0,30))
pct <- round(noh_top10/sum(noh_top10)*100,1)

pct

text(x=bplot, y=noh_top10*1.05, labels=paste("(",pct,"%",")"), col="black", cex=0.7)
text(x=bplot, y=noh_top10*0.95, labels=paste(noh_top10,"건"), col="black", cex=0.7)
