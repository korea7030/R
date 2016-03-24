install.packages("KoNLP")
install.packages("wordcloud")
install.packages("rJava")
install.packages("stringr")
install.packages("RColorBrewer")

### 

## install.packages("wordcloud")
## install.packages("tm")
## install.packages("SnowballC")
## install.packages("Rcpp")

library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(rJava)

### 
## library(Rcpp)
##library(tm)
## library(SnowballC)


steve <- readLines("steve.txt")
steve

steve <- sapply(steve, extractNoun, USE.NAMES=F)
steve

steve2 <- Filter(function(x) { nchar(x) > 2}, unlist(steve))
steve2

head(steve2,30)

steve2 <-gsub("\\d+","",steve2)
steve2 <- gsub("that", "", steve2)
steve2 <- gsub("with", "", steve2)
steve2 <- gsub("have", "", steve2)
steve2 <- gsub("what", "", steve2)
steve2 <- gsub("your", "", steve2)
steve2 <- gsub("about", "", steve2)
steve2 <- gsub("very", "", steve2)
steve2 <- gsub("when", "", steve2)
steve2 <- gsub("know", "", steve2)
steve2 <- gsub("later", "", steve2)
steve2 <- gsub("only", "", steve2)
steve2 <- gsub("then","", steve2)
steve2 <- gsub("Stay", "", steve2)
steve2 <- gsub("love", "", steve2)
steve2 <- gsub("ever", "", steve2)
steve2 <- gsub("would", "", steve2)
steve2 <- gsub("ething", "", steve2)
steve2 <- gsub("into", "", steve2)
steve2 <- gsub("most", "", steve2)

steve2 <- gsub("they", "", steve2)
steve2 <- gsub("just", "", steve2)
steve2 <- gsub("this", "", steve2)
steve2 <- gsub("didn", "", steve2)
steve2 <- gsub("want", "", steve2)
steve2 <- gsub("were", "", steve2)
steve2 <- gsub("bean", "", steve2)

steve2 <- gsub("som", "", steve2)
steve2 <- gsub("before", "", steve2)
steve2 <- gsub("being", "", steve2)

steve2 <- gsub("the", "", steve2)
steve2 <- gsub("and", "", steve2)
steve2 <- gsub("was", "", steve2)
steve2 <- gsub("had", "", steve2)
steve2 <- gsub("out", "", steve2)
steve2 <- gsub("for", "", steve2)
steve2 <- gsub("all", "", steve2)
steve2 <- gsub("but", "", steve2)
steve2 <- gsub("one", "", steve2)
steve2 <- gsub("The", "", steve2)
steve2 <- gsub("been", "", steve2)
steve2 <- gsub("But", "", steve2)
steve2 <- gsub("can", "", steve2)
steve2 <- gsub("And", "", steve2)
steve2 <- gsub("did", "", steve2)
steve2 <- gsub("Don", "", steve2)
steve2 <- gsub("few", "", steve2)
steve2 <- gsub("get", "", steve2)
steve2 <- gsub("got", "", steve2)
steve2 <- gsub("not", "", steve2)

steve2 <- gsub("you", "", steve2)
steve2 <- gsub("now", "", steve2)
steve2 <- gsub("dots", "", steve2)

steve2 <- gsub("way", "", steve2)
steve2 <- gsub("who", "", steve2)
steve2 <- gsub("from", "", steve2)

steve2 <- gsub("going", "", steve2)
steve2 <- gsub("has", "", steve2)


write(steve2, "steve_2.txt")

steve3 <- read.table("steve_2.txt")

steve4 <- unlist(steve3, use.names = FALSE)
length(steve4)

write(steve4, "steve_3.txt")


for (i in 1:length(steve4)) {
  steve5 <- steve4[which(length(steve4[i]) > 1)]
}

steve5

wordcount <- table(steve3)
head(sort(wordcount, decreasing=T), 30)







windowsFonts(malgun=windowsFont("맑은 고딕"))

palete <- brewer.pal(8,"Set2")

wordcloud(names(wordcount2), freq=wordcount2, scale=c(5,0.5),rot.per=0.25, min.freq=2, random.order=T, random.color=T, colors=palete, family = "malgun")
warnings()
