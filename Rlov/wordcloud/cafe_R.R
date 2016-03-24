cafe <- readLines("yeca_1.txt")
cafe

order <- sapply(cafe, extractNoun, USE.NAMES=F)

head(order)

cafe_2 <- unlist(order)
cafe_2

cafe_3 <- Filter(function(x) { nchar(x) >= 2}, cafe_2)

cafe_3 <- gsub("공구","", cafe_3)
cafe_3 <- gsub("", "-", cafe_3)
cafe_3 <- gsub("공구해주세요.","", cafe_3)
cafe_3 <- gsub("다운로드","", cafe_3)
cafe_3 <- gsub("공구해주세요~","", cafe_3)
cafe_3 <- gsub("조회수","", cafe_3)
cafe_3 <- gsub("추천수","", cafe_3)
cafe_3 <- gsub("답변수", "", cafe_3)
cafe_3 <- gsub("\\n","", cafe_3)
cafe_3 <- gsub("\\d+","", cafe_3)
cafe_3 <- gsub("ㅠㅠ..","",cafe_3)
cafe_3 <- gsub("퍼스나콘/아이디","", cafe_3)
cafe_3 <- gsub("구해주세요~~","", cafe_3)
cafe_3 <- gsub("구해주세요.","", cafe_3)
cafe_3 <- gsub("해주","", cafe_3)
cafe_3 <- gsub("부탁","", cafe_3)
cafe_3 <- gsub("영역","",cafe_3)
cafe_3 <- gsub("주세","",cafe_3)
cafe_3 <- gsub("해주세요.","", cafe_3)
cafe_3 <- gsub("해주세요","", cafe_3)
cafe_3 <- gsub("해주세요~~", "", cafe_3)
cafe_3 <- gsub("-", "", cafe_3)
cafe_3 <- gsub("퍼","", cafe_3)
cafe_3 <- gsub(".", "", cafe_3)
head(unlist(cafe_3), 20)

write(unlist(cafe_3), "yeca_2.txt")

cafe_4 <- read.table("yeca_2.txt")

nrow(cafe_4)

wordcount <- table(cafe_4)
head(sort(wordcount, decreasing=T), 20)

palete <- brewer.pal(9,"Set1")
wordcloud(names(wordcount), freq = wordcount, scale=c(5,0,5), rot.per=1, min.freq=0.1, random.color=T, random.order=F, colors = palete)
