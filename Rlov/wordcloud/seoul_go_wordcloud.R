install.packages("KoNLP")
install.packages("wordcloud")
install.packages("rJava")
install.packages("stringr")
install.packages("RColorBrewer")

library(KoNLP)
library(wordcloud)
library(rJava)
library(stringr)
library(RColorBrewer)

seoul_go <- readLines("seoul_go.txt")
seoul_go

place <- sapply(seoul_go,extractNoun, USE.NAMES=F)
place

head(place,10)

place2 <- unlist(place)
place3 <- Filter(function(x) { nchar(x) >= 2}, place2)
place3

res <- str_replace_all(place3, "[^[:alpha:]]", "")
res <- res[res != ""]
res <- gsub("서울","",res)
res <- gsub("명소","",res)
res <- gsub("블로그","",res)
res <- gsub("검색","",res)
res <- gsub("데이트","",res)
res <- gsub("tistory", "", res)
res <- gsub("시내", "", res)
res <- gsub("데이트","",res)
res <- gsub("코스","", res)
res <- gsub("소인배小人輩","",res)
res <- gsub("을","",res)
res <- gsub("국민","",res)
res <- gsub("연인","",res)
res <- gsub("한곳","",res)
res <- gsub("주말","",res)
res <- gsub("한눈","",res)
res <- gsub("개월","",res)
res <- gsub("여행지","",res)
res <- gsub("개월","",res)
res <- gsub("여행","",res)
res <- gsub("생각","",res)
res <- gsub("여행지","",res)
res <- gsub("구경하","",res)
res <- gsub("다","",res)
res <- gsub("대장장이","",res)
res <- gsub("쭈욱", "", res)
res <- gsub("화덕","",res)
res <- gsub("블로거s","",res)
res <- gsub("blog","",res)
res <- gsub("com","",res)
res <- gsub("가볼만한곳","",res)
res <- gsub("트랜드","",res)
res <- gsub("여행", "", res)
res <- gsub("작정","",res)
res <- gsub("SH기획","",res)
res <- gsub("그림","",res)
res <- gsub("사진","",res)
res <- gsub("출사","", res)
res <- gsub("좋은곳", "", res)
res <- gsub("커플어플", "", res)
res <- gsub("한풀꺽였던", "", res)
res <- gsub("무미건조", "",res)
res <- gsub("상냥","",res)
res <- gsub("입니","",res)
res <- gsub("ㅠㅠ그래도","",res)
res <- gsub("감성","",res)
res <- gsub("춥긴","",res)
res <- gsub("특", "", res)
res <- gsub("등극","",res)
res <- gsub("아동문학가","",res)
res <- gsub("나들이였답니","",res)

res <- gsub("사실","",res)
res <- gsub("패션","",res)
res <- gsub("큐플에서","",res)
res <- gsub("추천","",res)
res <- gsub("안녕","",res)
res <- gsub("이용","",res)
res <- gsub("풍경","",res)
res <- gsub("시장","",res)
res <- gsub("오늘","",res)
res <- gsub("포스팅","",res)

res <- gsub("못할경우","",res)
res <- gsub("새로바뀐","",res)
res <- gsub("가볼만","",res)

res <- gsub("명성","",res)
res <- gsub("me","",res)
res <- gsub("우슬","",res)
res <- gsub("유일무이","",res)

res <- gsub("naver","",res)
res <- gsub("인사","",res)
res <- gsub("소개","",res)
res <- gsub("요한","",res)
res <- gsub("약도","",res)
res <- gsub("관광","",res)
res <- gsub("km","",res)
res <- gsub("에","",res)
res <- gsub("걷기","",res)
res <- gsub("마","",res)
res <- gsub("가","",res)
res <- gsub("거리","",res)

res <- gsub("Always","",res)
res <- gsub("asd","",res)
res <- gsub("baaa","",res)
res <- gsub("backnumber","",res)
res <- gsub("barty","",res)
res <- gsub("bookslover","",res)
res <- gsub("campusstar","",res)
res <- gsub("cendy","",res)
res <- gsub("civileng","",res)
res <- gsub("cjg","",res)
res <- gsub("coipsd","",res)
res <- gsub("cruiserlaw","",res)
res <- gsub("damul","",res)
res <- gsub("daramzi","",res)
res <- gsub("야경","",res)
res <- gsub("나들이","",res)
res <- gsub("벚꽃", "", res)
res <- gsub("단풍", "", res)
res <- gsub("축제","",res)
res <- gsub("행복","",res)
res <- gsub("하늘","",res)
res <- gsub("공원","",res)
res <- gsub("올림픽","",res)
res <- gsub("억새","",res)
write(res,"seoul_go2.txt")

place4 <- read.table("seoul_go2.txt")
place4

recommend <- table(place4)
recommend
head(sort(recommend, decreasing=F),20)

palete <- brewer.pal(8,"Set2")

## window font 설정
windowsFonts(malgun=windowsFont("맑은 고딕"))

## min.freq : 최소 출력된 값
## family : 폰트 설정
## random.color : 폰트색깔 random 여부
wordcloud(names(recommend), freq=recommend, rot.per=1, min.freq=3, random.order=F, random.color=T, colors=palete, family = "malgun")
warnings()

top10 <- head(sort(recommend, decreasing=T),10)
pie(top10)
## 색깔추가
pie(top10, col=rainbow(10), radius=1)
savePlot("seoul_go__2.png",type="png")

## 수치 나오도록 
pct <- round(top10/sum(top10)*100, 1)
names(top10)
lab <- paste(names(top10),"\n",pct,"%")
pie(top10,main="서울시 가볼만한 명소 TOP10", col=rainbow(10), cex=0.8, labels=lab)

## 도넛모양 차트 
pie(top10,main="서울시 가볼만한 명소 TOP10", col=rainbow(10), cex=0.8, labels=lab)
par(new=T) ## 덧칠하기위해 새로 생성
pie(top10, radius=0.6, col="white" , labels=NA, border=NA)


## 3D pie 차트 추가 
install.packages("plotrix")
library(plotrix)

th_pct <- round(top10/sum(top10)*100,1)
th_names<-names(top10)
th_labels <- paste(th_names,"\n","(",th_pct,")")
pie3D(top10,main="서울의 가볼만한 장소 Top 10", col=rainbow(10), cex=0.5, labels=th_labels,explode=0.1)
#######################
## bar차트 

bplot <- barplot(top10, main="서울의 가볼만한 명소 Top 10", col=rainbow(10), cex.names=0.8, las=2,  ylim=c(0,30))
pct <- round(top10/sum(top10)*100,1)

pct

text(x=bplot, y=top10*1.05, labels=paste("(",pct,"%",")"), col="black", cex=0.7)
text(x=bplot, y=top10*0.95, labels=paste(bchart,"건"), col="black", cex=0.7)
######################