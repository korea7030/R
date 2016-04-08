## 2014년1월1일 ~ 2014년12월31일(1년치 데이터)
## 55~84세 고객의 구매내역(서울,경기,부산)
sub2 <- read.table('subject2/1-2. 구매내역정보.txt', header=T, sep=",", stringsAsFactors = FALSE)
str(sub2)

library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)

############################################# 주제2 기본 EDA #################################################
procate1<- as.data.frame(table(sub2$상품대분류명))
# 가구 가전제품   레포츠     명품 생활잡화     식품 의류잡화 
# 734    11006     8015     1226    10086    38597    32028 
str(procate1)
procate1$Freq <- as.numeric(procate1$Freq)
procate2 <- as.data.frame(table(sub2$상품중분류명))
## 대분류별 개수(식품>의류잡화 순)
ggplot(procate1, aes(Var1, Freq))+geom_histogram(stat="identity", fill = c(1:7))
## 중분류별 개수(가공식품>여성의류-SPA>농산물 순)
ggplot(procate2, aes(Var1, Freq))+geom_bar(stat="identity", fill = "red")+theme(axis.text.x = element_text(angle=45,size=12))  
## 15~16시 사이 시간대가 가장 많음.
ggplot(data=sub2, aes(구매시간))+geom_histogram(binwidth=0.5, fill = "darkblue") 
## 취소여부
table(sub2_num$취소여부)
#     0     1 
# 93333  8359

## 구매지역(송파구>중구>안양시 순)
loc <- as.data.frame(table(sub2$구매지역))
str(loc)
ggplot(loc, aes(Var1, Freq))+geom_histogram(stat="identity")+theme(axis.text.x = element_text(angle=90,size=10, hjust = 1))  

demo2 <- read.table('subject2/1-1. Demo.txt',header=T, sep=",", stringsAsFactors = FALSE)
str(demo2)

## 성별(여성이 3배 정보 많음)
table(demo2$성별)
#   1    2 
# 859 2683

## 연령(63세 이후로 급격히 소비 감소)
age <- as.data.frame(table(demo2$연령))
ggplot(data=age, aes(Var1, Freq))+geom_histogram(stat="identity")

## 거주지역(송파구>해운대구>고양시 순) - 거주하는사람이 많은 지역이 구매지역이 가장 많지는 않음.
region <- as.data.frame(table(demo2$거주지역))
ggplot(data=region, aes(Var1, Freq))+geom_histogram(stat="identity")+theme(axis.text.x = element_text(angle=90, size=10, hjust=1))

merge_data2 <- merge(demo2, sub2, by="ID")
head(merge_data2)

jpeg("구매시간에 따른 지역별 구매금액.jpeg")
# 구매지역별 구매시가넹 따른 구매금액 plot 
ggplot(data=merge_data2, aes(x=구매시간, y=구매금액, colour = 구매지역))+geom_point()
dev.off()

# 상품분류별 구매금액
jpeg("상품분류별 구매금액.jpeg")
ggplot(data=merge_data2, aes(x=상품중분류명, y=구매금액, color = 상품대분류명))+geom_boxplot() + theme(axis.text.x = element_text(angle=90, face="bold", colour="black"))
dev.off()
# + facet_grid(상품대분류명~.)+ theme(axis.text.x = element_text(angle=90, face="bold", colour="black"))

head(merge_data2)

## 연령별 상품 구매금액합계
group_age <- group_by(merge_data2, 연령, 상품대분류명, 상품중분류명)
group_age.summary <- summarize(group_age, sumAmount=length(구매건수))
group_age.arrange <- arrange(group_age.summary, desc(sumAmount))
head(group_age.arrange)
ggplot(group_age.arrange, aes(x=연령, y=sumAmount, colour= 상품대분류명))+geom_line()+facet_grid(상품대분류명~상품중분류명)

group_buy <- subset(merge_data2, subset= 취소여부==0)
group_buy_age <- group_by(group_buy, 연령, 상품대분류명, 상품중분류명)
group_buy.summary <- summarize(group_buy_age, 구매건수합=length(구매수량))

head(group_buy.summary)

### 구매건수 합과 구매/취소 비율 그래프 비교 과정
merge_data2_cancel <- subset(merge_data2, (취소여부==1))
head(merge_data2_cancel)

group_cancel_age <- group_by(merge_data2_cancel, 연령, 상품대분류명, 상품중분류명)
group_cancel.summary <- summarize(group_cancel_age, 취소건수합=sum(취소여부))
head(group_cancel.summary)

buy_plot <- ggplot(group_buy.summary, aes(x=연령, y=구매건수합)) +geom_bar(fill = "darkblue", stat="identity")+
  facet_grid(~상품대분류명)+
  scale_y_continuous(limits=c(0,4000))

cancel_rate_plot<- ggplot(mg, aes(x=연령, y=비율))+
  geom_bar(fill = "darkblue", stat="identity")+
  facet_grid(~상품대분류명)

multiplot(buy_plot, cancel_rate_plot, cols=1)

### 구매건수 합과 구매/취소 비율 그래프 비교
############################################# 주제2 기본 EDA 끝 #################################################