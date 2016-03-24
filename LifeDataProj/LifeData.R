library(XLConnect)
library(ggplot2)

data <- loadWorkbook("book_data.xlsx")
data_df <- readWorksheet(data,1)
head(data_df)

## Not Use Column except
data_df.except <- data_df[,-c(4,9,13)] # Translater , ISBN, Tag 제외
head(data_df.except)
str(data_df.except)

## Convert Data Type to factor
data_df.except$Borrowed <- as.factor(data_df.except$Borrowed)
data_df.except$Have.Weekend <- as.factor(data_df.except$Have.Weekend)
data_df.except$Category <- as.factor(data_df.except$Category)
data_df.except$Format <- as.factor(data_df.except$Format)
data_df.except$Status <- as.factor(data_df.except$Status)

## 기본 탐색 시작 ##########################################################################################
## Category by Read Count
ggplot(data=data_df.except, aes(factor(Category), fill=factor(Category)))+geom_bar()+ggtitle("카테고리별 읽은 권수")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)

## Borrowed Count(Y : Borrowed , N : Not Borrowed)
ggplot(data=data_df.except, aes(factor(Borrowed), fill=factor(Borrowed)))+geom_bar()+ggtitle("빌린 권수")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)

## Read Format Count
ggplot(data=data_df.except, aes(factor(Format), fill=factor(Format)))+geom_bar()+ggtitle("책 읽기 수단")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)
## paperback은 book으로 해야 하는데 앱상에 기입하며 잘못 기입된 데이터.

## Reading.Period and Pages Correlation 
ggplot(data= data_df.except, aes(x=Reading.Period, y=Pages))+geom_point()+ggtitle("읽은 기간과 페이지 수")
mean(data_df.except$Reading.Period)
cor(data_df.except$Reading.Period, data_df.except$Pages) # 0.3154084 
# 그다지 상관관계가 존재 하지 않음.

## Rating vs Yes24.Rating Correlation
ggplot(data= data_df.except, aes(x=Rating, y=Yes24.Rating))+geom_point()+ggtitle("별점 비교")
cor(data_df.except$Rating, data_df.except$Yes24.Rating) # 강한 음의 상관관게

ggplot(data=data_df.except, aes(factor(Have.Weekend), fill=factor(Have.Weekend)))+geom_bar()+ggtitle("주말 여부")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)

##########################################################################################################################

## Rating Score depending on whether the weekend
head(data_df.except)
data_df.except.Week <- data_df.except[data_df.except$Have.Weekend=="Y",c(1,2,7,9,10,12,13,14,15)]
data_df.except.NoWeek <- data_df.except[data_df.except$Have.Weekend=="N", c(1,2,7,9,10,12,13,14,15)]

head(data_df.except.Week)
ggplot(data=data_df.except.Week, aes(factor(Rating), fill=factor(Rating)))+geom_bar()+ggtitle("별점 현황-주말포함")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)

head(data_df.except.NoWeek)
ggplot(data=data_df.except.NoWeek, aes(factor(Rating), fill=factor(Rating)))+geom_bar()+ggtitle("별점 현황-주말제외")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)

## 주말이 포함된 경우 주말을 끼지 않고 읽었을 때 보다 대체적으로 Rating 이 낮음... 
## 데이터 상으로 봤을 때 낮은 이유는 빌린 여부에 해당하지 않을까 생각함. 

## NoWeek vs Week Borrowed 
ggplot(data = data_df.except.Week, aes(Borrowed , fill=Borrowed))+geom_bar()+ggtitle("책 빌린여부 확인-주말포함")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)
ggplot(data = data_df.except.NoWeek, aes(Borrowed , fill=Borrowed))+geom_bar()+ggtitle("책 빌린여부 확인-주말제외")+theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_text(aes(y=..count.., label=..count..),stat="count", color="blue", hjust=1.0, size=5)

## 주말 포함의 경우, 5권이 빌리지 않았던 책이고, 1권이 빌린 책임
## 주말 제외의 경우, 빌린 책과 아닌 책이 각각 3권씩임.


