############################################ 연관분석 #############################################################
# install.packages('arules')
library(arules)
# install.packages("pmml")
library(pmml)
### data get
tr <- read.delim('dataTransactions.tab', stringsAsFactors = FALSE)
head(tr)

### corner 레벨을 통해 일반식품과 화장품이 아닌 데이터 (custid 와 corner 로만 뽑기)
### subset(data, 조건(subset), 선택한컬럼(select)) : 데이터에서 조건에 맞는 선택한 컬럼을 추출 
### (select위치와 subset은 위치가 바뀔수 있음)
tr.filter <- subset(tr, subset=!(corner %in% c("일반식품", "화장품")), select=c(-1,-3:-5,-7:-9))
head(tr.filter)

### 중복 제거
tr.filter.uniq <- unique(tr.filter)
head(tr.filter.uniq)
### 고객별로 구매한 item을 묶어줌
### split(데이터,기준) : 기준별 데이터를 리스트형태로 나타냄
### as(data, "transactions") : transactions 형태로 데이터를 바꿔주라
trans <- as(split(tr.filter.uniq$corner, tr.filter.uniq$custid), "transactions")
trans # 487 명의 고객이 있음

image(trans[1:5])
# transaction에 대한 출력
inspect(trans)

### 연관분석(item 별 지지도가 0.2 이상인 plot)
itemFrequencyPlot(trans, support=0.2, cex.names = 0.8)

### apriori (최소 지지도 0.2, 최소 신뢰도가 0.8 이상인 것만 뽑아라)
rules <- apriori(trans, parameter = list(support=0.2, confidence = 0.8))
summary(rules)
inspect(rules) # 1.2의 향상도라도 별로 의미가 없다

### rules에서 rhs(결과절) 이 스포츠 이면서 향상도 값이 1.4보다 큰 규칙만 get
rules.target <- subset(rules, rhs %in% "스포츠" & lift > 1.4)
### 신뢰도가 가장 높은것부터 뽑아라
inspect(sort(rules.target, by="confidence"))

write(rules.target, file="arules.txt", sep="\t", row.names = F)
write.PMML(rules.target, file="data.xml") 

##################################################################################################################
############################################ Exercise ############################################################
### 데이터 형식이 id에 대한 상품의 구매여부(0,1) 형태로 되어있을 경우
data <- read.delim('shoppingmall.txt', stringsAsFactors = FALSE) 
### matrix로 변환(id 컬럼만 제거) 
st <- as.matrix(data[,-1]) 
###  trainsaction 형태로 변환
trans <- as(st, "transactions") 
##################################################################################################################
########################################### 군집분석 #############################################################
library(ggplot2)
### 데이터 읽기
cdata <- read.delim('Cluster.txt', stringsAsFactors = FALSE)
### kmeans(data, centers = 군집의수)
### 이 앞전에 상관계수나 PCA로 군집할 변수 개수 지정 필요
### (PCA로 전체 변수를 쓸수도 있고 일부만 사용할 수 있음)
### (지금은 데이터가 예쁜 데이터라 다 쓴걸로 보임. 
### 수업에서 다쓴다고 실제데이터를 할 때 군집분석할 경우 무조건 다 쓴다는 착각금지)
km <- kmeans(subset(cdata, select=-c(ID)), centers = 4)
str(km)
km
# install.packages("cluster")
library(cluster)
### 군집의 반경과 관계를 2차원으로 도식
### x,y 축은 알아서 변수 구성
clusplot(subset(cdata, select=-c(ID)), km$cluster)

### 최적의 군집수 찾기 1)
wss <- 0
for (i in 1:15) wss[i] <- kmeans(cdata, centers = i)$tot.withinss # 군집내 분산값
### 기울기가 급격하게 감소하는 지점에서 군집의 수 지정
plot(1:15, wss, type="b", xlab = "# of clusters ", ylab = "Within group sum of squares")

### 최적의 군집수 찾기 2)
# install.packages("NbClust")
library(NbClust)
### 군집의 실루엣 척도 평가(26개의 척도) 
### 그 척도 중에 가장 우수한 것만 뽑음
nc = NbClust(subset(cdata, select= -c(ID)), min.nc = 2, max.nc = 15, method='kmeans')
barplot(table(nc$Best.nc[1,]), xlab="# of clusters", ylab="# of criteria", main="Number of clusters chosen by 26 criteria")

### 군집의 분포 도식 
### kmeans의 경우는 outlier(이상치) 가 문제
cdata$cluster <- as.factor(km$cluster)
qplot(MONEY, VISIT, colour = cluster, data=cdata)
plot(subset(cdata, select=-c(ID)), col=km$cluster)

### 군집의 밀도 도식
qplot(MONEY, colour=cluster, data=cdata, geom="density")

#################### SOM ###########################
library(kohonen)
cdata2 <- read.delim("Cluster.txt", stringsAsFactors = FALSE)
### 데이터 정규화
### 단위가 다른 데이터를 고쳐줌
### SOM을 할 경우 필요
cdata.n <- scale(subset(cdata2, select=-c(ID)))

### grid를 3*3 으로 하는 SOM clustering 
set.seed(1) # 돌릴떄마다 같은 결과를 나타내려고 숫자는 아무거나 씀
sm <- som(data = cdata.n , grid = somgrid(3,3,"rectangular")) # row, column , 사각형
str(sm)
## pts   : int [1:9, 1:2] 1 2 3 1 2 3 1 2 3 1 . 1:9 는 군집번호 , 1:2 는 X,Y
## unit.classif: int [1:1000] 9 8 5 8 6 5 4 5 9 4 ...각각의 사람이 어떤 군집에 속하는지
plot(sm, main="B department")

### ggplot2 패키지를 이용한 grid 도식
cdata2$clusterX <- sm$grid$pts[sm$unit.classif,"x"] # x 값
cdata2$clusterY <- sm$grid$pts[sm$unit.classif, "y"] # y 값
p <- ggplot(cdata2, aes(clusterX, clusterY)) 
p+geom_jitter(position = position_jitter(width=.2, height=.2))
##################################################################################################################