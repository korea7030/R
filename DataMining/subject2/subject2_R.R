catalog <- read.csv('catalogCrossSell.csv', stringsAsFactors = FALSE)
head(catalog)
summary(catalog)
catalog <- catalog[,-c(1,4)] # health의 경우 산 경우만 있기 때문에 제외
head(catalog)

### jaccard coefficient 관련 package ################################
# install.packages("prabclus")
library(prabclus)
jaccard(as.matrix(catalog))

## 의류, 가정용품, 자동차, 전자제품, 컴퓨터, 원예, 선물, 쥬얼리(1,2,3,4,5,6,7,8)
### 각각 용품별 상품을 구분 했으나 의류와 상관계수가 높은 용품이 거의 없다고 판단하여 전체를 넣고 kmodes 수행 ###
catalog1 <- catalog

## 최적의 군집수 찾기(명목형 변수)
# library(klaR)
set.seed(1)
wss <- 0
for (i in 1:15) wss[i] <- kmodes(catalog1, i)$withindiff # 군집내 분산값
### 기울기가 급격하게 감소하는 지점에서 군집의 수 지정
plot(1:15, wss, type="b", xlab = "# of clusters ", ylab = "Within group sum of squares") # 최적의 군집 : 12

#### 

### 최적의 군집수 찾은 후 군집 진행
km <- kmodes(catalog1, 12)
clusplot(catalog1, km$cluster)

### 

### SOM 군집 활용
### 12개의 SOM으로 최대한 나타낼 수 있는 r과 c는 각각 3 으로 지정
library(kohonen)
str(catalog1)
set.seed(1) 
catalog1.n <- scale(catalog1)
sm <- som(data = catalog1.n , grid = somgrid(3,3,"rectangular"), rlen=13) # row, column , 사각형
str(sm)
plot(sm, main="Catalog")

### 군집별 세분화변수 표시
catalog1$cluster <- sm$unit.classif
plot(xtabs(~cluster+Clothing, data=catalog1), main="Clothing plot")
plot(xtabs(~cluster+Housewares, data=catalog1), main="Housewares plot")
plot(xtabs(~cluster+Automotive, data=catalog1), main="Automotive plot")
plot(xtabs(~cluster+Electronics, data=catalog1), main="Electronics plot")
plot(xtabs(~cluster+Computers, data=catalog1), main="Computers plot")
plot(xtabs(~cluster+Garden, data=catalog1), main="Garden plot")
plot(xtabs(~cluster+Gift, data=catalog1), main="Gift plot")
plot(xtabs(~cluster+Jewelry, data=catalog1), main="Jewelry plot")

### x,y 좌표 표시(SOM 좌표값으로 나누어지는 군집 확인하고자 그림)
library(ggplot2)
catalog1$clusterX <- sm$grid$pts[sm$unit.classif,"x"] # x 값
catalog1$clusterY <- sm$grid$pts[sm$unit.classif, "y"] # y 값
p <- ggplot(catalog1, aes(clusterX, clusterY)) 
p+geom_jitter(position = position_jitter(width=.2, height=.2))
