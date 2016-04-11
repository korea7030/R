bball <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", header = T)
head(bball)
# rownames 변경
rownames(bball) <- bball[,1]
bball <- bball[,-1]

bball_mat <- as.matrix(bball) # matrix 화 필요

# 변수간의 scale 차이가 큼
# 선수들 간의 상대적인 특징을 원한다면 변경해줘야함
summary(bball_mat)
heatmap(bball_mat, scale = 'column', Colv = NA)
# 덴드로그램 없이 그리면 그냥 나옴
heatmap(bball_mat[,c(2,1,3:20)], scale = 'column', Colv = NA, Rowv =NA) 

## G로 sort
bball_mat_sort <- bball_mat[order(bball_mat[,1]), ]
heatmap(bball_mat_sort[,c(2,1,3:20)], scale = 'column', Colv = NA, Rowv =NA) 

###################### practice 5 ###########################################
sales <- read.csv("sales.csv", header = T)
library(plyr)
library(dplyr)
library(psych)
head(sales)
sales_plus <- sales[sales$Price > 0,]

demo = read.csv("silver_demo.csv", header = T)

sales_plus_m <- merge(sales_plus, demo, by="ID")
head(sales_plus_m)
CT2_sales <- ddply(sales_plus_m, .(CT2), summarize, 총구매수량= sum(Amount), 총구매금액=sum(Price), 구매횟수=length(ID), 평균구매금액 = mean(Price), 구매고객=length(unique(ID)), 평균구매나이 = mean(Age))
head(CT2_sales)

rownames(CT2_sales) <- CT2_sales$CT2
CT2_sales <- CT2_sales[,-1]

pairs.panels(CT2_sales)
pairs.panels(log(CT2_sales))

CT2_sales[,1:5] <- log(CT2_sales[,1:5])
CT2_sales <- as.matrix(CT2_sales)
CT2_sales <- CT2_sales[order(CT2_sales[,4]),]
heatmap(CT2_sales, scale="column", Colv = NA, margins = c(7,7), Rowv = NA)

## 체르노프 페이스, 나이팅게일 
# install.packages("aplpack")
library(aplpack)
faces(bball)
faces(CT2_sales)

a <- stars(bball, flip.labels = FALSE, cex = 0.8, key.loc = c(13,2))
stars(CT2_sales, flip.labels = FALSE, cex = 0.8, key.loc = c(13,2), draw.segments = TRUE)

## 평행좌표그래프
# 여러 변수들의 패턴을 보기 위한 그래프
education <- read.csv("http://datasets.flowingdata.com/education.csv", header= T)
head(education)
library(lattice)
parallelplot(education[,2:7], horizontal.axis = FALSE, col = 1)

## 중퇴율을 기준으로 상위 25% 확인
summary(education)
col <- rep(1, nrow(education))
col[education$reading > 523] <- 2
col

parallelplot(education[,2:7], horizontal.axis = FALSE, col = col, lwd = col)

####
baseball <- read.csv("baseball_2015.csv", header = T)
head(baseball)

base_col <- rep(1, dim(baseball)[1])
base_col[baseball$RBI > 73] <- 2  
parallelplot(baseball[,c(4:16)], horizontal.axis = FALSE, col = base_col, lwd = base_col)

base_hist<- histogram(~NAME|TEAM, data=baseball, layout = c(10,1))
update(base_hist) ## index 조정