library(dplyr)
library(lubridate)
# install.packages("arules")
library(arules)
# install.packages("arulesViz")
library(arulesViz)

## 서울지역 그래프 그리기 위한 data 작업 시작 ##########################
group_region <- group_by(merge_data2, 거주지역, 상품대분류명)
group_region.summ <- summarize(group_region, sumAmount = sum(구매금액))

group_region_buy <- group_by(merge_data2, 구매지역, 상품대분류명)
group_region_buy.summ <- summarize(group_region_buy, sumAmount = sum(구매금액)) 

head(group_region_buy.summ)

unique(group_region.summ$거주지역)
group_region.summ_seoul <- group_region.summ[substr(group_region.summ$거주지역, 1, 2)=="서울",]
group_region_buy.summ.seoul <- group_region_buy.summ[substr(group_region_buy.summ$구매지역, 1,2)== "서울", ]

goo_nm <- unique(group_region.summ_seoul$거주지역)
goo_nm2 <- unique(group_region_buy.summ.seoul$구매지역) 

for (i in 1:length(goo_nm)) {
  assign(paste("group",i,sep=""), group_region.summ_seoul[group_region.summ_seoul$거주지역==goo_nm[i],], envir = .GlobalEnv)
}

for (j in 1:length(goo_nm2)) {
  assign(paste("group.buy",j,sep=""), group_region_buy.summ.seoul[group_region_buy.summ.seoul$구매지역 == goo_nm2[j],], envir=.GlobalEnv)
}

## 서울지역 그래프 그리기 위한 data 작업 끝 ##########################

### 지역별 pie 그래프  함수 시작(거주지역, 구매지역) #########################################
options(scipen=999)

createPlot <- function(goo_nm, text) { 
  for ( i in 1:length(goo_nm)) {
    
    data <- eval(parse(text=paste0("group",i)))
    filename <- paste(goo_nm[i],"거주지역 금액.jpeg" , sep=" ")
    
    jpeg(filename = filename, width = 900, height = 700)
    print ({
      ggplot(data, aes(x = "", y = sumAmount , fill = 상품대분류명)) +
        geom_bar(stat="identity", position = "fill", width=1)+
        coord_polar(theta="y")+
        ggtitle(paste(goo_nm[i], "거주지역 금액", sep=" "))+
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              panel.background = element_blank())
    })
    dev.off()
  }
}

createPlot2 <- function(goo_nm2, text) { 
  for ( i in 1:length(goo_nm2)) {
    
    data <- eval(parse(text=paste0("group",i)))
    filename <- paste(goo_nm2[i],"구매지역 금액.jpeg" , sep=" ")
    
    jpeg(filename = filename, width = 900, height = 700)
    print ({
      ggplot(data, aes(x = "", y = sumAmount , fill = 상품대분류명)) +
        geom_bar(stat="identity", position = "fill", width=1)+
        coord_polar(theta="y")+
        ggtitle(paste(goo_nm2[i], "구매지역 금액", sep=" "))+
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              panel.background = element_blank())
    })
    dev.off()
  }
}
### 지역별 pie 그래프  함수 끝(거주지역, 구매지역)#############################################

createPlot(goo_nm, "거주지역 금액.jpeg")
createPlot2(goo_nm2, "구매지역 금액.jpeg")

####### 연령대, 구매월, 구매요일 변수 추가 및 그래프 시작 ####################################
merge_data2 <- within(merge_data2, {
  연령대 = character(0)
  연령대[ 연령  >= 50 & 연령 < 60] = "50대"
  연령대[ 연령  >= 60 & 연령 < 70] = "60대"
  연령대[ 연령  >= 70 & 연령 < 80] = "70대"
  연령대[ 연령  >= 80 & 연령 < 90] = "80대"
  연령대[ 연령  >= 90 & 연령 < 100] = "90대"
  
  # 연령대 = factor(연령, level=c("10대","20대","30대","40대","50대","60대","70대","80대","90대"))
})


merge_data2$구매월 <- month(merge_data2$구매일자)
merge_data2$구매요일 <- wday(merge_data2$구매일자, label=TRUE)


head(merge_data2)
ggplot(merge_data2, aes(구매월))+geom_bar()+facet_wrap(~성별)+ggtitle("성별 구매건수")
ggplot(merge_data2, aes(구매월))+geom_bar()+facet_wrap(~연령대)+ggtitle("연령대 구매건수")
ggplot(merge_data2, aes(구매월))+geom_bar()+facet_wrap(~성별+연령대)+ggtitle("성별/연령대 구매건수")
ggplot(merge_data2, aes(구매월))+geom_bar()+facet_wrap(~연령대+성별)+ggtitle("연령대/성별 구매건수")


agg_dat <- aggregate(구매금액~ID+구매월, data=merge_data2, sum)
ggplot(agg_dat, aes(구매월, 구매금액)) + geom_boxplot(aes(group=구매월))
ggplot(subset(agg_dat, 구매금액 < 20000000), aes(구매월, 구매금액)) + geom_boxplot(aes(group=구매월))

agg.summ <- aggregate(구매금액~구매요일, data=merge_data2, sum)
ggplot(data= agg.summ, aes(x=구매요일, y= 구매금액))+geom_bar(stat="identity")

ggplot(merge_data2, aes(구매요일))+geom_bar()+facet_wrap(~성별)+ggtitle("성별 구매건수")
ggplot(merge_data2, aes(구매요일))+geom_bar()+facet_wrap(~연령대)+ggtitle("연령대 구매건수")
ggplot(merge_data2, aes(구매요일))+geom_bar()+facet_wrap(~성별+연령대)+ggtitle("성별/연령대 구매건수")
ggplot(merge_data2, aes(구매요일))+geom_bar()+facet_wrap(~연령대+성별)+ggtitle("연령대/성별 구매건수")
####### 연령대, 구매월, 구매요일 변수 추가 및 그래프 끝 ####################################

#################################### 연관분석 ID 기준(전체)#########################################


group_mid <- gorup_by(merge_data2, ID, 상품중분류명) 
group_mid.summ <- summarize(group_mid, sumAmount = length(상품중분류명))
head(group_mid.summ)

tmp_df <- group_mid.summ[,c(1,2)]
head(tmp_df)

tmp_df.list <- split(tmp_df$상품중분류명, tmp_df$ID)
head(tmp_df.list)

tmp_df.trans <- as(tmp_df.list, "transactions")
tmp_df.trans

summary(tmp_df.trans)  

image(tmp_df.trans)

tmp_df.rules <- apriori(tmp_df.trans)  ## support 와 confidence 가 default일때(0.1, 0.8) 
tmp_df.rules <- apriori(tmp_df.trans, parameter = list(minlen = 2, support = 0.2 , confidence = 0.6), control=list(verbose=F))
tmp_df.rules.profood <- apriori(tmp_df.trans, parameter = list(minlen = 2, support = 0.2 , confidence = 0.6), control=list(verbose=F), appearance=list(rhs="가공식품", default="lhs"))
tmp_df.rules.fcloth <- apriori(tmp_df.trans, parameter = list(minlen = 2, support = 0.2 , confidence = 0.6), control=list(verbose=F), appearance=list(rhs="여성용의류-SPA", default="lhs"))

## 전체 데이터 중복 제거
subset.matrix.clust.full <- is.subset(tmp_df.rules, tmp_df.rules)
subset.matrix.clust.full[lower.tri(subset.matrix.clust.full, diag=T)] <- NA
redundant.clust.full <- colSums(subset.matrix.clust.full, na.rm=T) >= 1
which(redundant.clust.full)

tmp_df.rules.pruned <- tmp_df.rules[!redundant.clust.full]
inspect(tmp_df.rules.pruned)
####

summary(tmp_df.rules)  
# summary(tmp_df.rules.profood)
# summary(tmp_df.rules.fcloth)
# head(sort(tmp_df.rules, by="lift"))
inspect(tmp_df.rules.pruned, by="lift")
inspect(tmp_df.rules.profood, by="lift")
inspect(tmp_df.rules.fcloth, by="lift")



plot(tmp_df.rules.pruned)
plot(tmp_df.rules.pruned, method="grouped")
plot(tmp_df.rules, method="graph")
plot(tmp_df.rules, method="graph", control = list(type = "items"))
plot(tmp_df.rules, method="paracoord", control = list(reorder = TRUE))

##################### 연관분석 ID기준(전체) 끝 #################################

##################### RFM 데이터를 가지고 군집분석 시작########################
load(file = "RFM_data.RData")
head(userRFM)
head(userRFM.with.data)

userRFM_split <- userRFM.with.data[,c(3,4,6,13)]
head(userRFM_split)

km <- kmeans(scale(userRFM_split[,-4]), 5)
km

plot(recency~frequency, data=userRFM_split[,-4], pch=km$cluster, col= km$cluster)
text(userRFM_split$frequency, userRFM_split$recency, userRFM_split$grade, cex=0.8, col=km$cluster)

plot(recency~monetary, data=userRFM_split[,-4], pch=km$cluster, col= km$cluster)
text(userRFM_split$monetary, userRFM_split$recency, userRFM_split$grade, cex=0.8, col=km$cluster)

plot(monetary~frequency, data=userRFM_split[,-4], pch=km$cluster, col= km$cluster)
text(userRFM_split$frequency, userRFM_split$monetary, userRFM_split$grade, cex=0.8, col=km$cluster)
##################### RFM 데이터를 가지고 군집분석 끝########################

##################### 군집별로 데이터 나눈 후 연관분석 시작(군집별) #####################
head(userRFM.with.data)
userRFM.with.data.clust1 <- userRFM.with.data[userRFM.with.data$cluster == 1,]
userRFM.with.data.clust2 <- userRFM.with.data[userRFM.with.data$cluster == 2,]
userRFM.with.data.clust3 <- userRFM.with.data[userRFM.with.data$cluster == 3,]
userRFM.with.data.clust4 <- userRFM.with.data[userRFM.with.data$cluster == 4,]
userRFM.with.data.clust5 <- userRFM.with.data[userRFM.with.data$cluster == 5,]

group.clust1 <- group_by(userRFM.with.data.clust1, ID, 상품중분류명)
group.clust1.summ <- summarize(group.clust1, sumAmount = length(상품중분류명))
head(group.clust1.summ)
clust1_df <- group.clust1.summ[,c(1,2)]
clust1_df.list <- split(clust1_df$상품중분류명, clust1_df$ID)
clust1_df.trans <- as(clust1_df.list, "transactions")
summary(clust1_df.trans)
clust1_df.rules <- apriori(clust1_df.trans, parameter = list(minlen = 2, support = 0.2 , confidence = 0.8), control=list(verbose=F))

## 중복 제거 
subset.matrix.clust1 <- is.subset(clust1_df.rules, clust1_df.rules)
subset.matrix.clust1[lower.tri(subset.matrix.clust1, diag=T)] <- NA
redundant.clust1 <- colSums(subset.matrix.clust1, na.rm=T) >= 1
which(redundant.clust1)

clust1_df.rules.pruned <- clust1_df.rules[!redundant.clust1]
inspect(clust1_df.rules.pruned)
## 

summary(clust1_df.rules.pruned)
inspect(clust1_df.rules.pruned, by="lift") 
plot(clust1_df.rules.pruned, method="grouped")

###############################################################################################################

group.clust2 <- group_by(userRFM.with.data.clust2, ID, 상품중분류명)
group.clust2.summ <- summarize(group.clust2, sumAmount = length(상품중분류명))
head(group.clust1.summ)
clust2_df <- group.clust2.summ[,c(1,2)]
clust2_df.list <- split(clust2_df$상품중분류명, clust2_df$ID)
clust2_df.trans <- as(clust2_df.list, "transactions")
summary(clust2_df.trans)
clust2_df.rules <- apriori(clust2_df.trans, parameter = list(minlen = 2, support = 0.8 , confidence = 0.4), control=list(verbose=F))

## 중복 제거 
subset.matrix.clust2 <- is.subset(clust2_df.rules, clust2_df.rules)
subset.matrix.clust2[lower.tri(subset.matrix.clust2, diag=T)] <- NA
redundant.clust2 <- colSums(subset.matrix.clust2, na.rm=T) >= 1
which(redundant.clust2)

clust2_df.rules.pruned <- clust2_df.rules[!redundant]
inspect(clust2_df.rules.pruned)
## 

summary(clust2_df.rules.pruned)
inspect(clust2_df.rules.pruned, by="lift") 
plot(clust2_df.rules.pruned, method="grouped")

##############################################################################################################

group.clust3 <- group_by(userRFM.with.data.clust3, ID, 상품중분류명)
group.clust3.summ <- summarize(group.clust3, sumAmount = length(상품중분류명))
head(group.clust3.summ)
clust3_df <- group.clust3.summ[,c(1,2)]
clust3_df.list <- split(clust3_df$상품중분류명, clust3_df$ID)
clust3_df.trans <- as(clust3_df.list, "transactions")
summary(clust3_df.trans)
clust3_df.rules <- apriori(clust3_df.trans, parameter = list(support = 0.05 , confidence = 0.4), control=list(verbose=F))

## 중복 제거 
subset.matrix.clust3 <- is.subset(clust3_df.rules, clust3_df.rules)
subset.matrix.clust3[lower.tri(subset.matrix.clust3, diag=T)] <- NA
redundant.clust3 <- colSums(subset.matrix.clust3, na.rm=T) >= 1
which(redundant.clust3)

clust3_df.rules.pruned <- clust3_df.rules[!redundant.clust3]
inspect(clust3_df.rules.pruned)
## 

summary(clust3_df.rules.pruned)
inspect(clust3_df.rules.pruned, by="lift") 
plot(clust3_df.rules.pruned, method="grouped")

###############################################################################################################

group.clust4 <- group_by(userRFM.with.data.clust4, ID, 상품중분류명)
group.clust4.summ <- summarize(group.clust4, sumAmount = length(상품중분류명))
head(group.clust4.summ)
clust4_df <- group.clust4.summ[,c(1,2)]
clust4_df.list <- split(clust4_df$상품중분류명, clust4_df$ID)
clust4_df.trans <- as(clust4_df.list, "transactions")
summary(clust4_df.trans)
clust4_df.rules <- apriori(clust4_df.trans, parameter = list(minlen = 2, support = 0.95, confidence = 0.2), control=list(verbose=F))

## 중복 제거 
subset.matrix.clust4 <- is.subset(clust4_df.rules, clust4_df.rules)
subset.matrix.clust4[lower.tri(subset.matrix.clust4, diag=T)] <- NA
redundant.clust4 <- colSums(subset.matrix.clust4, na.rm=T) >= 1
which(redundant.clust4)

clust4_df.rules.pruned <- clust4_df.rules[!redundant.clust4]
inspect(clust4_df.rules.pruned)
## 

summary(clust4_df.rules.pruned)
inspect(clust4_df.rules.pruned, by="lift") 
plot(clust4_df.rules.pruned, method="grouped")

##############################################################################################################
group.clust5 <- group_by(userRFM.with.data.clust5, ID, 상품중분류명)
group.clust5.summ <- summarize(group.clust5, sumAmount = length(상품중분류명))
head(group.clust5.summ)
clust5_df <- group.clust5.summ[,c(1,2)]
clust5_df.list <- split(clust5_df$상품중분류명, clust5_df$ID)
clust5_df.trans <- as(clust5_df.list, "transactions")
summary(clust5_df.trans)
clust5_df.rules <- apriori(clust5_df.trans, parameter = list(minlen = 2, support = 0.2 , confidence = 0.8), control=list(verbose=F))

summary(clust1_df.rules)
inspect(clust1_df.rules, by="lift") 
plot(clust1_df.rules, method="grouped")
##############################################################################################
##################### 군집별로 데이터 나눈 후 연관분석 끝(군집별) ################################