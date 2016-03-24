install.packages("dplyr")
library(dplyr)

custom <- read.csv("customerDb.csv")
basket <- read.csv("basketData.csv")

joinData <- left_join(basket, custom)

head(as.data.frame(joinData))

categoryCustom <- joinData[joinData$category == "daily necessities",]
head(categoryCustom)

babyPurchase <- categoryCustom[categoryCustom$division == "baby products",]
head(babyPurchase)

// 360개의 베이비 프로덕트 구매 데이터
dim(babyPurchase)

babyPurchase <- group_by(babyPurchase, sex)
head(babyPurchase)

MaleCustom <- babyPurchase[babyPurchase$sex == "M",]
FemailCustom <- babyPurchase[babyPurchase$sex == "F",]

babyPurchase <- group_by(babyPurchase, custId, category, division)
babyPurchaseTot <- summarize(babyPurchase, sumAmount = sum(amount))
babyPurchaseTot <- arrange(babyPurchaseTot, desc(sumAmount))

head(babyPurchaseTot)

//38명의 베이비 프로덕트 구매자
dim(babyPurchaseTot)

//남녀 전체 베이비 프로덕트 구매자
write.csv(babyPurchaseTot, file="Total.csv")

//남자  56명(구매금액 합계가 아님)
dim(MaleCustom)

//여자  304명(구매금액 합계가 아님)
dim(FemailCustom)

/// 남자 중에 제일 많이 사는 놈
MaleCustomDiv <- group_by(MaleCustom, custId, category, division) 
MaleCustomDiv2 <- summarize(MaleCustomDiv, sumAmount = sum(amount))

head(MaleCustomDiv2)

MaleCustomDiv3 <- arrange(MaleCustomDiv2, desc=(sumAmount))
head(MaleCustomDiv3)
// C1553  208400
/////////////////////////////////////////

  //여자중에 제일 많이 사는 ㄴ
FeMaleCustomDiv <- group_by(FemailCustom, custId, category, division) 
FeMaleCustomDiv2 <- summarize(FeMaleCustomDiv, sumAmount = sum(amount))

head(FeMaleCustomDiv2)

FeMaleCustomDiv3 <- arrange(FeMaleCustomDiv2, desc=(sumAmount))
head(FeMaleCustomDiv3,32)
// 436000 
////////////////////////////////////////////////

  //남자 데이터(6명)
  write.csv(MaleCustomDiv3, file = "Male.csv")
//여자(32명)
  write.csv(FeMaleCustomDiv3, file="FeMale.csv")
    
////////////////////////////////////////////////////////////////////

NotBabyProduct <- basket[basket$division != "baby products",]
NoyBabyProductgroup <- group_by(NotBabyProduct, custId, category, division)
NotBabyProductSummarize <- summarize(NoyBabyProductgroup, sumAmount = sum(amount))
NotBabyArrange <- arrange(NotBabyProductSummarize, desc(sumAmount))

// 베이비 프로덕트를 사지 않는 고객의 데이터 : 1
dim(NotBabyArrange)
head(NotBabyArrange)

//plot(NotBabyArrange$custId, NotBabyArrange$sumAmount)

powderedMilkData <- NotBabyArrange[NotBabyArrange$division=="powdered milk",]

dim(powderedMilkData)

names(powderedMilkData) <- c("custId", "cate", "div", "sAmount")

data <- left_join(babyPurchaseTot,powderedMilkData) 
data
//////////////////////////////////////////////////////////////////////
#   NotBabyProductMilk <- basket[basket$division == "powdered milk",]
# NotBabyProductMilkgroup <- group_by(NotBabyProductMilk, custId, category, division)
# NotBabyProductMilkSummarize <- summarize(NotBabyProductMilkgroup, sumAmount = sum(amount))
# NotBabyProductMilkArrange <- arrange(NotBabyProductMilkSummarize, desc(sumAmount))

# dim(NotBabyProductMilkArrange)


///////////////////////////////////////////////////////////////////////
  
  
# BabyProduct <- basket[basket$division == "baby products",]
# BabyProductgroup <- group_by(BabyProduct, custId, category, division)
# BabyProductSummarize <- summarize(BabyProductgroup, sumAmount = sum(amount))
# BabyArrange <- arrange(BabyProductSummarize, desc(sumAmount))

# dim(BabyArrange)
