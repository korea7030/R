getwd()
setwd("C:/Users/leejh/Documents/R/20150321_R")
getwd()


##### 20150321 ?닔?뾽吏꾪뻾 #####

## quiz ?궡?슜 ##
1)
sum(5:5)  ## 5?뿉?꽌5 源뚯? index 
sqrt(100)
sum(0:3+1)
sum(0:(3+1))
10^2/10

2)
myName <- "Turing"
hisNam <- "Tukey"
tmpName <- myName
myName <- hisNam
hisNam <- tmpName
myName
hisNam

3)
1:3+1

4)
x<-1
y<- c(1,"one")
mode(y)
mode(x)

5)
Probability <- 1/(1+exp(-x))

6)
quizData <- seq(from=-150, by=3, to=150)
evenQuizData <- quizData[which(quizData %% 2 == 0)]
length(evenQuizData)

sum(evenQuizData)

7)
oddQuizData <- quizData[which(quizData %% 2 == 1)]
oddQuizData

sum(oddQuizData)

8)
mean(quizData)


quizData
?mean

9)
quizData[3] <- 500
print(quizData[3])

quizData <- c(quizData[1:2], 500, quizData[3:length(quizData)])

####################
#### Matrix
## 1遺?꽣 20源뚯? ?닽?옄瑜? 4?뻾 5?뿴 ?삎?깭?쓽 matrix濡? 留뚮뱾?뼱.
## ?뿴踰≫꽣 ?슦?꽑 ?썝移? 泥섎━
## byrow=TRUE ?떆 ?쁿?쑝濡? 諛곗뿴 ?맖 .
## ?뿴?씠由? 吏?젙 媛?뒫(?븿?닔)
## ?뻾,?뿴 吏?젙?? ?븯?굹留? 吏?젙?빐?룄 留욎떠?꽌 異쒕젰?맂?떎. 

(myMatrix <- matrix(1:20, nrow=4, ncol=5, byrow=TRUE))  
myMatrix

## 留ㅽ듃由??뒪?쓽 湲몄씠蹂대떎 ?닽?옄 踰붿쐞媛 ?옉?? 寃쎌슦 ?닽?옄?쓽 媛?옣 ?옉?? ?닽?옄遺?꽣 梨꾩썙吏꾨떎.
myMatrix <- matrix(1:19, nrow=4, ncol=5)
myMatrix

#### Matrix ?걹

#### List 
## list ?깮?꽦 ?떆 由ъ뒪?듃濡? 吏?젙?븷 媛곴컖?쓽 蹂?닔?뿉 ???븳 ?씠由꾩쓣 諛섎뱶?떆 ?꽔?뼱?빞 ?븳?떎. 
## [[2]] 寃? ??愿꾪샇濡? indexing 
c1NAme <- "?옣?슫?샇"
c1Age <- 46
c1hobby <- "?벑?궛"
c1Visit <- c("愿?븙?궛", "遺곹븳?궛", "泥?怨꾩궛")
customerDatabase <- list(name=c1NAme, age=c1Age, hobby=c1hobby, visit=c1Visit)
customerDatabase

## visit?쓽 遺곹븳?궛?쓣 戮묎퀬 ?떢?떎!! 
customerDatabase[[4]][2]  ## [[4]] ?븷 寃쎌슦 vector瑜? 戮묒븘?삤寃? ?릺誘濡?, ?뮘?뿉 [2]瑜? ?꽔寃? ?릺硫? 遺곹븳?궛?쓣 戮묎쾶 ?맂?떎. 

#### List ?걹

#### DataFrame 
## list濡? ?맂 ?뜲?씠?꽣?쓽 湲몄씠媛 ?떎 媛숈? 寃쎌슦 DataFrame
## 紐⑤뱺 而щ읆?? 湲몄씠媛 媛숈븘?빞 ?븳?떎. (Null媛믪쓣 ?꽔?뼱?룄 ?맖)
## ?븳?뿴?? 媛숈? ?옄猷뚰삎?씠?뼱?빞 ?븳?떎. 
## 泥ロ뻾(?뀒?씠釉? ?젣紐?) - 蹂?닔, ?냽?꽦(myFamilyNames ... )
## 洹? 諛? 遺遺?(?뀒?씠釉? ?뜲?씠?꽣) - case, ?궗濡, ?뒠?뵆(Dad, Mom, sis ... )

myFamilyNames <- c("Dad", "Mom", "sis", "Bro", "Dog")
myFamilyAges <- c(43,42,12,8,5)
myFamilyGenders <- c("Male", "Female", "Female", "Male", "Female")
myFamilyWeights <- c(188, 136, 83, 61, 44)
myFamily <- data.frame(myFamilyNames, myFamilyAges, myFamilyGenders, myFamilyWeights)

myFamily
myFamily[1,2]  ## dataframe?궡遺?쓽 蹂?닔 李얘린
myFamily[,2]  ##  紐⑤뱺 ?뻾?쓽 ?빐?떦 ?뿴?쓣 媛?졇??

str(myFamily) ## dataframe ?쓽 援ъ“
str(myFamily, stringsAsFactors=FALSE);  ##臾몄옄?뿴濡쒕쭔 

#### DataFrame 