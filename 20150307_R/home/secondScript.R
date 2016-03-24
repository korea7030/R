##--------------------------------------------------
## scrpit that Read the spred sheet and analize it
##--------------------------------------------------
?read.delim
HealthSys <- read.delim("HealthSys.txt", quote="", header=T, comment.char="#", as.is=T, check.names=F)
View(HealthSys)

##-- 각 열별 요약정보 
summary(HealthSys)

HealthSys <- read.table("HealthSys.txt", header=T, sep="\t", quote="", as.is=T, check.names=F, comment.char="#")
GDP <- read.table("GDP.txt", header=T, sep="\t", quote="", as.is=T, check.names=F, comment.char="#")
View(HealthSys)
View(GDP)

## 데이터의 열(변수)과 행(항목)을 조회
dim(HealthSys)  ## 15개의 변수와 214개의 항목
dim(GDP)        ## 8개의 변수와 214개의 항목

## 데이터의 열의 이름
names(HealthSys)
colnames(GDP)

## 데이터 요약
summary(HealthSys)
summary(GDP)

## 데이터의 열(변수) 접근
## 1. HealthSys[[6]] -- 열의 위치를 알 경우 사용
## 2. HealthSys[["PublicHealthExpensePercTotal"]]  -- 열 이름에 특수문자가 있을 경우 사용
## 3. $를 사용
HealthSys$PublicHealthExpensePercTotal
GDP$OECD
summary(GDP$GDPperCapita2012)

## 2012년 일인당 GDP 최대(NA 빼고 계산)
max(GDP$GDPperCapita2012, na.rm=TRUE)

## 2012년 일인당 GDP 최소(NA 빼고 계산)
min(GDP$GDPperCapita2012, na.rm=T)

##전체 데이터의 행 이름을 지정
rownames(GDP) <- GDP$CountryCode

## GDP의 KOR 행 이름에 대한 GDPperCapita2012 열을 지정
GDP["KOR", "GDPperCapita2012"]

## 파일을 읽을 시 행이름 지정 방법(row.names=(행번호))
GDP <- read.table(<…파일…>, header=T, sep=“\t”, quote=“”, as.is=T, check.names=F, comment.char=“#”, row.names=2)
                  
## OECD 국가들의 2012년 1인당 GDP를 보여줘
GDP$GDPperCapita2012[GDP$OECD == "Y"]

## 2012년 일인당 GDP가 10000이 넘는 나라는? 
## which(A) -> A에서 NA 나 FALSE말고 TRUE인 값만 골라줌
GDP$Country[which(GDP$GDPperCapita2012 > 10000)]

## 한국 GDP를 변수 지정 
KORGDP <- GDP$GDPperCapita2012[GDP$CountryCode=='KOR']
GDP$Country[which(GDP$GDPperCapita2012 > KORGDP)]

## HealthSys 가운데 OECD의 것만 사용
ind <- which(HealthSys$OECD == "Y")
HealthSysOECD <- HealthSys[ind,]  ## HealthSys가운데 ind에 있는 것만 사용
dim(HealthSysOECD)

## 우리나라 건강관리 관련 전체 지출 중 공공부문이 차지하는 비율을 전세계의 다른 나라와 비교하라.
## 1. 전체 국가중 공공부문 지출 순위는?
indkor <- which(HealthSys$CountryCode=="KOR")
pubkor <- HealthSys$PublicHealthExpensePercTotal[indkor]
pubkor  ## 54.4

## 전체 국가의 공공부문 지출 요약
summary(HealthSys$PublicHealthExpensePercTotal)

## rank 순위 구하기?
## na.last = TRUE 가 기본 값 -> 이건 NA 포함하여 계산한 rank이다를 나타냄
?rank
rank(HealthSys$PublicHealthExpensePercTotal)[indkor]  ## 아래에서 74위
## 2. OECD국가 중 공공부문 지출 지출 순위는? 

## 공공부문 지출이 우리보다 적은 나라 가운데 우리나라보다 일인당 GDP가 큰 나라는 얼마나 될까 ?
HealthSys$Country[HealthSys$PublicHealthExpensePercTotal < pubkor && GDP$GDPperCapita2012 > KORGDP]

## OECD 대상 몇번째 인지 
indkoroecd <- which(HealthSysOECD$CountryCode == "KOR")
rank(HealthSysOECD$PublicHealthExpensePercTotal)[indkoroecd]  ## 밑에서 5번째

dim(HealthSysOECD)

summary(HealthSysOECD$PublicHealthExpensePercTotal)
