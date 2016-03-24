# 1. 제공된 csv파일 read--------

basketData <- read.csv("basketData.csv", header=T, stringsAsFactors=F)
customerDb <- read.csv("customerDb.csv", header=T, stringsAsFactors=F)
library(dplyr)

# 2. 구매이력을 가진 고객 수 산정 --------
Count_customer_receipt <- basketData %>%
  group_by(custId) %>%
  summarize(N=n())


dim(Count_customer_receipt)
length(unique(Count_customer_receipt$custId))


# 3. 미구매 고객 추출---------

# 추가적인 마케팅을 하기위해서
# 필요하므로, 고객리스트를 출력하여 주시기 바랍니다.

# join을 할 경우에는 조인명령어에서 좌측에 놓을 데이터셋과 우측에 놓을 데이터셋을
# 잘 구분하여서 넣어 주어야 합니다.
# 만약에 이 순서가 바뀌면 구매이력이 있는 고객만 남게 될 수도 있습니다.
# 이 경우에는 customerDb를 왼쪽에 놓아야, left_join시에 구매이력이 없는
# 고객이 NA로 처리되어 남게 됩니다.
# 이렇게 한 후, is.na()함수를 통해 구매이력이 없는 고객, 즉, 고객정보는 있으나
# 구매이력이 없는 잠재 고객을 추출하여 마케팅을 할 수 있게 됩니다.
# 익숙치 않으면 실수할 가능성이 높으니 많은 연습이 필요합니다.

joindummy <- left_join(customerDb, Count_customer_receipt)
head(joindummy)

# join결과가 없는 고객들만 추출합니다.
joindummy[is.na(joindummy$N),]

# 이렇게 dimension을 보면 몇분이나 되시는지 알 수 있게 됩니다.
dim(joindummy[is.na(joindummy$N),])


# 4. userRFM.RData loading---------- 

load("userRFM.RData")


##### 객단가 산출을 위한 단계별 문제 풀이


# 5. 지점별 고객등급별 매출 Aggregation------

# 데이터를 Aggregation할 경우에는 최종결과문을 염두에 두고,
# 집계함수를 설계하는 것이 중요합니다.

# 일단 문제가 지점별 고객 등급별이 이기 때문에,
# 지점별 정보를 가지고 있는 데이터를 찾아,
# 고객 등급을 붙여서 문제를 풀 수 있는 기본 데이터 셋을
# 만들어야 겠다는 생각을 하는 것이 중요하고,
# 또한 이를 어떻게 하면 할 수 있는지 이해하는 것도 중요합니다.
# 이러한 구상을 못하시면 아예 이미 알고 있는 dplyr을 어떻게
# 사용하면 좋을지 알 수 없게 되거나,
# join을 할 때, 내가 원하는 정보만 골라서 붙이는
# 방법을 모르시면, 너무 데이터가 많아져서 당황하게 될 수도 있습니다.
# 이런 경우를 피하거나, 타개할 수 있는 능력을 키웠는지 확인하는
# 문제입니다.



head(userRFM)

# join할 때, 좌/우측에 무엇을 놓는 것이 중요한가는 말씀을 드렸습니다만,
# 우측에 놓이는 부분에 내가 붙이고자 하는 key가 되는 column명과 
# 목표가되는 column명인 grade만을 지정해 주면,
# 당장은 필요없는 정보가 제외되고, 내가 원하는 정보만 join시켜 사용이 
# 가능합니다.
# 데이터가 적을 때는 모르지만, 데이터가 많아지면,
# 이러한 기술들이 아주 중요하게 됩니다.

basketGrade <- left_join(basketData, 
                         userRFM[,c("custId","grade")])

# 기존의 basketData에 고객 등급 Data가 붙어져 있는 것을 확인할 수 있습니다.
head(basketGrade)

# 이제 고객등급별 지점별 데이터를 추출하기 위해 summarize를 진행합니다.
# ARPU, 즉 객단가는 총 매출액을 고객의 수로 나누어야 하기 때문에
# summarize를 매출로 한 번한 후, 고객 수를 카운트 하는 summarize를 한번더해서
# 이들 결과치를 나누어 주면, 수치를 얻을 수 있습니다.
# 다만 이렇게 할 때, 서로간의 데이블 형태가 틀리면 계산을 하는데, 손으로 해야 하거나
# 하는데, 이런 수고를 덜기위해서 각각의 데이터셋의 행과 열의 수와 순서를 동일하게
# 만들어 두면 나중에 손쉽게 객단가 계산이 가능하게 됩니다. 

# 나중에 cast할 것을 감안하여 grade와 branchId로 groupby를 한후,
# 이를 summarize해두면 손쉽게 고객등급별 지점별 데이터를 얻을 수 있게 됩니다.
# 물론 다른 방법도 있으시겠습니다만, 그렇게 되면 그에 따라 
# 이후의 데이터를 처리하는 방법도 달라져야 합니다.

# 나중에 이해하기 쉽도록 summarize한 결과를 sales라는 
# 이름으로 집계하였습니다.

salesData <- basketGrade %>%
  group_by(grade, branchId) %>%
  summarize(sales=sum(amount))

head(salesData)

# 고객등급별 branchId별로 정리하기위해 다음과 같이 cast를 적용합니다.
library(reshape)
salesCast <- cast(salesData, grade ~ branchId, 
                  value="sales", 
                  fun.aggregate=sum)
salesCast

# 만약에 branchId를 행에 배치하신다면, 다음과 같이 하시면 됩니다.

salesCast2 <- cast(salesData, branchId ~ grade,
                   value="sales",
                   fun.aggregate=sum)

# 적절한 arguments들을 지정된 장소에 지정해 주면,
# 보기편하고 나중에 활용하기에도 용이한 데이터셋을 만드실 수 있게됩니다.
salesCast2

# 6. 고객 등급별 고객수 Count---------

# 데이터 aggregation할 때는 내가 다루는 데이터가 어떤 데이터인지를 분명하게
# 알고 있어야 합니다.
# 현재 basketData는 고객별이 아니라 고객이 발급받은 영수증 번호가 가장 세부단위입니다.
# 따라서 매출을 일으킨 고객수를 추출하기위해서는 먼저 grade별, branchId에다 custId 기준으로
# graoupby를 해서 summarize한 다음, 이 고객수를 다시한번 count해주어야 
# 고객 수를 파악할 수 있게됩니다.
# 한달에 와서 한번 사는 고객이든 매일와서 여러 카테고리의 상품을 사가건,
# 고객 수로는 한 고객으로 보는 것이 맞기 때문입니다.
# 만약이 한달에 와서 한번 사는 소량규매 고객이 많다면 ARPU(객단가)가 낮을 것이고,
# 그 반대라면 ARPU(객단가)가 상대적으로 더 높게 나오게 될 것입니다.
# 이러한 고객군의 특성을 하나의 지표로 볼 수 있기 때문에 ARPU 혹은 객단가라는
# 지표를 개발 하여 사용하는 것입니다.
# 이것만 보면, 고객군이 어떻게 변하는 지, 개략적인 특성을 알 수 있기 때문입니다.



custCountdummy <- basketGrade %>%
  group_by(grade, branchId, custId) %>%
  summarize(custSales=sum(amount))

head(custCountdummy)

custCount <- custCountdummy %>%
              group_by(grade, branchId) %>%
              summarize(count=n())

countCast <- cast(custCount, grade ~ branchId, 
                  value="count", 
                  fun.aggregate=sum)
countCast

# 전체 고객수를 점검해 봅니다.
sum(apply(countCast, 2, sum))

## 위에서 전체 고객을 대상으로한 구매이력 고객은
## 2,089명입니다만, 이를 고객등급별도 구매 지점별로
## 나누어 보면 2,097명입니다. 약 8명의 고객은
## 여러 지점에서 물품을 구매하는 고객으로 판단됩니다.



# 7. 객단가(ARPU) 계산-----------

# 위에서 매출액과 고객수를 계산했으므로, 이제 나누어 주기만 하면됩니다.
# 어떻게 나누면 될까요?

# 만약에 위와같이 동일한 행수와 열수를 가진 데이터로 포맷을 맞추어 놓았다면,
# 상대적으로 수월하게 계산됩니다.

# 그런데 위와 같이 동일한 포맷으로 cast되어 있지 않고, 일렬로 되어 있거나,
# 일렬로 되어 있어도 순서가 동일한가 아닌가에 따라, ARPU 또는 객단가  계산이
# 가능할 수도 가능하지 않을 수도 있습니다.
# 따라서 최초에 summarize할 때부터, 최종적인 산출물을 어떠한 구조로
# 할지 설계를 해놓고, 분석을 시작해야 한다는 점을 느끼실 수 있기를 바랍니다.

# 일단, 포맷이 동일한 바,
# ARPU 또는 객단가를 기록한 결과물 파일을 만듭니다.


ARPU <- countCast

# 그리고, 데이터 프레임 indexing원칙에 따라,
# 데이터 프레임내의 각각의 원소를 매출액과 구매고객수로 나눈 값으로
# 채워질 수 있도록 공식을 만들되, 동일한 포맷으로 정리가 되어 있는바,
# for 문을 돌려서 빠르게 종료할 수 있도록 합니다.
# 그리고 동시에 결과값을 반올림하여 나타낼 수 있도록 round함수를 
# 적용합니다.

for (i in 2:5 ) {
  ARPU[,i] <- round(salesCast[,i] / countCast[,i],0)
}

# 고객등급별 지점별 분기별 객단가는 다음과 같습니다.
ARPU

# 이를 주간 단위 객단가로 바꾸려면 어떻게 하면 될까요?

# 분기는 3개월, 3개월은 12주이므로, 분기별 객단가를 12로 나누면
# 주간 객단가가 됩니다.

weekARPU <- ARPU 
weekARPU[,2:5] <- round(ARPU[,2:5]/12,0)

## 결과를 확인해 보시면 branch_02의 A고객의 ARPU 또는 객단가가
## 가장 높은 수준을 보입니다. 그리고 branch_04가 B, C, D 고객군의
## ARPU 또는 객단가가 높아, 중산층이 밀집된 아파트 단지가
## 아닐까 하는 추측을 가능하게 합니다.

weekARPU

## 수고 많으셨습니다.
## 이번 주말까지 부여해드린 마케팅기획 수립 과제 리포트를
## 잊지말고 제출하여 주시기 바랍니다.

## 장운호 삼가드림.
