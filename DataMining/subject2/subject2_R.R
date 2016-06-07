catalog <- read.csv('catalogCrossSell.csv', stringsAsFactors = FALSE)
head(catalog)
catalog <- catalog[,-c(1,4)]

## cramer'sV 명목형 변수 상관계수 함수
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

### 의류 상관계수 
with(catalog, cv.test(Clothing, Housewares));with(catalog, cv.test(Clothing, Automotive));with(catalog, cv.test(Clothing, Electronics)); # 의류와 자동차, 전자제품
with(catalog, cv.test(Clothing, Computers));with(catalog, cv.test(Clothing, Garden));with(catalog, cv.test(Clothing, Gift)); # 의류와 원예용품, 의류와 선물
with(catalog, cv.test(Clothing, Jewelry)); # 의류와 쥬얼리
### 가정용품 상관계수 
with(catalog, cv.test(Housewares, Clothing));with(catalog, cv.test(Housewares, Automotive));with(catalog, cv.test(Housewares, Electronics)); # 가정용품과 자동차
with(catalog, cv.test(Housewares, Computers));with(catalog, cv.test(Housewares, Garden));with(catalog, cv.test(Housewares, Gift));
with(catalog, cv.test(Housewares, Jewelry));
### 자동차 상관계수 
with(catalog, cv.test(Automotive, Clothing));with(catalog, cv.test(Automotive, Housewares));with(catalog, cv.test(Automotive, Electronics)); # 자동차와 의류, 가정용품
with(catalog, cv.test(Automotive, Computers));with(catalog, cv.test(Automotive, Garden));with(catalog, cv.test(Automotive, Gift)); # 자동차와 컴퓨터
with(catalog, cv.test(Automotive, Jewelry));
### 전자제품 상관계수 
with(catalog, cv.test(Electronics, Clothing));with(catalog, cv.test(Electronics, Housewares));with(catalog, cv.test(Electronics, Automotive)); # 전자제품과 의류
with(catalog, cv.test(Electronics, Computers));with(catalog, cv.test(Electronics, Garden));with(catalog, cv.test(Electronics, Gift)); # 전자제품과 컴퓨터
with(catalog, cv.test(Electronics, Jewelry));
### 컴퓨터 상관계수 
with(catalog, cv.test(Computers, Clothing));with(catalog, cv.test(Computers, Housewares));with(catalog, cv.test(Computers, Automotive)); # 컴퓨터와 자동차
with(catalog, cv.test(Computers, Electronics));with(catalog, cv.test(Computers, Garden));with(catalog, cv.test(Computers, Gift)); # 컴퓨터와 전자제품, 원예용품, 선물
with(catalog, cv.test(Computers, Jewelry));
### 원예용품 상관계수 
with(catalog, cv.test(Garden, Clothing));with(catalog, cv.test(Garden, Housewares));with(catalog, cv.test(Garden, Automotive)); # 원예와 의류
with(catalog, cv.test(Garden, Electronics));with(catalog, cv.test(Garden, Computers));with(catalog, cv.test(Garden, Gift)); # 원예와 컴퓨터
with(catalog, cv.test(Garden, Jewelry));
### 선물 상관계수 
with(catalog, cv.test(Gift, Clothing));with(catalog, cv.test(Gift, Housewares));with(catalog, cv.test(Gift, Automotive)); # 선물과 의류
with(catalog, cv.test(Gift, Electronics));with(catalog, cv.test(Gift, Computers));with(catalog, cv.test(Gift, Garden)); # 선물과 컴퓨터
with(catalog, cv.test(Gift, Jewelry));
### 쥬얼리 상관계수 
with(catalog, cv.test(Jewelry, Clothing));with(catalog, cv.test(Jewelry, Housewares));with(catalog, cv.test(Jewelry, Automotive)); # 쥬얼리와 의류
with(catalog, cv.test(Jewelry, Electronics));with(catalog, cv.test(Jewelry, Computers));with(catalog, cv.test(Jewelry, Garden)); 
with(catalog, cv.test(Jewelry, Gift));
