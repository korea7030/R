library(MVA)

us2 = scale(USArrests)
str(us2)
## data frame으로 변환
us2 = data.frame(us2)

km = kmeans(us2,3)
km
# K-means clustering with 3 clusters of sizes 20, 17, 13  (Initial을 Random 하게 잡아줌)
# 
# Cluster means: (군집된 공간에서의 중심점)
#   Murder    Assault   UrbanPop       Rape
# 1  1.0049340  1.0138274  0.1975853  0.8469650
# 2 -0.4469795 -0.3465138  0.4788049 -0.2571398
# 3 -0.9615407 -1.1066010 -0.9301069 -0.9667633
# 
# Clustering vector: (각 관측치가 어떤 그룹으로 할당되었는지를 나타냄)
#   Alabama         Alaska        Arizona       Arkansas     California       Colorado 
# 1              1              1              2              1              1 
# Connecticut       Delaware        Florida        Georgia         Hawaii          Idaho 
# 2              2              1              1              2              3 
# Illinois        Indiana           Iowa         Kansas       Kentucky      Louisiana 
# 1              2              3              2              3              1 
# Maine       Maryland  Massachusetts       Michigan      Minnesota    Mississippi 
# 3              1              2              1              3              1 
# Missouri        Montana       Nebraska         Nevada  New Hampshire     New Jersey 
# 1              3              3              1              3              2 
# New Mexico       New York North Carolina   North Dakota           Ohio       Oklahoma 
# 1              1              1              3              2              2 
# Oregon   Pennsylvania   Rhode Island South Carolina   South Dakota      Tennessee 
# 2              2              2              1              3              1 
# Texas           Utah        Vermont       Virginia     Washington  West Virginia 
# 1              2              3              2              2              3 
# Wisconsin        Wyoming 
# 3              2 
# 
# Within cluster sum of squares by cluster: 
# (각각 3가지의 값이 나타내는게 각 군집 중심에서 관측치까지 나온 거리의 제곱의 합)
#   [1] 46.74796 19.62285 11.95246
# (between_SS / total_SS =  60.0 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
# [7] "size"         "iter"         "ifault"

km$cluster  ## 클러스터가 할당된 결과

plot(Murder~Rape, data=us2, pch=km$cluster, col= km$cluster)
text(us2$Rape, us2$Murder, rownames(us2), cex=0.8, col=km$cluster)

##### 각 클러스터가 어떤식으로 나타나는지 확인

## 그리는 개수를 3행으로 표시
par(mfcol=c(3,1))

## y축의 scale을 맞춤
for (i in 1:3) {
  boxplot(us2[km$cluster == i,], main=paste("Group", i), ylim=c(-2.5, 3))
  abline(0,0,lty=2)  ## y=a+bx 의 선을 그어줌(a=0, b=0)
}
## 0을 기준으로 밑에 있으면 평균이하를 나타냄.

## ESS 계산


ESS = c()
for (k in 1:10) {
  km = kmeans(us2,k)
  ESS[k] = sum(km$withinss)
}

plot(ESS, type="l")

# [1] 46.74796 19.62285 11.95246
## 4개로 군집의 개수를 나눠주면 좋을듯 합니다.


# HW4############################################################################################################
# 22개 미국 전투기에 대한 6개 변수값이 jet.csv에 저장되어 있다. 각 변수는 아래와 같다.
# -	FFD: 처음 비행 날짜
# -	SPR: 단위무게 당 출력에 비례하는 특정한 출력
# -	RGF: 비행범위 요인
# -	PLF: 비행기의 총 무게의 일부분으로서의 탑재량
# -	SLF: 일관된 무게 요인
# -	CAR: 비행기가 항공모함에 착륙 가능여부
# 1.	계층적군집분석
# A.	FFD와 CAR를 제외한 변수를 표준화(scale) 한 후 최장연결법을 사용해 비계층적 군집화를 시행하고 덴드로그램을 그리시오.
jet_csv = read.csv("jet.csv")
head(jet_csv)
summary(jet_csv)
jet_csv[,1]
row.names(jet_csv) = jet_csv[,1]
jet_csv = jet_csv[,-1]
jet = jet_csv[,-c(1,6)]
jet
hc = hclust(dist(scale(jet)), method="complete")
hc
plot(hc)

# 1. B 
## 군집의 개수 결정(2개의 집단)
groups = cutree(hc, k=2)
rect.hclust(hc, k=2, border="red")

## 군집개수 결과를 groups 라는 변수로 dataframe 추가
## plot 그릴 때의 숫자값으로 쓰일 예정
jet['groups'] = as.factor(groups)
head(jet)

str(jet)

## 주성분 계산 시에는 빼고 계산 할 것. 
## 위에서 factor로 groups를 생성했기 때문에 계산이 안됨.
jet_pca = prcomp(jet[,-5], scale=TRUE)
jet_pca

## PC1, PC2에 대한 주성분 분석 결과 산점도
plot(main="procomp PC1 & PC2 Plot", xlab="PC1", ylab="PC2", jet_pca$x[,1], 
     jet_pca$x[,2], col= as.numeric(jet$groups)+1, pch=as.numeric(jet$groups)+1)
text(jet_pca$x[,1] , jet_pca$x[,2], rownames(jet), cex=0.5, pos=3)


# 2. A



ESS2 = c()
for (k in 1:5) {
  km = kmeans(scale(jet[,-5]),k)  ## 위와 똑같이 groups 값 제외 하고 계산
  ESS2[k] = sum(km$withinss)
}

plot(ESS2, type="l")


# 2. B 
km_jet = kmeans(scale(jet[,-5]), 2)  ## 위와 똑같이 groups 값 제외 하고 계산
km_jet 
km_jet$cluster

## 그래프 표시 결과 x축, y축 값이 관측치마다 달라 똑같게 지정 후 그래프 표시
## 산점도라 했는데, 여쭤보니 변수 적으니 scatter plot으로 그려도 상관없다 하셨음.
pairs(scale(jet[,-5]), pch=km_jet$cluster, col= km_jet$cluster, ylim=c(-2.5, 3), xlim = c(-2.5, 3))

# 3.
jet
jet_mc = Mclust(scale(jet[, -5]))
jet_mc

install.packages("mclust")
library(mclust)

plot(jet_mc)
# HW4############################################################################################################

## m모형기반 군집분석

us2
mc= Mclust(us2)
mc
# 'Mclust' model object:
#   best model: diagonal, equal shape (VEI) with 3 components
## 그룹이 3개 이면서, VEI 형태의 공분산 행렬 형태가 제일 좋은 BIC값이라고 선택된 것을 나타냄. 

plot(mc)
## x축은 그룹의 개수 
## y축은 BIC값
## 1번 선택 시(BIC plot)
## 그룹이 3개면서 VEI가 최대인게 최적의 모양이라고 그래프에 표시됨.

## 2번 선택 시
## scatter plot 표시
## 그래프에서의 동그라미는 각 그룹의 정규분포 모양을 나타냄
#  (중심 : 평균 , 공분산행렬 : 타원형(서로같은 방향)을 나타낸다. 
#  but dimension 간의 분산은 다르고, 그룹간의 분산이 다르다.)

## 3번 선택 시
## uncertainty(불확실한)
## 결과가 애매한 거에 대해 그래프를 보여줌(관측치의 모양이 크고 bold체 되어 있는 결과에 대해서)

## 4번 선택 시
## density
## 추정된 분포를 나타냄. 등고선형태로 표현

str(mc)
mc$classification ## 각 관측치가 어떤 그룹으로 할당 되어있는지

## 

## 특정값에 대해 모형분석을 할 경우
mc2 = Mclust(us2, 4)