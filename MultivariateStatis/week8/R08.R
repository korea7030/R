# 클러스터링을 하기 전에 꼭 scale을 해야한다. 특정 변수의 값에 의해 영향을 많이 받을 수 있으므로....

x=c(1,3,6,12,20)
dist(x)

# 최단 연결법
hc1 = hclust(dist(x), method="single")
hc1
plot(hc1)
cutree(hc1, 3)

# 최장 연결법
hc2 = hclust(dist(x), method="complete")
plot(hc2)

# 평균 연결법
hc3 = hclust(dist(x), method="average")
plot(hc3)


## USArrests
USArrests
hc5 = hclust(dist(scale(USArrests)), method="single")
plot(hc5)

hc6 = hclust(dist(scale(USArrests)), method="complete")
plot(hc6)
class6 = cutree(hc6, 4)
sUS = as.data.frame(scale(USArrests))
attach(sUS)
plot(Murder, Assault, col=class6, pch=class6, type="no")
text(Murder, Assault, col=class6, pch=class6, labels=rownames(USArrests))


# 주성분 분석으로 변수를 그룹화하여 군집분석하기
pc1 = prcomp(USArrests, scale=T)
plot(pc1$x[,1], pc1$x[,2], type="n")
text(pc1$x[,1], pc1$x[,2], labels=rownames(USArrests), col=class6, pch=class6)

# 각 그룹의 특징(요약통계량)
library(psych)
describeBy(USArrests, group=class6)
describeBy(sUS, group=class6)

# 상자그림
sUS$class = class6
par(mfcol=c(2,2))
boxplot(Murder~class, sUS, main="Murder")
boxplot(Rape~class, sUS, main="Rape")
boxplot(Assault~class, sUS, main="Assault")
boxplot(UrbanPop~class, sUS, main="UrbanPop")


# HW4
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
plot(hc)
