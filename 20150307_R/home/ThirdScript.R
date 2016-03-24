## 히스토그램 그래프 그리기(hist) 
hist(GDP$GDPperCapita2012)
hist(HealthSys$PublicHealthExpensePercTotal)

## x축과 제목 추가(xlab, main)
hist(GDP$GDPperCapita2012, xlab="US $ (2012)", main="GDP Capita")
hist(HealthSys$PublicHealthExpensePercTotal, xlab="% of Total", main="Public Health Expense")

## 구간 나누기(br)
hist(GDP$GDPperCapita2012, xlab="US $ (2012)", main="GDP Capita", br=25)
hist(HealthSys$PublicHealthExpensePercTotal, xlab="% of Total", main="Public Health Expense", br=25)

## 색깔 입히기(col)
hist(GDP$GDPperCapita2012, xlab="US $ (2012)", main="GDP Capita", br=25, col="darkorange")
hist(HealthSys$PublicHealthExpensePercTotal, xlab="% of Total", main="Public Health Expense", br=25, col="purple")

## 색깔 지정 참조
## 이름으로 지정할 때: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
## 좀더 일반적인 내용: http://www.stat.tamu.edu/~jkim/Rcolorstyle.pdf
##

## 빗금 추가(density, angle)
hist(GDP$GDPperCapita2012, xlab="US $ (2012)", main="GDP Capita", br=25, col="darkorange", density=30)
hist(HealthSys$PublicHealthExpensePercTotal, xlab="% of Total", main="Public Health Expense", br=25, col="purple", density=30, angle=120)

## 추세선 추가(lines)
hist(GDP$GDPperCapita2012, xlab="US $ (2012)", main="GDP Capita", br=25, col="darkorange", density=30, freq=F)  ## 각구간 빈도가 아니고
lines(density(GDP$GDPperCapita2012, na.rm=T), col="darkorange", lwd=2)
hist(HealthSys$PublicHealthExpensePercTotal, xlab="% of Total", main="Public Health Expense", br=25, col="purple", density=30, angle=120, freq=F)
lines(density(HealthSys$PublicHealthExpensePercTotal, na.rm=T), col="purple", lwd=2, lty=2)

## 한국 항목 위치에 수직선 그리기(abline)
indkor <- which(GDP$CountryCode == "KOR")
hist(GDP$GDPperCapita2012, xlab="US $ (2012)", main="GDP Capita", br=25, col="darkorange", density=30, freq=F)  ## 각구간 빈도가 아니고
abline(v=GDP$GDPperCapita2012[indkor], col="red") ## 한국 값에 수직선

## 상자그림(boxplot) - 여러 데이터를 각데이터별로 묶음으로 그려줄때
## 2012년 일인당 GDP를 OECD와 비 OECD인 나라를 비교
gdp.oecd <- GDP$GDPperCapita2012[GDP$OECD == "Y"]
gdp.other <- GDP$GDPperCapita2012[GDP$OECD != "Y"]

## boxplot은 list를 받음
## list에는 다른 타입의 값들이 들어갈수 있음 (ex "문자", 숫자, 배열 ...)
boxplot(list(gdp.oecd, gdp.other))

gdplist <- list("OECD"=gdp.oecd, "Others"=gdp.other)
boxplot(gdplist)

## x축 y축 추가
boxplot(gdplist, main="GDP / Capita", xlab="Group", ylab="US $ (2012)")

##극한값 나타내기(pch, cex, bg, col)
boxplot(gdplist, main="GDP / Capita", xlab="Group", ylab="US $ (2012)", pch=17, cex=1.5, bg="darkblue", col="beige")

## 수평방향 그리기(horizontal)
boxplot(gdplist, main="GDP / Capita", xlab="US $ (2012)", ylab="Group", pch=17, cex=1.5, bg="darkblue", col="beige", horizontal=T)

## 한국의 위치 나타내기
boxplot(gdplist, main="GDP / Capita", xlab="Group", ylab="US $ (2012)", pch=17, cex=1.5, bg="darkblue", col="beige")
abline(h=GDP$GDPperCapita2012[indkor], col="red", lty=2)

## #####2차원 산포도
plot(GDP$GDPperCapita2012, HealthSys$PublicHealthExpensePercTotal, main="GDP vs Public Health Portion", xlab="GDP / Capita (US $, 2012)", ylab="Public Health Portion (%)", pch=21, bg="darkblue", col="lightblue")

## 한국만 따로 
plot(GDP$GDPperCapita2012, HealthSys$PublicHealthExpensePercTotal, main="GDP vs Public Health Portion", xlab="GDP / Capita (US $, 2012)", ylab="Public Health Portion (%)", pch=21, bg="darkblue", col="lightblue")

points(GDP$GDPperCapita2012[indkor], HealthSys$PublicHealthExpensePercTotal[indkor], pch=21, bg="darkorange", col="yellow")

abline(v=GDP$GDPperCapita2012[indkor], lty=2, col="orange")
abline(h=HealthSys$PublicHealthExpensePercTotal[indkor], lty=2, col="orange")

## OECD 위치 표시
indoecd <- which(GDP$OECD == "Y")
points(GDP$GDPperCapita2012[indoecd], HealthSys$PublicHealthExpensePercTotal[indoecd], pch=21, bg="darkred", col="pink")
points(GDP$GDPperCapita2012[indkor], HealthSys$PublicHealthExpensePercTotal[indkor], pch=21, bg="darkorange", col="yellow")

## 범례 표시(legend)
## legend(<위치>, legend=<범례의 설명문>, <기타 그림 관련 입력 변수>)

legend("bottomright", legend=c("OECD", "Korea"), pch=21, col=c("pink", "yellow"), pt.bg=c("darkred", "darkorange"))
