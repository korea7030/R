rm(list=ls())
install.packages("googleVis")
library(googleVis)
# demo(WorldBank)

Fruits

Fruits1 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(Fruits1)


googleVis::gvisMotionChart

##########################################

subway58 <- read.csv("서울지하철_5-8호선_이용현황_시간대별.csv", sep=",", stringsAsFactors=NA)

subway58_2 <- gvisMotionChart(subway58, idvar="호선명", timevar="시간")
plot(subway58_2)
