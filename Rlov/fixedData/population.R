rm(list=ls())

install.packages("XML")
library(XML)

install.packages("RCurl")
library(RCurl)
library(httr)

pop <- 'http://en.wikipedia.org/wiki/World_population'

pop

tabs <- getURL(pop)
pop <- readHTMLTable(pop, stringsAsFactors=F)
pop_table <- readHTMLTable(rawToChar(GET(pop)$content),which=1)

length(pop_table)

head(pop_table)

pop2 <- pop_table[c(1:11), c(2:5)]
pop2

write.csv(pop2, 'pop.csv')

pop3 <- read.csv('pop.csv', header=T)
pop3

pop4 <- gvisColumnChart(pop3, xvar="Top.ten.most.populous.countries", options=list(title='국가별 인구수 (단위:백만명)', height=400, weight=600))

plot(pop4)

#######################################

## 1990년 자료만 

pop5 <- pop3[,c(1:2)]

pop5

pie_1 <- gvisPieChart(pop5, options=list(width=500, height=500))

plot(pie_1)
# 
# length(pop_table)
