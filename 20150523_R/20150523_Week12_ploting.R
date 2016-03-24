##Ploting-------------

head(userRFM)
# save(userRFM, file="userRFM.RData")
tblgrade <- table(userRFM$grade)
plot(tblgrade)
plot(tblgrade, type="l")
title(main="My first plot image")

plot(tblgrade, type="l", xaxt="n", ylab="")
title(main="My second plot image")
axis(1, at=1:length(tblgrade), label=c("최우수고객","우수고객","보통고객","평균 고객","불만고객"))
title(xlab="고객구분", col.lab="red")
title(ylab="고객수", col.lab="blue")

userRFM$gradeFac <- factor(userRFM$grade, levels=c("A","B","C","D","E"), ordered=T)
boxplot(userRFM$score~userRFM$grade)
boxplot(userRFM$score~userRFM$grade, notch=T, col=c("red", "blue", rep("black",3)))

library(dplyr)
library(plyr)
head(basketData)
head(userRFM)
# basketData$datetime <- strptime(paste0(basketData$date,basketData$time), format="%Y%m%d%H")
basketGrade <- join(basketData, userRFM[,c("custId","grade")])
head(basketGrade)
basketGrade <- basketGrade[,-(ncol(basketGrade)-1)]

graphOut <- basketGrade %>%
  group_by(grade, custId, branchId) %>%
  summarize(avgPurchase=mean(amount)) 

dim(graphOut)

boxplot(graphOut$avgPurchase~graphOut$grade, notch=T)
# 참조 : http://stackoverflow.com/questions/15181086/labeling-outliers-on-boxplot-in-r

graphOut2 <- basketGrade %>%
  group_by(grade, custId, branchId) %>%
  summarize(avgPurchase=mean(amount)) %>%
  group_by(grade, branchId) %>%
  summarize(N=n())

head(graphOut2)
boxplot(graphOut2$N ~ graphOut2$grade)
library(reshape)
graphDummy <- data.frame(graphOut2)
graphCast <- cast(graphDummy, grade ~ branchId, value="N", fun.aggregate=sum)
head(graphCast)

matplot(graphCast)
matplot(graphCast, type="o")
matplot(graphCast, type="o", pch=16)
matplot(graphCast, type="l", pch=c(16,20,21,22), col=c("blue", "yellow","magenta","red"))
legend("topright",
       c("branchId_1","branchId_2","branchId_3","branchId_4"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       cex=1,
       lwd=c(5, 5, 5,5),
       col=c("blue", "yellow","magenta","red"))
matplot(graphCast, type="o", pch=c(16,20,21,22), col=c("blue", "yellow","magenta","red"),
        ylim=c(30,300))
legend("topright",
       c("branchId_1","branchId_2","branchId_3","branchId_4"),
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       cex=1,
       lwd=c(5, 5, 5,5),
       col=c("blue", "yellow","magenta","red"))

graphmat <- as.matrix(graphCast)
head(graphmat)

pdfpngplot(graphmat,"pdfpngplot_01","My first pdf_png Plot","total Number of customers")

pdfpngplot <- function(data, filename, maintitle, labtitle) {
  # pdf 그래프 출력 샘플 -------
  # 15 vs 11.25, 30 vs 22.5
  
  pdf(paste0(filename,".pdf"), width=10, height=7)
  
  par(mar=c(5,7,10,1))
  
  matplot(data, type="l", xaxt="n", yaxt="n",
          col=c("blue", "yellow","magenta","green","red"),
          lwd=c(3,3,3,3),ylab="")
  title(main=maintitle, 
        col.main="gray8", cex.main=3.2)
  title(ylab=labtitle, 
        col.lab="black", cex.lab=3)
  
  axis(1,at=1:dim(data)[1],labels=row.names(data), las=2, cex=0.5)
  axis(2, cex.axis=1.5)
  
  legend("topright",
         c("branchId_1","branchId_2","branchId_3","branchId_4"),
         lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
         cex=1,
         lwd=c(5, 5, 5,5),
         col=c("blue", "yellow","magenta","red"))
  dev.off()
  
  # png 그래프 출력 샘플 -------
  png(paste0(filename,".png"), width=10, height=7, units = "in", res=400)
  
  par(mar=c(5,7,10,1))
  
  matplot(data, type="l", xaxt="n", yaxt="n",
          col=c("blue", "yellow","magenta","red"),
          lwd=c(3,3,3,3), ylab="")
  
  title(main=maintitle, 
        col.main="gray8", cex.main=3.2)
  
  title(ylab=labtitle, 
        col.lab="black", cex.lab=3)
  
  axis(1,at=1:dim(data)[1],labels=row.names(data), las=2, cex=0.5)
  axis(2, cex.axis=1.5)
  
  legend("topright",
         c("branchId_1","branchId_2","branchId_3","branchId_4"),
         lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
         cex=1,
         lwd=c(5, 5, 5,5),
         col=c("blue", "yellow","magenta","red"))
  dev.off()
}


#http://cran.r-project.org/doc/contrib/Lemon-kickstart/kr_addat.html
plot(6:25,rnorm(20),type="b",xlim=c(1,30),ylim=c(-2.5,2.5),col=2)
par(new=T)
plot(rnorm(30),type="b",axes=F,col=3)
par(new=F)

#blank plot으로 시작하는 방법
plot(1, type="n", axes=F, xlab="", ylab="")
plot(3, type="n", axes=T, xlab="", ylab="")
matplot(graphCast, type="n")

# producing simple graph in R 참조 : http://www.harding.edu/fmccown/r/


## ggplot2------------
install.packages("ggplot2")
library(ggplot2)

# interesting graph by Liang Xianglong in SNU.

f <- function(x) 1/(x^2-1)
x <- seq(-3,3, by=0.001)
y <- f(x)
d <- data.frame(x=x, y=y)

p <- ggplot()
p <- p + geom_rect(fill="white", color="black", size=3,
                   aes(NULL, NULL, xmin=-3, xmax=3,
                       ymin=-3, ymax=3, alpha=0.1))

p <- p + geom_line(data=d, aes(x,y), size=3) + ylim(-3,3)
p

theme_null <- function() {
  theme_bw() %+replace%
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.position="none",
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank())
}

p+theme_null()+xlab("")+ylab("")


p <- ggplot( mtcars, aes(mpg, wt, colour = cyl) ) + geom_point()
mtcars <- transform(mtcars, mpg = mpg ^ 2)
p %+% mtcars 

p %+% mtcars + geom_smooth()
p %+% mtcars + geom_smooth(method=loess)

p + geom_point( aes(colour = cyl) ) # 속성을 추가하거나,
p + geom_point( aes(y = disp) ) # 변경하는 것이 매우 자유로움.
