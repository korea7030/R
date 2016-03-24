## ggplot2------------
# install.packages("ggplot2")
library(ggplot2)

head(diamonds)
dim(diamonds)

p <- ggplot(diamonds, aes(carat, price, colour = cut))

p <- p + layer(geom = "point")

p <- p + geom_point()
p

p <- ggplot(diamonds, aes(carat, price, colour = clarity))

p <- p + geom_point()
p

p <- ggplot(diamonds, aes(carat, price, colour = color))
p <- p + geom_line() + geom_smooth()
p <- p + geom_line() + geom_smooth(method="loess")
p


p <- ggplot(diamonds, aes(carat, colour = color))
p <- p + geom_histogram(stat="bin")
p

hist(diamonds$carat)

# interesting graph by Liang Xianglong in SNU.

f <- function(x) 1/(x^2-1)

x <- seq(-3,3, by=0.001)
y <- f(x)
d <- data.frame(x=x, y=y)

p <- ggplot()
p #그래프를 출력할려고 하면 에러가 남.

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



# 아래는 ggplot2 영문교재에서 나온 ch5까지의 실습 사례 코드임.

p <- ggplot( mtcars, aes(mpg, wt, colour = cyl) ) + geom_point()
mtcars <- transform(mtcars, mpg = mpg ^ 2)
p %+% mtcars 

p %+% mtcars + geom_smooth()
p %+% mtcars + geom_smooth(method=loess)

p + geom_point( aes(colour = cyl) ) # 속성을 추가하거나,
p + geom_point( aes(y = disp) ) # 변경하는 것이 매우 자유로움.



ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point()


# which is equivalent to
qplot(sleep_rem / sleep_total, awake, data = msleep)

# You can add layers to qplot too:
qplot(sleep_rem / sleep_total, awake, data = msleep) + geom_smooth()

# This is equivalent to
qplot(sleep_rem / sleep_total, awake, data = msleep, geom = c("point", "smooth"))

# or
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth()


# $4.4
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p
head(mtcars)
mtcars <- transform(mtcars, mpg = mpg ^ 2)
p %+% mtcars

# $4.5
install.packages("nlme")  # 데이터 활용을 위한 패키지 설치임.
library(nlme)
class(Oxboys)
head(Oxboys)
?Oxboys

p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour = "darkblue")
p + geom_point(aes(colour = "darkblue"))

p <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
p
p <- ggplot(Oxboys, aes(age, height)) + geom_line()
p

boysbox <- ggplot( Oxboys, aes(Occasion, height) ) + geom_boxplot()
boysbox + geom_line( aes(group = Subject), colour = "pink")


d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)

d + geom_histogram(binwidth=0.1, position="stack", aes(colour=cut))
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "bar")
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
d + stat_bin( aes(size = ..density..), binwidth = 0.1, geom = "point", position="identity")


require(nlme, quiet=T, warn.conflicts=F)

model = lme( height ~ age, data = Oxboys, random = ~1+age | Subject)

oplot = ggplot( Oxboys, aes(age, height, group = Subject)) + geom_line()

age_grid = seq(-1,1,length = 10)
age_grid

subject = unique( Oxboys$Subject )

preds = expand.grid ( age = age_grid, Subject = subject)
preds$height = predict (model, preds)

oplot + geom_line(data = preds, colour="#3366FF", size = 0.4) #predict한걸 겹쳐그림


#compare the model to the data (residual)
Oxboys$fitted = predict(model)
Oxboys$resid = with(Oxboys, fitted - height)

oplot %+% Oxboys + aes(y=resid) + geom_smooth(aes(group=1)) #잔차그림override


#swapping out the data makes it easy to experiment with imputation schemes or model fits

model2 <- update(model, height ~ age + I(age ^ 2))
Oxboys$fitted2 <- predict(model2)
Oxboys$resid2 <- with(Oxboys, fitted2 - height)
oplot %+% Oxboys + aes(y = resid2) + geom_smooth(aes(group=1))


df <- data.frame(
  x <- c(3,1,5),
  y <- c(2,4,6),
  label <- c("a","b","c")
  )

p <- ggplot(df, aes(x, y, label=label)) + xlab(NULL) + ylab(NULL)
p + geom_point() + labs(title="geom_point")
p + geom_bar(stat="identity") + labs(title="geom_bar(stat=\"identity\")")
p + geom_line() + labs(title="geom_line")
p + geom_area() + labs(title="geom_area")
p + geom_path() + labs(title="geom_path")
p + geom_tile() + labs(title="geom_tile")
p + geom_polygon() + labs(title="geom-polygon")

data(diamonds)
head(diamonds)
?diamonds

depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58,68)
depth_dist + geom_histogram(aes(y=..density..), binwidth=0.1)

depth_dist + geom_histogram(aes(fill=cut), binwidth=0.1, position="fill")

depth_dist + geom_freqpoly(aes(y=..density.., colour=cut), binwidth=0.1)