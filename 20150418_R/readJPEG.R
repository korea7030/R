##greeting with jpeg package------- 

install.packages("jpeg")
library(jpeg)
mtrushBMP <- readJPEG("image_sample.jpg")
str(mtrushBMP)
class(mtrushBMP)
dim(mtrushBMP)
range(mtrushBMP[1,,1])
max(mtrushBMP)
min(mtrushBMP)

## max()와 min()결과로 보면 RGB를 0과 1사이의 함수값으로 나타내는 것으로 보임.
## 따라서 일부를 지울려면, 모두 1로 바꾸면 가능할 것으로 보임.

mtrushBMP[100:200,100:200,1] <- 1
mtrushBMP[100:200,100:200,2] <- 1
mtrushBMP[100:200,100:200,3] <- 1

writeJPEG(mtrushBMP,target="mtrushJPG.jpg")

##흐름제어 연습 with jpeg package--------

mtrushBMP <- readJPEG("image_sample.jpg")

for (x in 1:dim(mtrushBMP)[1]) {
  if ( x %% 50 == 0) {
    mtrushBMP[x,,1] <- 1
    mtrushBMP[x,,2] <- 1
    mtrushBMP[x,,3] <- 1
  }
}

writeJPEG(mtrushBMP,target="mtrushJPG_x.jpg")

mtrushBMP <- readJPEG("image_sample.jpg")

for (y in 1:dim(mtrushBMP)[2]) {
  if ( y %% 50 == 0) {
    mtrushBMP[,y,1] <- 1
    mtrushBMP[,y,2] <- 1
    mtrushBMP[,y,3] <- 1
  }
}

writeJPEG(mtrushBMP,target="mtrushJPG_y.jpg")

##중첩된 흐름제어 with jpeg package

mtrushBMP <- readJPEG("image_sample.jpg")

for (x in 1:dim(mtrushBMP)[1]) {
  if ( x %% 50 == 0) {
    mtrushBMP[x,,1] <- 1
    mtrushBMP[x,,2] <- 1
    mtrushBMP[x,,3] <- 1
  }
}

writeJPEG(mtrushBMP,target="mtrushJPG_x.jpg")

for (y in 1:dim(mtrushBMP)[2]) {
  if ( y %% 50 == 0) {
    mtrushBMP[,y,1] <- 1
    mtrushBMP[,y,2] <- 1
    mtrushBMP[,y,3] <- 1
  }
}

writeJPEG(mtrushBMP,target="mtrushJPG_xy.jpg")
