getwd()
setwd("C:/Users/leejh/Documents/R/20150404_R")
getwd()

####################################
######## 20150404 강의내용##########
myMatrix <- matrix(1:20, nrow=4, ncol=5, byrow=TRUE)  ## byrow -> row기준으로 출력
attributes(myMatrix) ## matrix에 대한 속성을 보여줌 
myMatrix

length(myMatrix)  ## 전체 길이 
myMatrix
myMatrix[1,]  ## 해당행의 모든내용
myMatrix[,1]  ## 해당열읨 모든내용

myArray <- array(60:1, dim=c(4,5,3))
myArray

herArray <- array(50:1, dim=c(4,5,3)) ## 집어넣을 값이 모자를 경우 다시 맨처음 값이 입력됨.
herArray

attributes(myArray)
myArray[1,2,1]

## 3원색과 색료의 이미지 형태 처리 
## 빛의 삼원색 RGB
## 색의 삼원색 MCY?

## 이미지 에대한 처리르 위한 packages (* dependencies : 종속된 내용도 같이 설치하라)
install.packages("raster", dependencies=T)  

library(raster)
dir()

mtrushRGB <- brick("image_sample.jpg")

str(mtrushRGB)

attributes(mtrushRGB)

mtrushRGBvalue <- getValues(mtrushRGB)
dim(mtrushRGB)

mtrushRGB[1,1][,3] ## 1,1인 점의 blue 수치를 알고 싶어
mtrushRGB[1][,3]

mtrushTweak <- mtrushRGB
head(mtrushTweak[100:200, 100:200][,])

mtrushTweak[100:200, 100:200][,3] <- 255  ## Blue
mtrushTweak[100:200, 100:200][,1] <- 0    ## Red
mtrushTweak[100:200, 100:200][,2] <- 0    ## Green

plotRGB(mtrushTweak)

eraseXy <- function(imageData, x, y, ncol, nrow) {
  xrange <- x:(x+ncol)
  yrange <- y:(y+nrow)
  imageData[yrange, xrange][,1] <- 255
  imageData[yrange, xrange][,2] <- 255
  imageData[yrange, xrange][,3] <- 255
  
  return(imageData)
}

imgErase1 <- eraseXy(mtrushTweak, 900, 600, 300,300)
imgErase2 <- eraseXy(mtrushTweak, 1100, 500, 300,500)
plotRGB(imgErase1)
plotRGB(imgErase2)


eraseXy2 <- function(imageData, x, y, ncol=500, nrow=500) {
  xrange <- x:(x+ncol)
  yrange <- y:(y+nrow)
  imageData[yrange, xrange][,1] <- 255
  imageData[yrange, xrange][,2] <- 255
  imageData[yrange, xrange][,3] <- 255
  
  return(imageData)
}

imgErase3 <- eraseXy2(mtrushRGB, 800,500)
plotRGB(imgErase3)

myFamilyNames <- c("Dad", "Mom", "Sis", "Bro", "Dog")
myFamilyAges <- c(43, 42, 12, 8,5)

myFamilyGenders <- c("Male", "Female", "Female", "Male", "Female")
myFamilyWeights <- c(188,136,83, 61, 44)

myFamilyDf <- data.frame(myFamilyNames, myFamilyAges, myFamilyGenders, myFamilyWeights)
myFamilyList <- list(names= myFamilyNames, ages=myFamilyAges, genders=myFamilyGenders, weights=myFamilyWeights)

## 트위터 연동 ##
