library(XLConnect)
# install.packages("ROCR")
library(ROCR)
first_2014 <- loadWorkbook('2014_1.xlsx')
second_2014 <- loadWorkbook('2014_2.xlsx')
first_2015 <- loadWorkbook('2015_1.xlsx')
second_2015 <- loadWorkbook('2015_2.xlsx')

except <- c("건축공학과","실내건축학과","토목공학과","디지털콘텐츠학과","컴퓨터소프트웨어공학과","전자공학과","정보통신공학과","컴퓨터제어공학과","e-비즈니스학과","섬유의류비즈니스학과","산업디자인학과","패션디자인학과","생활스포츠학과","식품영양학과","경영학과","관광경영학과","부동산금융정보학과","비서학과","세무회계학과","행정학과","유아교육과","사회복지학과")
engineer <- c("건축과","실내건축디자인과","토목과","컴퓨터정보보안과","컴퓨터소프트웨어과","전자과","정보통신과","지능로봇과","e-비즈니스과","섬유패션비즈니스과","영상&게임콘텐츠과", "실내건축과", "디지털콘텐츠과", "모바일통신과", "컴퓨터제어과", "섬유의류비즈니스과")
social <- c("경영과","호텔관광경영과","부동산유통학과","비서사무행정과","세무회계과","유아교육과","사회복지과","영유아보육과","항공서비스과", "관광경영과", "부동산금융정보과", "비서과", "행정과")
art_psy <- c("광고디자인전공","산업디자인과","재활스포츠학과", "패션디자인과", "만화&2D영상그래픽전공", "3D영상그래픽전공", "쥬얼리디자인전공", "실용사진전공", "생활스포츠과")
science <- c("식품영양과","호텔외식조리학과","간호학과", "호텔외식조리과", "간호과")

## make sheet data function
getSubData <- function(mainName, sheetNum) {
  main <- eval(parse(text=mainName))
  tmp <- readWorksheet(main, sheetNum)
  return (tmp)
}

## get Cut off value
getCutOff <- function(mainData, fitData) {
  pred <- data.frame(Direction = mainData$Direction, fit = fitData$fitted.values)
  # get cut off 
  ob <- prediction(pred$fit, pred$Direction)
  performance <- performance(ob, "tpr", "fpr")
  # plot(performance)
  err_perf <- performance(ob, "err")
  plot(err_perf)
  cut <- err_perf@x.values[[1]]
  err <- err_perf@y.values[[1]]
  cutoff <- cut[err == min(err)] ## cutoff value : 0.44
  # plot(err.2014.korean~cut.2014.korean) ## cutoff graph	
  return (cutoff)
}

## get sheet data(2014,2015 korean, english, math) ##################
first_2014_korean <- getSubData("first_2014",1)
first_2014_english <- getSubData("first_2014",2)
first_2014_math <- getSubData("first_2014",3)

second_2014_korean <- getSubData('second_2014', 1)
second_2014_english <- getSubData('second_2014', 2)
second_2014_math <- getSubData('second_2014', 3)

first_2015_korean <- getSubData("first_2015",1)
first_2015_english <- getSubData("first_2015",2)
first_2015_math <- getSubData("first_2015",3)

second_2015_korean <- getSubData('second_2015', 1)
second_2015_english <- getSubData('second_2015', 2)
second_2015_math <- getSubData('second_2015', 3)
############################################################

## set Direction function 
setDirection <- function(mainData, colName, value) {
  data <- eval(parse(text=paste0(mainData,"$", colName)))
  data <- value
  return (data)
}

## setting Direction Column #####################################################
first_2014_korean$Direction<- setDirection("first_2014_korean", "Direction", 1)
first_2014_english$Direction<- setDirection("first_2014_english", "Direction", 1)
first_2014_math$Direction<- setDirection("first_2014_math", "Direction", 1)

second_2014_korean$Direction<- setDirection("second_2014_korean", "Direction", 1)
second_2014_english$Direction<- setDirection("second_2014_english", "Direction", 1)
second_2014_math$Direction<- setDirection("second_2014_math", "Direction", 1)

first_2015_korean$Direction<- setDirection("first_2015_korean", "Direction", 1)
first_2015_english$Direction<- setDirection("first_2015_english", "Direction", 1)
first_2015_math$Direction<- setDirection("first_2015_math", "Direction", 1)

second_2015_korean$Direction<- setDirection("second_2015_korean", "Direction", 1)
second_2015_english$Direction<- setDirection("second_2015_english", "Direction", 1)
second_2015_math$Direction<- setDirection("second_2015_math", "Direction", 1)
#####################################################################################

#### merge subject data ############################################################
merge_2014_korean <- merge(first_2014_korean, second_2014_korean)
merge_2014_english <- merge(first_2014_english, second_2014_english)
merge_2014_math <- merge(first_2014_math, second_2014_math)

merge_2015_korean <- merge(first_2015_korean, second_2015_korean)
merge_2015_english <- merge(first_2015_english, second_2015_english)
merge_2015_math <- merge(first_2015_math, second_2015_math)

#####################################################################################

#### set UP & DOWN #######################################################################################
merge_2014_korean$Direction[merge_2014_korean$second_korean - merge_2014_korean$first_korean > 0] <- "UP"
merge_2014_korean$Direction[merge_2014_korean$second_korean - merge_2014_korean$first_korean <= 0] <- "DOWN"

merge_2014_english$Direction[merge_2014_english$second_english - merge_2014_english$first_english > 0] <- "UP"
merge_2014_english$Direction[merge_2014_english$second_english - merge_2014_english$first_english <= 0] <- "DOWN"

merge_2014_math$Direction[merge_2014_math$second_math - merge_2014_math$first_math > 0] <- "UP"
merge_2014_math$Direction[merge_2014_math$second_math - merge_2014_math$first_math <= 0] <- "DOWN"

merge_2015_korean$Direction[merge_2015_korean$second_korean - merge_2015_korean$first_korean > 0] <- "UP"
merge_2015_korean$Direction[merge_2015_korean$second_korean - merge_2015_korean$first_korean <= 0] <- "DOWN"

merge_2015_english$Direction[merge_2015_english$second_english - merge_2015_english$first_english > 0] <- "UP"
merge_2015_english$Direction[merge_2015_english$second_english - merge_2015_english$first_english <= 0] <- "DOWN"

merge_2015_math$Direction[merge_2015_math$second_math - merge_2015_math$first_math > 0] <- "UP"
merge_2015_math$Direction[merge_2015_math$second_math - merge_2015_math$first_math <= 0] <- "DOWN"
####################################################################################################################

## except deptarment ##################################################################################
merge_2014_korean <- subset(merge_2014_korean, !(학과 %in% except))
merge_2014_english <- subset(merge_2014_english, !(학과 %in% except))
merge_2014_math <- subset(merge_2014_math, !(학과 %in% except))

merge_2015_korean <- subset(merge_2015_korean, !(학과 %in% except))
merge_2015_english <- subset(merge_2015_english, !(학과 %in% except))
merge_2015_math <- subset(merge_2015_math, !(학과 %in% except))

merge_2014_korean$Direction <- as.factor(merge_2014_korean$Direction)
merge_2014_english$Direction <- as.factor(merge_2014_english$Direction)
merge_2014_math$Direction <- as.factor(merge_2014_math$Direction)

merge_2015_korean$Direction <- as.factor(merge_2015_korean$Direction)
merge_2015_english$Direction <- as.factor(merge_2015_english$Direction)
merge_2015_math$Direction <- as.factor(merge_2015_math$Direction)

#######################################################################################################

## logistic analytics (Full Department) ##############################################################

##### 2014 ###################################################################################
fit.korean <- glm(Direction ~ second_korean, data = merge_2014_korean, family = binomial)
summary(fit.korean)
exp(0.032149) # Odds : 1.032671

fit.english <- glm(Direction ~ second_english, data = merge_2014_english , family = binomial)
summary(fit.english)
exp(0.031749) # Odds : 1.032258

fit.math <- glm(Direction~second_math, data = merge_2014_math, family = binomial)
summary(fit.math)
exp(0.035350) # 1.035982
#############################################################################################
##### 2015 ##################################################################################
fit.korean.2015 <- glm(Direction ~ second_korean, data = merge_2015_korean, family = binomial)
summary(fit.korean.2015)
exp(0.068193) # Odds :  1.070572

fit.english.2015 <- glm(Direction ~ second_english, data = merge_2015_english , family = binomial)
summary(fit.english.2015)
exp(0.035152) # Odds : 1.035777

fit.math.2015 <- glm(Direction~second_math, data = merge_2015_math, family = binomial)
summary(fit.math.2015)
exp(0.037110) # 1.037807
#############################################################################################
### ErrorRate, Sensitivity, Specificity ##############################
########### 2014 ###############################
table(merge_2014_korean$Direction) ## DOWN : 1038 , UP : 251
err.2014.korean <- getCutOff(merge_2014_korean, fit.korean) # cutoff : 0.4365607
xtabs(~merge_2014_korean$Direction+(fit.korean$fitted.values> err.2014.korean))
# merge_2014_korean$Direction FALSE
#                        DOWN  1038
#                        UP     251
table(merge_2014_english$Direction) ## DOWN : 656 , UP : 633
err.2014.english <- getCutOff(merge_2014_english, fit.english) # 0.4600607
xtabs(~merge_2014_english$Direction+(fit.english$fitted.values > err.2014.english))
# merge_2014_english$Direction FALSE TRUE
#                         DOWN   384  272
#                         UP     204  429 
## Sensitivity = 429 / (204+429) : 0.678
## Specificity = 384 / (384+272) : 0.585
## Error Rate = (272+204) / (656+633) : 0.369

table(merge_2014_math$Direction) ## DOWN : 269, UP : 125
err.2014.math <- getCutOff(merge_2014_math, fit.math)
xtabs(~merge_2014_math$Direction+(fit.math$fitted.values > err.2014.math))
# merge_2014_math$Direction FALSE TRUE
#                      DOWN   252   17
#                      UP      98   27
## Sensitivity = 27 / (98+27) : 0.216
## Specificity = 252 / (252+17) : 0.936
## Error Rate = (17+98)/ (269+125) : 0.291
#######################################
############## 2015 ###################
table(merge_2015_korean$Direction) ## DOWN : 1778  , UP : 1160
err.2015.korean <- getCutOff(merge_2015_korean, fit.korean.2015) # cutoff : 0.5048324
xtabs(~merge_2015_korean$Direction+(fit.korean.2015$fitted.values> err.2015.korean))
# merge_2015_korean$Direction FALSE TRUE
#                        DOWN  1561  217
#                        UP     643  517
## Sensitivity = 517 / (643+517) : 0.446
## Specificity = 1561 / (1561+217) : 0.878
## Error Rate = (643+217) / (1778+1160) : 0.292
table(merge_2015_english$Direction) ## DOWN : 1742 , UP : 1196
err.2015.english <- getCutOff(merge_2015_english, fit.english.2015) # 0.4971123
xtabs(~merge_2015_english$Direction+(fit.english.2015$fitted.values > err.2015.english))
# merge_2015_english$Direction FALSE TRUE
#                         DOWN  1372  370
#                         UP     667  529 
## Sensitivity = 529 / (667+529) : 0.442
## Specificity = 1372 / (1372+370) : 0.788
## Error Rate = (370+667) / (1742+1196) : 0.352

table(merge_2015_math$Direction) ## DOWN : 241, UP : 361
err.2015.math <- getCutOff(merge_2015_math, fit.math.2015) ## 0.5790483
xtabs(~merge_2015_math$Direction+(fit.math.2015$fitted.values > err.2015.math))
# merge_2015_math$Direction FALSE TRUE
#                      DOWN   151   90
#                      UP     102  259
## Sensitivity = 102 / (102+259) : 0.282
## Specificity = 151 / (151+90) : 0.626
## Error Rate = (90+102)/ (241+361) : 0.319
#######################################
#####################################################################################################
## logistic analytics (Split Department) ############################################################
merge_2014_korean.engineer <- subset(merge_2014_korean, 학과 %in% engineer)
merge_2014_korean.social <- subset(merge_2014_korean, 학과 %in% social)
merge_2014_korean.art_psy <- subset(merge_2014_korean, 학과 %in% art_psy)
merge_2014_korean.science <- subset(merge_2014_korean, 학과 %in% science)

merge_2014_english.engineer <- subset(merge_2014_english, 학과 %in% engineer)
merge_2014_english.social <- subset(merge_2014_english, 학과 %in% social)
merge_2014_english.art_psy <- subset(merge_2014_english, 학과 %in% art_psy)
merge_2014_english.science <- subset(merge_2014_english, 학과 %in% science)

merge_2014_math.engineer <- subset(merge_2014_math, 학과 %in% engineer)

merge_2015_korean.engineer <- subset(merge_2015_korean, 학과 %in% engineer)
merge_2015_korean.social <- subset(merge_2015_korean, 학과 %in% social)
merge_2015_korean.art_psy <- subset(merge_2015_korean, 학과 %in% art_psy)
merge_2015_korean.science <- subset(merge_2015_korean, 학과 %in% science)

merge_2015_english.engineer <- subset(merge_2015_english, 학과 %in% engineer)
merge_2015_english.social <- subset(merge_2015_english, 학과 %in% social)
merge_2015_english.art_psy <- subset(merge_2015_english, 학과 %in% art_psy)
merge_2015_english.science <- subset(merge_2015_english, 학과 %in% science)

merge_2015_math.engineer <- subset(merge_2015_math, 학과 %in% engineer)

####### korean split Department ########################################################################################
fit.korean.engineer.2014 <- glm(Direction ~ second_korean, data = merge_2014_korean.engineer, family = binomial)
summary(fit.korean.engineer.2014)
exp(0.03208) # Odds : 1.0326

fit.korean.social.2014 <- glm(Direction ~ second_korean, data = merge_2014_korean.social, family = binomial)
summary(fit.korean.social.2014)
exp(0.03111) # Odds : 1.031599

fit.korean.art_psy.2014 <- glm(Direction ~ second_korean, data = merge_2014_korean.art_psy, family = binomial)
summary(fit.korean.art_psy.2014)
exp(0.01995) # Odds : 1.02015

fit.korean.science.2014 <- glm(Direction ~ second_korean, data = merge_2014_korean.science, family = binomial)
summary(fit.korean.science.2014)
exp(0.03352) # Odds : 1.034088

fit.korean.engineer.2015 <- glm(Direction ~ second_korean, data = merge_2015_korean.engineer, family = binomial)
summary(fit.korean.engineer.2015)
exp(0.075161) # Odds : 1.078058

fit.korean.social.2015 <- glm(Direction ~ second_korean, data = merge_2015_korean.social, family = binomial)
summary(fit.korean.social.2015)
exp(0.084215) # Odds : 1.087863

fit.korean.art_psy.2015 <- glm(Direction ~ second_korean, data = merge_2015_korean.art_psy, family = binomial)
summary(fit.korean.art_psy.2015)
exp(0.027845) # Odds : 1.028236

fit.korean.science.2015 <- glm(Direction ~ second_korean, data = merge_2015_korean.science, family = binomial)
summary(fit.korean.science.2015)
exp(0.06626) # Odds : 1.068504
##################################################################################################################################

#### Error Rate (2014,2015 split department korean) #############################
table(merge_2014_korean.engineer$Direction) ## DOWN : 451 , UP : 165
err.2014.korean.engineer <- getCutOff(merge_2014_korean.engineer, fit.korean.engineer.2014) # 0.4996522
xtabs(~merge_2014_korean.engineer$Direction+(fit.korean.engineer.2014$fitted.values > err.2014.korean.engineer))
# merge_2014_korean.engineer$Direction FALSE TRUE
#                                 DOWN   451    0
#                                 UP     164    1
## 
## Sensitivity = 1 / (164+1) : 0.006
## Specificity = 451 / (451+0) : 1
## Error Rate = (0+164)/ (451+165) : 0.267
table(merge_2014_korean.social$Direction) ## DOWN : 298 , UP : 45
err.2014.korean.social <- getCutOff(merge_2014_korean.social, fit.korean.social.2014) # Inf???
xtabs(~merge_2014_korean.social$Direction+(fit.korean.social.2014$fitted.values > err.2014.korean.social))
# merge_2014_korean.engineer$Direction FALSE 
#                                 DOWN   298 
#                                 UP     45 
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
table(merge_2014_korean.art_psy$Direction) ## DOWN : 111 , UP : 21
err.2014.korean.art_psy <- getCutOff(merge_2014_korean.art_psy, fit.korean.art_psy.2014) # 0.2574862
xtabs(~merge_2014_korean.art_psy$Direction+(fit.korean.art_psy.2014$fitted.values > err.2014.korean.art_psy))
# merge_2014_korean.engineer$Direction FALSE 
#                                 DOWN   111 
#                                 UP      21 
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
table(merge_2014_korean.science$Direction) ## DOWN : 120 , UP : 19
err.2014.korean.science <- getCutOff(merge_2014_korean.science, fit.korean.science.2014) # Inf
xtabs(~merge_2014_korean.science$Direction+(fit.korean.science.2014$fitted.values > err.2014.korean.science))
# merge_2014_korean.engineer$Direction FALSE
#                                 DOWN   120
#                                 UP      19
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
################################################################
###### english split Department ##################################################################################################
fit.english.engineer.2014 <- glm(Direction ~ second_english, data = merge_2014_english.engineer, family = binomial)
summary(fit.english.engineer.2014)
exp(0.037955) # Odds : 1.038684

fit.english.social.2014 <- glm(Direction ~ second_english, data = merge_2014_english.social, family = binomial)
summary(fit.english.social.2014)
exp(0.027987) # Odds : 1.028382

fit.english.art_psy.2014 <- glm(Direction ~ second_english, data = merge_2014_english.art_psy, family = binomial)
summary(fit.english.art_psy.2014)
exp(0.022455) # Odds : 1.022709

fit.english.science.2014 <- glm(Direction ~ second_english, data = merge_2014_english.science, family = binomial)
summary(fit.english.science.2014)
exp(0.023027) # Odds : 1.023294

fit.english.engineer.2015 <- glm(Direction ~ second_english, data = merge_2015_english.engineer, family = binomial)
summary(fit.english.engineer.2015)
exp(0.042827) # Odds : 1.043757

fit.english.social.2015 <- glm(Direction ~ second_english, data = merge_2015_english.social, family = binomial)
summary(fit.english.social.2015)
exp(0.031959) # Odds : 1.032475

fit.english.art_psy.2015 <- glm(Direction ~ second_english, data = merge_2015_english.art_psy, family = binomial)
summary(fit.english.art_psy.2015)
exp(0.028470) # Odds : 1.028879

fit.english.science.2015 <- glm(Direction ~ second_english, data = merge_2015_english.science, family = binomial)
summary(fit.english.science.2015)
exp(0.020457) # Odds : 1.020668

########################################################################################################################################
####### math split Department(Only engineer ) ##########################################################################################################
fit.math.engineer.2014 <- glm(Direction ~ second_math, data = merge_2014_math.engineer, family = binomial)
summary(fit.math.engineer.2014)
exp(0.035350) # Odds : 1.035982

fit.math.engineer.2015 <- glm(Direction ~ second_math, data = merge_2015_math.engineer, family = binomial)
summary(fit.math.engineer.2015)
exp(0.037110) # Odds : 1.037807

########################################################################################################################################

######## quantile (Full Department)##############################################################################################
quan_30_korean.2014 <- quantile(merge_2014_korean$second_korean, c(0,0.3,0.5, 0.7,1.0))
quan_30_english.2014 <- quantile(merge_2014_english$second_english, c(0,0.3,0.5, 0.7,1.0))
quan_30_math.2014 <- quantile(merge_2014_math$second_math, c(0,0.3,0.5, 0.7,1.0))

quan_30_korean.2015 <- quantile(merge_2015_korean$second_korean, c(0,0.3,0.5, 0.7,1.0))
quan_30_english.2015 <- quantile(merge_2015_english$second_english, c(0,0.3,0.5, 0.7,1.0))
quan_30_math.2015 <- quantile(merge_2015_math$second_math, c(0,0.3,0.5, 0.7,1.0))

quan_40_korean.2014 <- quantile(merge_2014_korean$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2014 <- quantile(merge_2014_english$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_math.2014 <- quantile(merge_2014_math$second_math, c(0,0.2,0.4,0.6,0.8,1.0))

quan_40_korean.2015 <- quantile(merge_2015_korean$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2015 <- quantile(merge_2015_english$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_math.2015 <- quantile(merge_2015_math$second_math, c(0,0.2,0.4,0.6,0.8,1.0))


#### high 30% #############################################################################################
merge_2014_korean_UP30<- merge_2014_korean[merge_2014_korean$second_korean >= quan_30_korean.2014[4],]
merge_2014_english_UP30<- merge_2014_english[merge_2014_english$second_english >= quan_30_english.2014[4],]
merge_2014_math_UP30<- merge_2014_math[merge_2014_math$second_math >= quan_30_math.2014[4],]

merge_2015_korean_UP30<- merge_2015_korean[merge_2015_korean$second_korean >= quan_30_korean.2014[4],]
merge_2015_english_UP30<- merge_2015_english[merge_2015_english$second_english >= quan_30_english.2014[4],]
merge_2015_math_UP30<- merge_2015_math[merge_2015_math$second_math >= quan_30_math.2014[4],]

########## 2014 ###########################################################################
fit.UP30.korean.2014 <- glm(Direction~second_korean, data = merge_2014_korean_UP30, family = binomial)
summary(fit.UP30.korean.2014)
exp(0.03074) # 1.031217

fit.UP30.english.2014 <- glm(Direction~second_english, data = merge_2014_english_UP30, family = binomial)
summary(fit.UP30.english.2014)
exp(0.02390) # 1.024188

fit.UP30.math.2014 <- glm(Direction~second_math, data = merge_2014_math_UP30, family = binomial)
summary(fit.UP30.math.2014)
exp(0.02894) # 1.029363
###########################################################################################
########## 2015 ###########################################################################
fit.UP30.korean.2015 <- glm(Direction~second_korean, data = merge_2015_korean_UP30, family = binomial)
summary(fit.UP30.korean.2015)
exp(0.08892) # 1.092993

fit.UP30.english.2015 <- glm(Direction~second_english, data = merge_2015_english_UP30, family = binomial)
summary(fit.UP30.english.2015)
exp(0.03285) # 1.033396

fit.UP30.math.2015 <- glm(Direction~second_math, data = merge_2015_math_UP30, family = binomial)
summary(fit.UP30.math.2015)
exp(0.06496) # 1.067116
###########################################################################################
#### Error Rate ####################################################################
#### 2014 ######################################################################
table(merge_2014_korean_UP30$Direction) ## DOWN : 412 , UP : 148
err.2014.UP30.korean <- getCutOff(merge_2014_korean_UP30, fit.UP30.korean.2014) # 0.4216374
xtabs(~merge_2014_korean_UP30$Direction+(fit.UP30.korean.2014$fitted.values > err.2014.UP30.korean))
# merge_2014_korean.engineer$Direction FALSE
#                                 DOWN   412
#                                 UP     148
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
table(merge_2014_english_UP30$Direction) ## DOWN : 157 , UP : 282
err.2014.UP30.english <- getCutOff(merge_2014_english_UP30, fit.UP30.english.2014) # 0.5946994
xtabs(~merge_2014_english_UP30$Direction+(fit.UP30.english.2014$fitted.values > err.2014.UP30.english))
# merge_2014_english.engineer$Direction FALSE TRUE
#                                 DOWN    37   120
#                                 UP      78   204
## 
## Sensitivity = 204 / (78+204) : 0.723
## Specificity = 37 / (37+120) : 0.236
## Error Rate =  (120+78) / (157+282) : 0.451
table(merge_2014_math_UP30$Direction) ## DOWN : 73 , UP : 73
err.2014.UP30.math <- getCutOff(merge_2014_math_UP30, fit.UP30.math.2014) # 0.5221217
xtabs(~merge_2014_math_UP30$Direction+(fit.UP30.math.2014$fitted.values > err.2014.UP30.math))
# merge_2014_math.engineer$Direction   FALSE TRUE
#                                 DOWN    56   17
#                                 UP      46   27
## 
## Sensitivity = 27 / (46+27) : 0.369863
## Specificity = 56 / (56+17) : 0.7671233
## Error Rate = (17+46) / (73+73) : 0.4315068
################################################################################
#### 2015 ######################################################################
table(merge_2015_korean_UP30$Direction) ## DOWN : 217 , UP : 517
err.2015.UP30.korean <- getCutOff(merge_2015_korean_UP30, fit.UP30.korean.2015) # 0.6204663
xtabs(~merge_2015_korean_UP30$Direction+(fit.UP30.korean.2015$fitted.values > err.2015.UP30.korean))
# merge_2015_korean.engineer$Direction FALSE TRUE
#                                 DOWN   119   98
#                                 UP     197  320
## 
## Sensitivity = 320 / (197+320) : 0.619
## Specificity = 119 / (119+98) : 0.548
## Error Rate = (98+197) / (217+517) : 0.4019074
table(merge_2015_english_UP30$Direction) ## DOWN : 488 , UP : 655
err.2015.UP30.english <- getCutOff(merge_2015_english_UP30, fit.UP30.english.2015) # 0.4922861
xtabs(~merge_2015_english_UP30$Direction+(fit.UP30.english.2015$fitted.values > err.2015.UP30.english))
# merge_2015_english.engineer$Direction FALSE TRUE
#                                 DOWN   118   370
#                                 UP     126   529
## 
## Sensitivity = 529 / (529+126) : 0.808
## Specificity = 118 / (118+370) : 0.241
## Error Rate =  (370+126) / (488+655) : 0.434
table(merge_2015_math_UP30$Direction) ## DOWN : 90 , UP : 259
err.2015.UP30.math <- getCutOff(merge_2015_math_UP30, fit.UP30.math.2015) # 0.5181663
xtabs(~merge_2015_math_UP30$Direction+(fit.UP30.math.2015$fitted.values > err.2015.UP30.math))
# merge_2015_math.engineer$Direction   FALSE TRUE
#                                 DOWN    22   68
#                                 UP      23  236
## 
## Sensitivity = 236 / (23+236) : 0.911
## Specificity = 22 / (22+68) : 0.244
## Error Rate = (68+23) / (90+259) : 0.261
################################################################################
####################################################################################

##########################################################################################################
#### Lower 30% #################################################################################
merge_2014_korean_DOWN30<- merge_2014_korean[merge_2014_korean$second_korean <= quan_30_korean.2014[2],]
merge_2014_english_DOWN30<- merge_2014_english[merge_2014_english$second_english <= quan_30_english.2014[2],]
merge_2014_math_DOWN30<- merge_2014_math[merge_2014_math$second_math <= quan_30_math.2014[2],]

merge_2015_korean_DOWN30<- merge_2015_korean[merge_2015_korean$second_korean <= quan_30_korean.2014[2],]
merge_2015_english_DOWN30<- merge_2015_english[merge_2015_english$second_english <= quan_30_english.2014[2],]
merge_2015_math_DOWN30<- merge_2015_math[merge_2015_math$second_math <= quan_30_math.2014[2],]

########## 2014 ###########################################################################
fit.DOWN30.korean.2014 <- glm(Direction~second_korean, data = merge_2014_korean_DOWN30, family = binomial)
summary(fit.DOWN30.korean.2014)
exp(0.05382) # 1.055295

fit.DOWN30.english.2014 <- glm(Direction~second_english, data = merge_2014_english_DOWN30, family = binomial)
summary(fit.DOWN30.english.2014)
exp(0.10278) # 1.108248

fit.DOWN30.math.2014 <- glm(Direction~second_math, data = merge_2014_math_DOWN30, family = binomial)
summary(fit.DOWN30.math.2014)
exp(0.09738) # 1.102279
###########################################################################################
########## 2015 ###########################################################################
fit.DOWN30.korean.2015 <- glm(Direction~second_korean, data = merge_2015_korean_DOWN30, family = binomial)
summary(fit.DOWN30.korean.2015)
exp(0.03967) # 1.040467

fit.DOWN30.english.2015 <- glm(Direction~second_english, data = merge_2015_english_DOWN30, family = binomial)
summary(fit.DOWN30.english.2015)
exp(0.08289) # 1.086422

fit.DOWN30.math.2015 <- glm(Direction~second_math, data = merge_2015_math_DOWN30, family = binomial)
summary(fit.DOWN30.math.2015)
exp(0.05346) # 1.054915
###########################################################################################
##### Error Rate ##########################################################################
#### 2014 ######################################################################
table(merge_2014_korean_DOWN30$Direction) ## DOWN : 390 , DOWN : 50
err.2014.DOWN30.korean <- getCutOff(merge_2014_korean_DOWN30, fit.DOWN30.korean.2014) # Inf
xtabs(~merge_2014_korean_DOWN30$Direction+(fit.DOWN30.korean.2014$fitted.values > err.2014.DOWN30.korean))
# merge_2014_korean.engineer$Direction FALSE
#                                 DOWN   390
#                                 UP      50
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
table(merge_2014_english_DOWN30$Direction) ## DOWN : 311 , DOWN : 125
err.2014.DOWN30.english <- getCutOff(merge_2014_english_DOWN30, fit.DOWN30.english.2014) # Inf 0.5663065
xtabs(~merge_2014_english_DOWN30$Direction+(fit.DOWN30.english.2014$fitted.values > err.2014.DOWN30.english))
# merge_2014_english.engineer$Direction FALSE
#                                 DOWN    311 
#                                 UP      125 
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate =  ?
table(merge_2014_math_DOWN30$Direction) ## DOWN : 108 , DOWN : 19
err.2014.DOWN30.math <- getCutOff(merge_2014_math_DOWN30, fit.DOWN30.math.2014) # Inf
xtabs(~merge_2014_math_DOWN30$Direction+(fit.DOWN30.math.2014$fitted.values > err.2014.DOWN30.math))
# merge_2014_math.engineer$Direction   FALSE
#                                 DOWN   108
#                                 UP      19
## 
## Sensitivity = 27 / (46+27) : 0.369863
## Specificity = 56 / (56+17) : 0.7671233
## Error Rate = (17+46) / (73+73) : 0.4315068
################################################################################
#### 2015 ######################################################################
table(merge_2015_korean_DOWN30$Direction) ## DOWN : 1079 , DOWN : 285
err.2015.DOWN30.korean <- getCutOff(merge_2015_korean_DOWN30, fit.DOWN30.korean.2015) # Inf
xtabs(~merge_2015_korean_DOWN30$Direction+(fit.DOWN30.korean.2015$fitted.values > err.2015.DOWN30.korean))
# merge_2015_korean.engineer$Direction FALSE
#                                 DOWN  1079
#                                 UP     285
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
table(merge_2015_english_DOWN30$Direction) ## DOWN : 629 , DOWN : 134
err.2015.DOWN30.english <- getCutOff(merge_2015_english_DOWN30, fit.DOWN30.english.2015) # Inf
xtabs(~merge_2015_english_DOWN30$Direction+(fit.DOWN30.english.2015$fitted.values > err.2015.DOWN30.english))
# merge_2015_english.engineer$Direction FALSE
#                                 DOWN   629 
#                                 UP     134 
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate =  ?
table(merge_2015_math_DOWN30$Direction) ## DOWN : 81 , DOWN : 30
err.2015.DOWN30.math <- getCutOff(merge_2015_math_DOWN30, fit.DOWN30.math.2015) # Inf
xtabs(~merge_2015_math_DOWN30$Direction+(fit.DOWN30.math.2015$fitted.values > err.2015.DOWN30.math))
# merge_2015_math.engineer$Direction   FALSE
#                                 DOWN    81
#                                 UP      30
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
################################################################################
###########################################################################################
###############################################################################################
#### Lower 40% #################################################################################
merge_2014_korean_DOWN40<- merge_2014_korean[merge_2014_korean$second_korean <= quan_40_korean.2014[3],]
merge_2014_english_DOWN40<- merge_2014_english[merge_2014_english$second_english <= quan_40_english.2014[3],]
merge_2014_math_DOWN40<- merge_2014_math[merge_2014_math$second_math <= quan_40_math.2014[3],]

merge_2015_korean_DOWN40<- merge_2015_korean[merge_2015_korean$second_korean <= quan_40_korean.2014[3],]
merge_2015_english_DOWN40<- merge_2015_english[merge_2015_english$second_english <= quan_40_english.2014[3],]
merge_2015_math_DOWN40<- merge_2015_math[merge_2015_math$second_math <= quan_40_math.2014[3],]

########## 2014 ###########################################################################
fit.DOWN40.korean.2014 <- glm(Direction~second_korean, data = merge_2014_korean_DOWN40, family = binomial)
summary(fit.DOWN40.korean.2014)
exp(0.03996) # 1.040769

fit.DOWN40.english.2014 <- glm(Direction~second_english, data = merge_2014_english_DOWN40, family = binomial)
summary(fit.DOWN40.english.2014)
exp(0.070622) # 1.073175

fit.DOWN40.math.2014 <- glm(Direction~second_math, data = merge_2014_math_DOWN40, family = binomial)
summary(fit.DOWN40.math.2014)
exp(0.07578) # 1.078725
###########################################################################################
########## 2015 ###########################################################################
fit.DOWN40.korean.2015 <- glm(Direction~second_korean, data = merge_2015_korean_DOWN40, family = binomial)
summary(fit.DOWN40.korean.2015)
exp(0.042612) # 1.040467

fit.DOWN40.english.2015 <- glm(Direction~second_english, data = merge_2015_english_DOWN40, family = binomial)
summary(fit.DOWN40.english.2015)
exp(0.055975) # 1.057571

fit.DOWN40.math.2015 <- glm(Direction~second_math, data = merge_2015_math_DOWN40, family = binomial)
summary(fit.DOWN40.math.2015)
exp(0.09408) # 1.098648
###########################################################################################
##### Error Rate ##########################################################################
#### 2014 ######################################################################
table(merge_2014_korean_DOWN40$Direction) ## DOWN : 500 , DOWN : 70
err.2014.DOWN40.korean <- getCutOff(merge_2014_korean_DOWN40, fit.DOWN40.korean.2014) # Inf
xtabs(~merge_2014_korean_DOWN40$Direction+(fit.DOWN40.korean.2014$fitted.values > err.2014.DOWN40.korean))
# merge_2014_korean.engineer$Direction FALSE
#                                 DOWN   500
#                                 UP      70
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
table(merge_2014_english_DOWN40$Direction) ## DOWN : 384 , DOWN : 204
err.2014.DOWN40.english <- getCutOff(merge_2014_english_DOWN40, fit.DOWN40.english.2014) # 0.6279284
xtabs(~merge_2014_english_DOWN40$Direction+(fit.DOWN40.english.2014$fitted.values > err.2014.DOWN40.english))
# merge_2014_english.engineer$Direction FALSE
#                                 DOWN    384 
#                                 UP      204 
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate =  ?
table(merge_2014_math_DOWN40$Direction) ## DOWN : 134 , DOWN : 28
err.2014.DOWN40.math <- getCutOff(merge_2014_math_DOWN40, fit.DOWN40.math.2014) # Inf
xtabs(~merge_2014_math_DOWN40$Direction+(fit.DOWN40.math.2014$fitted.values > err.2014.DOWN40.math))
# merge_2014_math.engineer$Direction   FALSE
#                                 DOWN   134
#                                 UP      28
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
################################################################################
#### 2015 ######################################################################
table(merge_2015_korean_DOWN40$Direction) ## DOWN : 1357 , DOWN : 438
err.2015.DOWN40.korean <- getCutOff(merge_2015_korean_DOWN40, fit.DOWN40.korean.2015) # Inf
xtabs(~merge_2015_korean_DOWN40$Direction+(fit.DOWN40.korean.2015$fitted.values > err.2015.DOWN40.korean))
# merge_2015_korean.engineer$Direction FALSE
#                                 DOWN  1357
#                                 UP     438
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
table(merge_2015_english_DOWN40$Direction) ## DOWN : 854 , DOWN : 251
err.2015.DOWN40.english <- getCutOff(merge_2015_english_DOWN40, fit.DOWN40.english.2015) # Inf
xtabs(~merge_2015_english_DOWN40$Direction+(fit.DOWN40.english.2015$fitted.values > err.2015.DOWN40.english))
# merge_2015_english.engineer$Direction FALSE
#                                 DOWN   854 
#                                 UP     251 
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate =  ?
table(merge_2015_math_DOWN40$Direction) ## DOWN : 89 , DOWN : 53
err.2015.DOWN40.math <- getCutOff(merge_2015_math_DOWN40, fit.DOWN40.math.2015) # 0.6369208
xtabs(~merge_2015_math_DOWN40$Direction+(fit.DOWN40.math.2015$fitted.values > err.2015.DOWN40.math))
# merge_2015_math.engineer$Direction   FALSE
#                                 DOWN    89
#                                 UP      53
## 
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
################################################################################
###########################################################################################
#########################################################################################################################################
######## quantile (split Department) ####################################################################################################
quan_30_korean.2014.engineer <- quantile(merge_2014_korean.engineer$second_korean, c(0,0.3,0.5,0.7,1.0))
quan_30_korean.2014.social <- quantile(merge_2014_korean.social$second_korean, c(0,0.3,0.5,0.7,1.0))
quan_30_korean.2014.art_psy <- quantile(merge_2014_korean.art_psy$second_korean, c(0,0.3,0.5,0.7,1.0))
quan_30_korean.2014.science <- quantile(merge_2014_korean.science$second_korean, c(0,0.3,0.5,0.7,1.0))

quan_30_english.2014.engineer <- quantile(merge_2014_english.engineer$second_english, c(0,0.3,0.5,0.7,1.0))
quan_30_english.2014.social <- quantile(merge_2014_english.social$second_english, c(0,0.3,0.5,0.7,1.0))
quan_30_english.2014.art_psy <- quantile(merge_2014_english.art_psy$second_english, c(0,0.3,0.5,0.7,1.0))
quan_30_english.2014.science <- quantile(merge_2014_english.science$second_english, c(0,0.3,0.5,0.7,1.0))

quan_30_math.2014.engineer <- quantile(merge_2014_math.engineer$second_math, c(0,0.3,0.5,0.7,1.0))

quan_30_korean.2015.engineer <- quantile(merge_2015_korean.engineer$second_korean, c(0,0.3,0.5,0.7,1.0))
quan_30_korean.2015.social <- quantile(merge_2015_korean.social$second_korean, c(0,0.3,0.5,0.7,1.0))
quan_30_korean.2015.art_psy <- quantile(merge_2015_korean.art_psy$second_korean, c(0,0.3,0.5,0.7,1.0))
quan_30_korean.2015.science <- quantile(merge_2015_korean.science$second_korean, c(0,0.3,0.5,0.7,1.0))

quan_30_english.2015.engineer <- quantile(merge_2015_english.engineer$second_english, c(0,0.3,0.5,0.7,1.0))
quan_30_english.2015.social <- quantile(merge_2015_english.social$second_english, c(0,0.3,0.5,0.7,1.0))
quan_30_english.2015.art_psy <- quantile(merge_2015_english.art_psy$second_english, c(0,0.3,0.5,0.7,1.0))
quan_30_english.2015.science <- quantile(merge_2015_english.science$second_english, c(0,0.3,0.5,0.7,1.0))

quan_30_math.2015.engineer <- quantile(merge_2015_math.engineer$second_math, c(0,0.3,0.5,0.7,1.0))

quan_40_korean.2014.engineer <- quantile(merge_2014_korean.engineer$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_korean.2014.social <- quantile(merge_2014_korean.social$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_korean.2014.art_psy <- quantile(merge_2014_korean.art_psy$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_korean.2014.science <- quantile(merge_2014_korean.science$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))

quan_40_english.2014.engineer <- quantile(merge_2014_english.engineer$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2014.social <- quantile(merge_2014_english.social$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2014.art_psy <- quantile(merge_2014_english.art_psy$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2014.science <- quantile(merge_2014_english.science$second_english, c(0,0.2,0.4,0.6,0.8,1.0))

quan_40_math.2014.engineer <- quantile(merge_2014_math.engineer$second_math, c(0,0.2,0.4,0.6,0.8,1.0))

quan_40_korean.2015.engineer <- quantile(merge_2015_korean.engineer$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_korean.2015.social <- quantile(merge_2015_korean.social$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_korean.2015.art_psy <- quantile(merge_2015_korean.art_psy$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_korean.2015.science <- quantile(merge_2015_korean.science$second_korean, c(0,0.2,0.4,0.6,0.8,1.0))

quan_40_english.2015.engineer <- quantile(merge_2015_english.engineer$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2015.social <- quantile(merge_2015_english.social$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2015.art_psy <- quantile(merge_2015_english.art_psy$second_english, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40_english.2015.science <- quantile(merge_2015_english.science$second_english, c(0,0.2,0.4,0.6,0.8,1.0))

quan_40_math.2015.engineer <- quantile(merge_2015_math.engineer$second_math, c(0,0.2,0.4,0.6,0.8,1.0))

##### High 30% (split Department) #######################################################################
merge_2014_korean_UP30.engineer<- merge_2014_korean.engineer[merge_2014_korean.engineer$second_korean >= quan_30_korean.2014.engineer[4],]
merge_2014_korean_UP30.social<- merge_2014_korean.social[merge_2014_korean.social$second_korean >= quan_30_korean.2014.social[4],]
merge_2014_korean_UP30.art_psy<- merge_2014_korean.art_psy[merge_2014_korean.art_psy$second_korean >= quan_30_korean.2014.art_psy[4],]
merge_2014_korean_UP30.science<- merge_2014_korean.science[merge_2014_korean.science$second_korean >= quan_30_korean.2014.science[4],]

merge_2015_korean_UP30.engineer<- merge_2015_korean.engineer[merge_2015_korean.engineer$second_korean >= quan_30_korean.2015.engineer[4],]
merge_2015_korean_UP30.social<- merge_2015_korean.social[merge_2015_korean.social$second_korean >= quan_30_korean.2015.social[4],]
merge_2015_korean_UP30.art_psy<- merge_2015_korean.art_psy[merge_2015_korean.art_psy$second_korean >= quan_30_korean.2015.art_psy[4],]
merge_2015_korean_UP30.science<- merge_2015_korean.science[merge_2015_korean.science$second_korean >= quan_30_korean.2015.science[4],]

merge_2014_english_UP30.engineer<- merge_2014_english.engineer[merge_2014_english.engineer$second_english >= quan_30_english.2014.engineer[4],]
merge_2014_english_UP30.social<- merge_2014_english.social[merge_2014_english.social$second_english >= quan_30_english.2014.social[4],]
merge_2014_english_UP30.art_psy<- merge_2014_english.art_psy[merge_2014_english.art_psy$second_english >= quan_30_english.2014.art_psy[4],]
merge_2014_english_UP30.science<- merge_2014_english.science[merge_2014_english.science$second_english >= quan_30_english.2014.science[4],]

merge_2015_english_UP30.engineer<- merge_2015_english.engineer[merge_2015_english.engineer$second_english >= quan_30_english.2015.engineer[4],]
merge_2015_english_UP30.social<- merge_2015_english.social[merge_2015_english.social$second_english >= quan_30_english.2015.social[4],]
merge_2015_english_UP30.art_psy<- merge_2015_english.art_psy[merge_2015_english.art_psy$second_english >= quan_30_english.2015.art_psy[4],]
merge_2015_english_UP30.science<- merge_2015_english.science[merge_2015_english.science$second_english >= quan_30_english.2015.science[4],]

merge_2014_math_UP30.engineer<- merge_2014_math.engineer[merge_2014_math.engineer$second_math >= quan_30_math.2014.engineer[4],]

merge_2015_math_UP30.engineer<- merge_2015_math.engineer[merge_2015_math.engineer$second_math >= quan_30_math.2015.engineer[4],]

########## 2014 ###########################################################################
fit.UP30.korean.2014.engineer <- glm(Direction~second_korean, data = merge_2014_korean_UP30.engineer, family = binomial)
summary(fit.UP30.korean.2014.engineer)
exp(0.04100) # 1.041852

fit.UP30.korean.2014.social <- glm(Direction~second_korean, data = merge_2014_korean_UP30.social, family = binomial)
summary(fit.UP30.korean.2014.social)
exp(0.07147) # 1.074086

fit.UP30.korean.2014.art_psy <- glm(Direction~second_korean, data = merge_2014_korean_UP30.art_psy, family = binomial)
summary(fit.UP30.korean.2014.art_psy)
exp(0.14563) # 1.156768

fit.UP30.korean.2014.science <- glm(Direction~second_korean, data = merge_2014_korean_UP30.science, family = binomial)
summary(fit.UP30.korean.2014.science)
exp(-0.02042) # 0.9797871

fit.UP30.english.2014.engineer <- glm(Direction~second_english, data = merge_2014_english_UP30.engineer, family = binomial)
summary(fit.UP30.english.2014.engineer)
exp(-0.00450) # 0.9955101

fit.UP30.english.2014.social <- glm(Direction~second_english, data = merge_2014_english_UP30.social, family = binomial)
summary(fit.UP30.english.2014.social)
exp(0.06122) # 1.063133

fit.UP30.english.2014.art_psy <- glm(Direction~second_english, data = merge_2014_english_UP30.art_psy, family = binomial)
summary(fit.UP30.english.2014.art_psy)
exp(0.04936) # 1.050598

fit.UP30.english.2014.science <- glm(Direction~second_english, data = merge_2014_english_UP30.science, family = binomial)
summary(fit.UP30.english.2014.science)
exp(-0.007706) # 0.9923236

fit.UP30.math.2014.engineer <- glm(Direction~second_math, data = merge_2014_math_UP30.engineer, family = binomial)
summary(fit.UP30.math.2014.engineer)
exp(0.02894) # 1.029363

###########################################################################################
########## 2015 ###########################################################################
fit.UP30.korean.2015.engineer <- glm(Direction~second_korean, data = merge_2015_korean_UP30.engineer, family = binomial)
summary(fit.UP30.korean.2015.engineer)
exp(0.11300) # 1.119632

fit.UP30.korean.2015.social <- glm(Direction~second_korean, data = merge_2015_korean_UP30.social, family = binomial)
summary(fit.UP30.korean.2015.social)
exp(0.08741) # 1.091344

fit.UP30.korean.2015.art_psy <- glm(Direction~second_korean, data = merge_2015_korean_UP30.art_psy, family = binomial)
summary(fit.UP30.korean.2015.art_psy)
exp(0.03370) # 1.034274

fit.UP30.korean.2015.science <- glm(Direction~second_korean, data = merge_2015_korean_UP30.science, family = binomial)
summary(fit.UP30.korean.2015.science)
exp(0.11366) # 1.120371

fit.UP30.english.2015.engineer <- glm(Direction~second_english, data = merge_2015_english_UP30.engineer, family = binomial)
summary(fit.UP30.english.2015.engineer)
exp(0.04254) # 1.043458

fit.UP30.english.2015.social <- glm(Direction~second_english, data = merge_2015_english_UP30.social, family = binomial)
summary(fit.UP30.english.2015.social)
exp(0.05615) # 1.057756

fit.UP30.english.2015.art_psy <- glm(Direction~second_english, data = merge_2015_english_UP30.art_psy, family = binomial)
summary(fit.UP30.english.2015.art_psy)
exp(0.04296) # 1.043896

fit.UP30.english.2015.science <- glm(Direction~second_english, data = merge_2015_english_UP30.science, family = binomial)
summary(fit.UP30.english.2015.science)
exp(-0.008937) # 0.9911028

fit.UP30.math.2015.engineer <- glm(Direction~second_math, data = merge_2015_math_UP30.engineer, family = binomial)
summary(fit.UP30.math.2015.engineer)
exp(0.09699) # 1.101849
###########################################################################################
#########################################################################################################
##### Lower 30% (split Department) #######################################################################
merge_2014_korean_DOWN30.engineer<- merge_2014_korean.engineer[merge_2014_korean.engineer$second_korean <= quan_30_korean.2014.engineer[2],]
merge_2014_korean_DOWN30.social<- merge_2014_korean.social[merge_2014_korean.social$second_korean <= quan_30_korean.2014.social[2],]
merge_2014_korean_DOWN30.art_psy<- merge_2014_korean.art_psy[merge_2014_korean.art_psy$second_korean <= quan_30_korean.2014.art_psy[2],]
merge_2014_korean_DOWN30.science<- merge_2014_korean.science[merge_2014_korean.science$second_korean <= quan_30_korean.2014.science[2],]

merge_2015_korean_DOWN30.engineer<- merge_2015_korean.engineer[merge_2015_korean.engineer$second_korean <= quan_30_korean.2015.engineer[2],]
merge_2015_korean_DOWN30.social<- merge_2015_korean.social[merge_2015_korean.social$second_korean <= quan_30_korean.2015.social[2],]
merge_2015_korean_DOWN30.art_psy<- merge_2015_korean.art_psy[merge_2015_korean.art_psy$second_korean <= quan_30_korean.2015.art_psy[2],]
merge_2015_korean_DOWN30.science<- merge_2015_korean.science[merge_2015_korean.science$second_korean <= quan_30_korean.2015.science[2],]

merge_2014_english_DOWN30.engineer<- merge_2014_english.engineer[merge_2014_english.engineer$second_english <= quan_30_english.2014.engineer[2],]
merge_2014_english_DOWN30.social<- merge_2014_english.social[merge_2014_english.social$second_english <= quan_30_english.2014.social[2],]
merge_2014_english_DOWN30.art_psy<- merge_2014_english.art_psy[merge_2014_english.art_psy$second_english <= quan_30_english.2014.art_psy[2],]
merge_2014_english_DOWN30.science<- merge_2014_english.science[merge_2014_english.science$second_english <= quan_30_english.2014.science[2],]

merge_2015_english_DOWN30.engineer<- merge_2015_english.engineer[merge_2015_english.engineer$second_english <= quan_30_english.2015.engineer[2],]
merge_2015_english_DOWN30.social<- merge_2015_english.social[merge_2015_english.social$second_english <= quan_30_english.2015.social[2],]
merge_2015_english_DOWN30.art_psy<- merge_2015_english.art_psy[merge_2015_english.art_psy$second_english <= quan_30_english.2015.art_psy[2],]
merge_2015_english_DOWN30.science<- merge_2015_english.science[merge_2015_english.science$second_english <= quan_30_english.2015.science[2],]

merge_2014_math_DOWN30.engineer<- merge_2014_math.engineer[merge_2014_math.engineer$second_math <= quan_30_math.2014.engineer[2],]

merge_2015_math_DOWN30.engineer<- merge_2015_math.engineer[merge_2015_math.engineer$second_math <= quan_30_math.2015.engineer[2],]

########## 2014 ###########################################################################
fit.DOWN30.korean.2014.engineer <- glm(Direction~second_korean, data = merge_2014_korean_DOWN30.engineer, family = binomial)
summary(fit.DOWN30.korean.2014.engineer)
exp(0.05260) # 1.054008

fit.DOWN30.korean.2014.social <- glm(Direction~second_korean, data = merge_2014_korean_DOWN30.social, family = binomial)
summary(fit.DOWN30.korean.2014.social)
exp(0.02804) # 1.028437

fit.DOWN30.korean.2014.art_psy <- glm(Direction~second_korean, data = merge_2014_korean_DOWN30.art_psy, family = binomial)
summary(fit.DOWN30.korean.2014.art_psy)
exp(-0.0002237) # 0.9997763

fit.DOWN30.korean.2014.science <- glm(Direction~second_korean, data = merge_2014_korean_DOWN30.science, family = binomial)
summary(fit.DOWN30.korean.2014.science)
exp(-0.01451) # 0.9855948

fit.DOWN30.english.2014.engineer <- glm(Direction~second_english, data = merge_2014_english_DOWN30.engineer, family = binomial)
summary(fit.DOWN30.english.2014.engineer)
exp(0.12247) # 1.130285

fit.DOWN30.english.2014.social <- glm(Direction~second_english, data = merge_2014_english_DOWN30.social, family = binomial)
summary(fit.DOWN30.english.2014.social)
exp(0.05267) # 1.054082

fit.DOWN30.english.2014.art_psy <- glm(Direction~second_english, data = merge_2014_english_DOWN30.art_psy, family = binomial)
summary(fit.DOWN30.english.2014.art_psy)
exp(0.01537) # 1.015489

fit.DOWN30.english.2014.science <- glm(Direction~second_english, data = merge_2014_english_DOWN30.science, family = binomial)
summary(fit.DOWN30.english.2014.science)
exp(0.08217) # 1.08564

fit.DOWN30.math.2014.engineer <- glm(Direction~second_math, data = merge_2014_math_DOWN30.engineer, family = binomial)
summary(fit.DOWN30.math.2014.engineer)
exp(0.09738) # 1.102279

###########################################################################################
########## 2015 ###########################################################################
fit.DOWN30.korean.2015.engineer <- glm(Direction~second_korean, data = merge_2015_korean_DOWN30.engineer, family = binomial)
summary(fit.DOWN30.korean.2015.engineer)
exp(0.04906) # 1.050283

fit.DOWN30.korean.2015.social <- glm(Direction~second_korean, data = merge_2015_korean_DOWN30.social, family = binomial)
summary(fit.DOWN30.korean.2015.social)
exp(0.05153) # 1.052881

fit.DOWN30.korean.2015.art_psy <- glm(Direction~second_korean, data = merge_2015_korean_DOWN30.art_psy, family = binomial)
summary(fit.DOWN30.korean.2015.art_psy)
exp(0.03817) # 1.038908

fit.DOWN30.korean.2015.science <- glm(Direction~second_korean, data = merge_2015_korean_DOWN30.science, family = binomial)
summary(fit.DOWN30.korean.2015.science)
exp(0.01757) # 1.017725

fit.DOWN30.english.2015.engineer <- glm(Direction~second_english, data = merge_2015_english_DOWN30.engineer, family = binomial)
summary(fit.DOWN30.english.2015.engineer)
exp(0.07063) # 1.073184

fit.DOWN30.english.2015.social <- glm(Direction~second_english, data = merge_2015_english_DOWN30.social, family = binomial)
summary(fit.DOWN30.english.2015.social)
exp(0.09652) # 1.101332

fit.DOWN30.english.2015.art_psy <- glm(Direction~second_english, data = merge_2015_english_DOWN30.art_psy, family = binomial)
summary(fit.DOWN30.english.2015.art_psy)
exp(0.07437) # 1.077205

fit.DOWN30.english.2015.science <- glm(Direction~second_english, data = merge_2015_english_DOWN30.science, family = binomial)
summary(fit.DOWN30.english.2015.science)
exp(0.06404) # 1.066135

fit.DOWN30.math.2015.engineer <- glm(Direction~second_math, data = merge_2015_math_DOWN30.engineer, family = binomial)
summary(fit.DOWN30.math.2015.engineer)
exp(0.04656) # 1.047661
###########################################################################################
#########################################################################################################
##### Lower 40% (split Department) #######################################################################
merge_2014_korean_DOWN40.engineer<- merge_2014_korean.engineer[merge_2014_korean.engineer$second_korean <= quan_40_korean.2014.engineer[3],]
merge_2014_korean_DOWN40.social<- merge_2014_korean.social[merge_2014_korean.social$second_korean <= quan_40_korean.2014.social[3],]
merge_2014_korean_DOWN40.art_psy<- merge_2014_korean.art_psy[merge_2014_korean.art_psy$second_korean <= quan_40_korean.2014.art_psy[3],]
merge_2014_korean_DOWN40.science<- merge_2014_korean.science[merge_2014_korean.science$second_korean <= quan_40_korean.2014.science[3],]

merge_2015_korean_DOWN40.engineer<- merge_2015_korean.engineer[merge_2015_korean.engineer$second_korean <= quan_40_korean.2015.engineer[3],]
merge_2015_korean_DOWN40.social<- merge_2015_korean.social[merge_2015_korean.social$second_korean <= quan_40_korean.2015.social[3],]
merge_2015_korean_DOWN40.art_psy<- merge_2015_korean.art_psy[merge_2015_korean.art_psy$second_korean <= quan_40_korean.2015.art_psy[3],]
merge_2015_korean_DOWN40.science<- merge_2015_korean.science[merge_2015_korean.science$second_korean <= quan_40_korean.2015.science[3],]

merge_2014_english_DOWN40.engineer<- merge_2014_english.engineer[merge_2014_english.engineer$second_english <= quan_40_english.2014.engineer[3],]
merge_2014_english_DOWN40.social<- merge_2014_english.social[merge_2014_english.social$second_english <= quan_40_english.2014.social[3],]
merge_2014_english_DOWN40.art_psy<- merge_2014_english.art_psy[merge_2014_english.art_psy$second_english <= quan_40_english.2014.art_psy[3],]
merge_2014_english_DOWN40.science<- merge_2014_english.science[merge_2014_english.science$second_english <= quan_40_english.2014.science[3],]

merge_2015_english_DOWN40.engineer<- merge_2015_english.engineer[merge_2015_english.engineer$second_english <= quan_40_english.2015.engineer[3],]
merge_2015_english_DOWN40.social<- merge_2015_english.social[merge_2015_english.social$second_english <= quan_40_english.2015.social[3],]
merge_2015_english_DOWN40.art_psy<- merge_2015_english.art_psy[merge_2015_english.art_psy$second_english <= quan_40_english.2015.art_psy[3],]
merge_2015_english_DOWN40.science<- merge_2015_english.science[merge_2015_english.science$second_english <= quan_40_english.2015.science[3],]

merge_2014_math_DOWN40.engineer<- merge_2014_math.engineer[merge_2014_math.engineer$second_math <= quan_40_math.2014.engineer[3],]

merge_2015_math_DOWN40.engineer<- merge_2015_math.engineer[merge_2015_math.engineer$second_math <= quan_40_math.2015.engineer[3],]

########## 2014 ###########################################################################
fit.DOWN40.korean.2014.engineer <- glm(Direction~second_korean, data = merge_2014_korean_DOWN40.engineer, family = binomial)
summary(fit.DOWN40.korean.2014.engineer)
exp(0.03658) # 1.037257

fit.DOWN40.korean.2014.social <- glm(Direction~second_korean, data = merge_2014_korean_DOWN40.social, family = binomial)
summary(fit.DOWN40.korean.2014.social)
exp(0.01559) # 1.015712

fit.DOWN40.korean.2014.art_psy <- glm(Direction~second_korean, data = merge_2014_korean_DOWN40.art_psy, family = binomial)
summary(fit.DOWN40.korean.2014.art_psy)
exp(-0.0002237) # 0.9997763

fit.DOWN40.korean.2014.science <- glm(Direction~second_korean, data = merge_2014_korean_DOWN40.science, family = binomial)
summary(fit.DOWN40.korean.2014.science)
exp(0.04169) # 1.042571

fit.DOWN40.english.2014.engineer <- glm(Direction~second_english, data = merge_2014_english_DOWN40.engineer, family = binomial)
summary(fit.DOWN40.english.2014.engineer)
exp(0.09276) # 1.097198

fit.DOWN40.english.2014.social <- glm(Direction~second_english, data = merge_2014_english_DOWN40.social, family = binomial)
summary(fit.DOWN40.english.2014.social)
exp(0.06785) # 1.070205

fit.DOWN40.english.2014.art_psy <- glm(Direction~second_english, data = merge_2014_english_DOWN40.art_psy, family = binomial)
summary(fit.DOWN40.english.2014.art_psy)
exp(0.001263) # 1.001264

fit.DOWN40.english.2014.science <- glm(Direction~second_english, data = merge_2014_english_DOWN40.science, family = binomial)
summary(fit.DOWN40.english.2014.science)
exp(0.04295) # 1.043886

fit.DOWN40.math.2014.engineer <- glm(Direction~second_math, data = merge_2014_math_DOWN40.engineer, family = binomial)
summary(fit.DOWN40.math.2014.engineer)
exp(0.07578) # 1.078725

###########################################################################################
########## 2015 ###########################################################################
fit.DOWN40.korean.2015.engineer <- glm(Direction~second_korean, data = merge_2015_korean_DOWN40.engineer, family = binomial)
summary(fit.DOWN40.korean.2015.engineer)
exp(0.039573) # 1.040366

fit.DOWN40.korean.2015.social <- glm(Direction~second_korean, data = merge_2015_korean_DOWN40.social, family = binomial)
summary(fit.DOWN40.korean.2015.social)
exp(0.04022) # 1.04104

fit.DOWN40.korean.2015.art_psy <- glm(Direction~second_korean, data = merge_2015_korean_DOWN40.art_psy, family = binomial)
summary(fit.DOWN40.korean.2015.art_psy)
exp(0.04436) # 1.045359

fit.DOWN40.korean.2015.science <- glm(Direction~second_korean, data = merge_2015_korean_DOWN40.science, family = binomial)
summary(fit.DOWN40.korean.2015.science)
exp(0.04549) # 1.046541

fit.DOWN40.english.2015.engineer <- glm(Direction~second_english, data = merge_2015_english_DOWN40.engineer, family = binomial)
summary(fit.DOWN40.english.2015.engineer)
exp(0.050159) # 1.051438

fit.DOWN40.english.2015.social <- glm(Direction~second_english, data = merge_2015_english_DOWN40.social, family = binomial)
summary(fit.DOWN40.english.2015.social)
exp(0.06907) # 1.071511

fit.DOWN40.english.2015.art_psy <- glm(Direction~second_english, data = merge_2015_english_DOWN40.art_psy, family = binomial)
summary(fit.DOWN40.english.2015.art_psy)
exp(0.05114) # 1.05247

fit.DOWN40.english.2015.science <- glm(Direction~second_english, data = merge_2015_english_DOWN40.science, family = binomial)
summary(fit.DOWN40.english.2015.science)
exp(0.04935) # 1.050588

fit.DOWN40.math.2015.engineer <- glm(Direction~second_math, data = merge_2015_math_DOWN40.engineer, family = binomial)
summary(fit.DOWN40.math.2015.engineer)
exp(0.029620) # 1.030063
###########################################################################################
#########################################################################################################
#########################################################################################################################################

### Add Data Analytic(Re Education)##################################################################
re_edu <- loadWorkbook("2015_re.xlsx")
re_edu_korean <- readWorksheet(re_edu, 1)
re_edu_english <- readWorksheet(re_edu, 2)
re_edu_math <- readWorksheet(re_edu, 3)

edu_2015_re.korean <- subset(merge_2015_korean, 성명 %in% re_edu_korean$성명)
edu_2015_re.english <- subset(merge_2015_english, 성명 %in% re_edu_english$성명)
edu_2015_re.math <- subset(merge_2015_math, 성명 %in% re_edu_math$성명)

## re education list 
nrow(edu_2015_re.korean) ## 국어 55명
nrow(edu_2015_re.english) ## 영어 75명
nrow(edu_2015_re.math) ## 2 명

fit.re.korean <- glm(Direction~second_korean, data = edu_2015_re.korean, family = binomial)
summary(fit.re.korean)
exp(0.05252) ## 1.053924
fit.re.english <- glm(Direction~second_english, data = edu_2015_re.english, family = binomial)
summary(fit.re.english)
exp(0.05046) ## 1.051755
fit.re.math <- glm(Direction~second_math, data = edu_2015_re.math, family = binomial)
summary(fit.re.math)
exp(1.218e-15) ## 1 (because very small data : 2)


#### Error Rate ##########################################################
########### 2015 ###############################
table(edu_2015_re.korean$Direction) ## DOWN : 37 , UP : 18
re.err.2015.korean <- getCutOff(edu_2015_re.korean, fit.re.korean) # cutoff : 0.3872484
xtabs(~edu_2015_re.korean$Direction+(fit.re.korean$fitted.values> re.err.2015.korean))
# merge_2015_korean$Direction FALSE TRUE
#                        DOWN    30    7
#                        UP       8   10
## Sensitivity = 10 / (10+8) : 0.556
## Specificity = 30 / (30+7) : 0.811
## Error Rate = (7+8) / (37+18) : 0.273
table(edu_2015_re.english$Direction) ## DOWN : 46 , UP : 29
re.err.2015.english <- getCutOff(edu_2015_re.english, fit.re.english) # 0.5578644
xtabs(~edu_2015_re.english$Direction+(fit.re.english$fitted.values > re.err.2015.english))
# edu_2015_re.english$Direction FALSE TRUE
#                         DOWN    39    7
#                         UP      16   13 
## Sensitivity = 13 / (16+13) : 0.118
## Specificity = 39 / (39+7) : 0.848
## Error Rate = (7+16) / (46+29) : 0.307
################################################
##########################################################################
#####################################################################################################