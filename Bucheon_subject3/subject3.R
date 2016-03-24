library(XLConnect)
library(ROCR)
tot_score1 <- loadWorkbook('2015_1.xls')
tot_score2 <- loadWorkbook('2015_2.xls')

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

tot_score1.df <- readWorksheet(tot_score1, 1)
tot_score2.df <- readWorksheet(tot_score2, 1)

merge_score <- merge(tot_score1.df, tot_score2.df)
head(merge_score)
str(merge_score)
merge_score$FIRST <- as.numeric(merge_score$FIRST)
merge_score$SECOND <- as.numeric(merge_score$SECOND)

merge_score$Direction <- 1
merge_score$Direction[merge_score$SECOND - merge_score$FIRST > 0] <- "UP"
merge_score$Direction[merge_score$SECOND - merge_score$FIRST <= 0] <- "DOWN"

except_dept <- c("건축공학과","실내건축학과","토목공학과","디지털콘텐츠학과","컴퓨터소프트웨어공학과","전자공학과","정보통신공학과","컴퓨터제어공학과","e-비즈니스학과","섬유의류비즈니스학과","산업디자인학과","패션디자인학과","생활스포츠학과","식품영양학과","경영학과","관광경영학과","부동산금융정보학과","비서학과","세무회계학과","행정학과","유아교육과","사회복지학과")
engineer <- c("건축과","실내건축디자인과","토목과","컴퓨터정보보안과","컴퓨터소프트웨어과","전자과","정보통신과","지능로봇과","e-비즈니스과","섬유패션비즈니스과","영상&게임콘텐츠과", "실내건축과", "디지털콘텐츠과", "모바일통신과", "컴퓨터제어과", "섬유의류비즈니스과")
social <- c("경영과","호텔관광경영과","부동산유통학과","비서사무행정과","세무회계과","유아교육과","사회복지과","영유아보육과","항공서비스과", "관광경영과", "부동산금융정보과", "비서과", "행정과")
art_psy <- c("광고디자인전공","산업디자인과","재활스포츠학과", "패션디자인과", "만화&2D영상그래픽전공", "3D영상그래픽전공", "쥬얼리디자인전공", "실용사진전공", "생활스포츠과")
science <- c("식품영양과","호텔외식조리학과","간호학과", "호텔외식조리과", "간호과")

#### 전체 ################################################################################################
merge_score <- subset(merge_score, !(학과 %in% except_dept))
merge_score <- merge_score[-c(24748,25019,25085,27156),]
merge_score <- merge_score[-c(901,1220,1286,3564),]
merge_score$Direction <- as.factor(merge_score$Direction)

fit.tot <- glm(Direction ~ SECOND, data = merge_score, family = binomial)
summary(fit.tot)
exp(0.115479) # 1.122411

table(merge_score$Direction) ## DOWN : 3378 , UP : 2599
err.tot <- getCutOff(merge_score, fit.tot) # cutoff : 0.4946292
xtabs(~merge_score$Direction+(fit.tot$fitted.values> err.tot))

# merge_score$Direction FALSE TRUE
#                  DOWN  2524  852
#                  UP    1003 1596
## Sensitivity = 1596 / (1003+1596) : 0.614
## Specificity = 2524 / (2524+852) : 0.748
## Error Rate = (852+1003) / (3378+2599) : 0.310
#### 계열분리 #############################################################################################
merge_score.engineer <- subset(merge_score, 학과 %in% engineer)
fit.engineer <- glm(Direction~SECOND, data = merge_score.engineer, family = binomial)
summary(fit.engineer)
exp(0.10933) # 1.11553

table(merge_score.engineer$Direction) ## DOWN : 1360 , UP : 989
err.engineer <- getCutOff(merge_score.engineer, fit.engineer) # cutoff : 0.490392
xtabs(~merge_score.engineer$Direction+(fit.engineer$fitted.values> err.engineer))
# merge_score$Direction FALSE TRUE
#                  DOWN  1041  319
#                  UP     416  573
## Sensitivity = 573 / (416+573) : 0.579
## Specificity = 1041 / (1041+319) : 0.765
## Error Rate = (319+416) / (1360+989) : 0.313

merge_score.social <- subset(merge_score, 학과 %in% social)
fit.social <- glm(Direction~SECOND, data = merge_score.social, family = binomial)
summary(fit.social)
exp(0.118190) # 1.125458

table(merge_score.social$Direction) ## DOWN : 1100 , UP : 822
err.social <- getCutOff(merge_score.social, fit.social) # cutoff : 0.5133138
xtabs(~merge_score.social$Direction+(fit.social$fitted.values> err.social))
# merge_score$Direction FALSE TRUE
#                  DOWN   871  229
#                  UP     350  472
## Sensitivity = 472 / (350+472) : 0.574
## Specificity = 871 / (871+229) : 0.792
## Error Rate = (229+350) / (1100+822) : 0.301

merge_score.art_psy <- subset(merge_score, 학과 %in% art_psy)
fit.art_psy <- glm(Direction~SECOND, data = merge_score.art_psy, family = binomial)
summary(fit.art_psy) 
exp(0.09710) # 1.101971

table(merge_score.art_psy$Direction) ## DOWN : 300 , UP : 222
err.art_psy <- getCutOff(merge_score.art_psy, fit.art_psy) # cutoff : 0.4797738
xtabs(~merge_score.art_psy$Direction+(fit.art_psy$fitted.values> err.art_psy))
# merge_score$Direction FALSE TRUE
#                  DOWN   213   87
#                  UP      88  134
## Sensitivity = 134 / (134+88) : 0.604
## Specificity = 213 / (213+87) : 0.71
## Error Rate = (87+88) / (300+222) : 0.335

merge_score.science <- subset(merge_score, 학과 %in% science)
fit.science <- glm(Direction~SECOND, data = merge_score.science, family = binomial)
summary(fit.science)
exp(0.1441) # 1.155 

table(merge_score.science$Direction) ## DOWN : 1360 , UP : 989
err.science <- getCutOff(merge_score.science, fit.science) # cutoff : 0.490392
xtabs(~merge_score.science$Direction+(fit.science$fitted.values> err.science))
# merge_score$Direction FALSE TRUE
#                  DOWN  1041  319
#                  UP     416  573
## Sensitivity = 573 / (416+573) : 0.579
## Specificity = 1041 / (1041+319) : 0.765
## Error Rate = (319+416) / (1360+989) : 0.313


###########################################################################################################

######################################################################################################################################
quan_30 <- quantile(merge_score$SECOND, c(0,0.3,0.5,0.7,1.0), na.rm= TRUE)
quan_30.engineer <- quantile(merge_score.engineer$SECOND, c(0,0.3,0.5,0.7,1.0), na.rm= TRUE)
quan_30.social <- quantile(merge_score.social$SECOND, c(0,0.3,0.5,0.7,1.0), na.rm = TRUE)
quan_30.art_psy <- quantile(merge_score.art_psy$SECOND, c(0,0.3,0.5,0.7,1.0), na.rm = TRUE)
quan_30.science <- quantile(merge_score.science$SECOND , c(0,0.3,0.5,0.7,1.0), na.rm = TRUE)

quan_40 <- quantile(merge_score$SECOND, c(0,0.2,0.4,0.6,0.8,1.0))
quan_40.engineer <- quantile(merge_score.engineer$SECOND, c(0,0.2,0.4,0.6,0.8,1.0), na.rm= TRUE)
quan_40.social <- quantile(merge_score.social$SECOND, c(0,0.2,0.4,0.6,0.8,1.0), na.rm = TRUE)
quan_40.art_psy <- quantile(merge_score.art_psy$SECOND, c(0,0.2,0.4,0.6,0.8,1.0), na.rm = TRUE)
quan_40.science <- quantile(merge_score.science$SECOND , c(0,0.2,0.4,0.6,0.8,1.0), na.rm = TRUE)

#### 상위 30%(전체) #############################################################################################
merge_score_UP30 <- merge_score[merge_score$SECOND >= quan_30[4],]
fit.tot.UP30 <- glm(Direction~SECOND, data = merge_score_UP30, family = binomial)
summary(fit.tot.UP30)
exp(0.16790)  # 1.182818

table(merge_score_UP30$Direction) ## DOWN : 1360 , UP : 989
err.UP30 <- getCutOff(merge_score_UP30, fit.tot.UP30) # cutoff : 0.5486676 0.5482518
xtabs(~merge_score_UP30$Direction+(fit.tot.UP30$fitted.values> err.UP30))
# merge_score$Direction FALSE TRUE
#                  DOWN    39  527
#                  UP      37 1191
## Sensitivity = 1191 / (37+1191) : 0.970
## Specificity = 39 / (39+527) : 0.069
## Error Rate = (527+37) / (1360+989) : 0.240

#################################################################################################################
#### 상위 30%(계열) #############################################################################################
merge_score.engineer_UP30 <- merge_score.engineer[merge_score.engineer$SECOND >= quan_30.engineer[4],]
fit.engineer.UP30 <- glm(Direction~SECOND, data = merge_score.engineer_UP30, family = binomial)
summary(fit.engineer.UP30)
exp(0.15093) # 1.162915

table(merge_score.engineer_UP30$Direction) ## DOWN : 248 , UP : 458
err.engineer.UP30 <- getCutOff(merge_score.engineer_UP30, fit.engineer.UP30) # cutoff : 0.544227
xtabs(~merge_score.engineer_UP30$Direction+(fit.engineer.UP30$fitted.values> err.engineer.UP30))
# merge_score$Direction FALSE TRUE
#                  DOWN    53  195
#                  UP      49  409
## Sensitivity = 409 / (49+409) : 0.893
## Specificity = 53 / (53+195) : 0.214
## Error Rate = (195+49) / (248+458) : 0.346

merge_score.social_UP30 <- merge_score.social[merge_score.social$SECOND >= quan_30.social[4],]
fit.social.UP30 <- glm(Direction~SECOND, data = merge_score.social_UP30, family = binomial)
summary(fit.social.UP30)
exp(0.18797) # 0.18797

table(merge_score.social_UP30$Direction) ## DOWN : 178 , UP : 401
err.social.UP30 <- getCutOff(merge_score.social_UP30, fit.social.UP30) # cutoff : 0.5498255 0.5451686
xtabs(~merge_score.social_UP30$Direction+(fit.social.UP30$fitted.values> err.social.UP30))
# merge_score$Direction FALSE TRUE
#                  DOWN    23  155
#                  UP      19  382
## Sensitivity = 382 / (382+19) : 0.953
## Specificity = 23 / (23+155) : 0.129
## Error Rate = (155+19) / (178+401) : 0.3

merge_score.art_psy_UP30 <- merge_score.art_psy[merge_score.art_psy$SECOND >= quan_30.art_psy[4],]
fit.art_psy.UP <- glm(Direction~SECOND, data = merge_score.art_psy_UP30, family = binomial)
summary(fit.art_psy.UP)
exp(0.18047) # 1.19778 

table(merge_score.art_psy_UP30$Direction) ## DOWN : 57 , UP : 100
err.art_psy.UP30 <- getCutOff(merge_score.art_psy_UP30, fit.art_psy.UP) # cutoff : 0.5015117
xtabs(~merge_score.art_psy_UP30$Direction+(fit.art_psy.UP$fitted.values> err.art_psy.UP30))
# merge_score$Direction FALSE TRUE
#                  DOWN    5    52
#                  UP      2    98
## Sensitivity = 98 / (2+98) : 0.98
## Specificity = 5 / (5+52) : 0.088
## Error Rate = (52+2) / (57+100) : 0.344

merge_score.science_UP30 <- merge_score.science[merge_score.science$SECOND >= quan_30.science[4],]
fit.science.UP <- glm(Direction~SECOND, data = merge_score.science_UP30, family = binomial)
summary(fit.science.UP) 
exp(0.23081) # 1.25962 

table(merge_score.science_UP30$Direction) ## DOWN : 39 , UP : 129
err.science.UP30 <- getCutOff(merge_score.science_UP30, fit.science.UP) # cutoff : 0.6295063
xtabs(~merge_score.science_UP30$Direction+(fit.science.UP$fitted.values> err.science.UP30))
# merge_score$Direction FALSE TRUE
#                  DOWN     4   35
#                  UP       3  126
## Sensitivity = 126 / (126+3) : 0.977
## Specificity =  4 / (4+35) : 0.103
## Error Rate = (35+3) / (39+129) : 0.226

#################################################################################################################
#### 하위 30%(전체) #############################################################################################
merge_score_DOWN30 <- merge_score[merge_score$SECOND <= quan_30[2],]
fit.tot.DOWN30 <- glm(Direction~SECOND, data = merge_score_DOWN30, family = binomial)
summary(fit.tot.DOWN30)
exp(0.039842)  #1.040646

table(merge_score_DOWN30$Direction) ## DOWN : 1420 , UP : 375
err.DOWN30 <- getCutOff(merge_score_DOWN30, fit.tot.DOWN30) # Inf
xtabs(~merge_score_DOWN30$Direction+(fit.tot.DOWN30$fitted.values> err.DOWN30))
# merge_score$Direction FALSE
#                  DOWN  1420
#                  UP     375
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
#################################################################################################################
#### 하위 30%(계열) #############################################################################################
merge_score.engineer_DOWN30 <- merge_score.engineer[merge_score.engineer$SECOND <= quan_30.engineer[2],]
fit.engineer.DOWN30 <- glm(Direction~SECOND, data = merge_score.engineer_DOWN30, family = binomial)
summary(fit.engineer.DOWN30)
exp(0.03371) # 1.034285

table(merge_score.engineer_DOWN30$Direction) ## DOWN : 568 , UP : 137
err.engineer.DOWN30 <- getCutOff(merge_score.engineer_DOWN30, fit.engineer.DOWN30) # cutoff : Inf
xtabs(~merge_score.engineer_DOWN30$Direction+(fit.engineer.DOWN30$fitted.values> err.engineer.DOWN30))
# merge_score$Direction FALSE
#                  DOWN   568
#                  UP     137
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?

merge_score.social_DOWN30 <- merge_score.social[merge_score.social$SECOND <= quan_30.social[2],]
fit.social.DOWN30 <- glm(Direction~SECOND, data = merge_score.social_DOWN30, family = binomial)
summary(fit.social.DOWN30)
exp(0.05353) # 1.054989

table(merge_score.social_DOWN30$Direction) ## DOWN : 459 , UP : 120
err.social.DOWN30 <- getCutOff(merge_score.social_DOWN30, fit.social.DOWN30) # cutoff : 0.2738212
xtabs(~merge_score.social_DOWN30$Direction+(fit.social.DOWN30$fitted.values> err.social.DOWN30))
# merge_score$Direction FALSE
#                  DOWN   459
#                  UP     120
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?

merge_score.art_psy_DOWN30 <- merge_score.art_psy[merge_score.art_psy$SECOND <= quan_30.art_psy[2],]
fit.art_psy.DOWN30 <- glm(Direction~SECOND, data = merge_score.art_psy_DOWN30, family = binomial)
summary(fit.art_psy.DOWN30)
exp(0.11445) # 1.121257

table(merge_score.art_psy_DOWN30$Direction) ## DOWN : 114 , UP : 43
err.art_psy.DOWN30 <- getCutOff(merge_score.art_psy_DOWN30, fit.art_psy.DOWN30) # cutoff : 0.4163132
xtabs(~merge_score.art_psy_DOWN30$Direction+(fit.art_psy.DOWN30$fitted.values> err.art_psy.DOWN30))
# merge_score$Direction FALSE TRUE
#                  DOWN   111    3
#                  UP      38    5
## Sensitivity = 5 / (38+5) : 0.1162791
## Specificity = 111 / (111+3) : 0.9736842
## Error Rate = (3+38) / (114+43) : 0.2611465


merge_score.science_DOWN30 <- merge_score.science[merge_score.science$SECOND <= quan_30.science[2],]
fit.science.DOWN30 <- glm(Direction~SECOND, data = merge_score.science_DOWN30, family = binomial)
summary(fit.science.DOWN30) 
exp(0.001457) # 1.001458

table(merge_score.art_psy_DOWN30$Direction) ## DOWN : 114 , UP : 43
err.art_psy.DOWN30 <- getCutOff(merge_score.art_psy_DOWN30, fit.art_psy.UP) # cutoff : 0.8671194 0.8511922
xtabs(~merge_score.art_psy_DOWN30$Direction+(fit.art_psy.UP$fitted.values> err.art_psy.DOWN30))
# merge_score$Direction FALSE TRUE
#                  DOWN   114    0
#                  UP      41    2
## Sensitivity = 2 / (41+2) : 0.047
## Specificity = 114 / (114) : 1
## Error Rate = (0+41) / (114+43) : 0.261
#################################################################################################################
#### 하위 40%(전체) #############################################################################################
merge_score_DOWN40 <- merge_score[merge_score$SECOND <= quan_40[2],]
fit.tot.DOWN40 <- glm(Direction~SECOND, data = merge_score_DOWN40, family = binomial)
summary(fit.tot.DOWN40)
exp(0.04600)  #  1.047074

table(merge_score_DOWN40$Direction) ## DOWN : 959 , UP : 236
err.DOWN40 <- getCutOff(merge_score_DOWN40, fit.tot.DOWN40) # cutoff : 0.2547010 0.2545264
xtabs(~merge_score_DOWN40$Direction+(fit.tot.DOWN40$fitted.values> err.DOWN40))
# merge_score$Direction FALSE TRUE
#                  DOWN   959    0
#                  UP     235    1
## Sensitivity = 1 / (235+1) : 0.004
## Specificity = 959 / (959) : 1
## Error Rate = (0+235) / (235+1) : 0.996

#################################################################################################################
#### 하위 40%(계열) #############################################################################################
merge_score.engineer_DOWN40 <- merge_score.engineer[merge_score.engineer$SECOND <= quan_40.engineer[2],]
fit.engineer.DOWN40 <- glm(Direction~SECOND, data = merge_score.engineer_DOWN40, family = binomial)
summary(fit.engineer.DOWN40)
exp(0.0305) # 1.031

table(merge_score.engineer_DOWN40$Direction) ## DOWN : 390 , UP : 82
err.engineer.DOWN40 <- getCutOff(merge_score.engineer_DOWN40, fit.engineer.DOWN40) # cutoff :  0.2119185 0.2118166
xtabs(~merge_score.engineer_DOWN40$Direction+(fit.engineer.DOWN40$fitted.values> err.engineer.DOWN40))
# merge_score$Direction FALSE TRUE
#                  DOWN   389    1
#                  UP      82    0
## Sensitivity = 0
## Specificity = 389 / (389+1) : 0.997
## Error Rate = (1+82) / (390+82) : 0.176

merge_score.social_DOWN40 <- merge_score.social[merge_score.social$SECOND <= quan_40.social[2],]
fit.social.DOWN40 <- glm(Direction~SECOND, data = merge_score.social_DOWN40, family = binomial)
summary(fit.social.DOWN40)
exp(0.05880) # 1.060563

table(merge_score.social_DOWN40$Direction) ## DOWN : 314 , UP : 72
err.social.DOWN40 <- getCutOff(merge_score.social_DOWN40, fit.social.DOWN40) # cutoff : Inf
xtabs(~merge_score.social_DOWN40$Direction+(fit.social.DOWN40$fitted.values> err.social.DOWN40))
# merge_score$Direction FALSE
#                  DOWN   314
#                  UP      72
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?

merge_score.art_psy_DOWN40 <- merge_score.art_psy[merge_score.art_psy$SECOND <= quan_40.art_psy[2],]
fit.art_psy.DOWN40 <- glm(Direction~SECOND, data = merge_score.art_psy_DOWN40, family = binomial)
summary(fit.art_psy.DOWN40)
exp(0.15115) # 1.163171

table(merge_score.art_psy_DOWN40$Direction) ## DOWN : 81 , UP : 24
err.art_psy.DOWN40 <- getCutOff(merge_score.art_psy_DOWN40, fit.art_psy.DOWN40) # cutoff : 0.5015117
xtabs(~merge_score.art_psy_DOWN40$Direction+(fit.art_psy.DOWN40$fitted.values> err.art_psy.DOWN40))
# merge_score$Direction FALSE TRUE
#                  DOWN   81     0
#                  UP     23     1
## Sensitivity = 1 / (23+1) :  0.041
## Specificity = 81 / (81) : 1
## Error Rate = (0+23) / (81+24) : 0.219

merge_score.science_DOWN40 <- merge_score.science[merge_score.science$SECOND <= quan_40.science[2],]
fit.science.DOWN40 <- glm(Direction~SECOND, data = merge_score.science_DOWN40, family = binomial)
summary(fit.science.DOWN40) 
exp(0.01170) # 1.011769

table(merge_score.science_DOWN40$Direction) ## DOWN : 87 , UP : 25
err.science.DOWN40 <- getCutOff(merge_score.science_DOWN40, fit.science.DOWN40) # cutoff : Inf
xtabs(~merge_score.science_DOWN40$Direction+(fit.science.DOWN40$fitted.values> err.science.DOWN40))
# merge_score$Direction FALSE
#                  DOWN    87 
#                  UP      25  
## Sensitivity = ?
## Specificity = ?
## Error Rate = ?
#################################################################################################################
#################################################################################################################
#### 재교육 받은 학생 ###########################################################################################
re_edu <- loadWorkbook('re.xlsx')
re_edu.df <- readWorksheet(re_edu, 1)
head(re_edu.df)

merge_score.re <- subset(merge_score, 성명 %in% re_edu.df$성명)
nrow(merge_score.re) # 794

fit.re_edu <- glm(Direction~SECOND, data = merge_score.re , family = binomial)
summary(fit.re_edu)
exp(0.11943)  # 1.126854

table(merge_score.re$Direction) ## DOWN : 445 , UP : 349
err.re <- getCutOff(merge_score.re, fit.re_edu) # cutoff : 0.5646323 0.4867719
xtabs(~merge_score.re$Direction+(fit.re_edu$fitted.values> err.re))
# merge_score$Direction FALSE TRUE
#                  DOWN   353   92
#                  UP     134  215
## Sensitivity = 215 / (134+215) : 0.616
## Specificity = 353 / (353+92) : 0.793
## Error Rate = (92+134) / (445+349) : 0.285

#################################################################################################################

### re education subject odds 
re_eval_sub <- loadWorkbook('re_subject.xlsx')
re_eval_sub_cnt3 <- readWorksheet(re_eval_sub, 1)
re_eval_sub_cnt2 <- readWorksheet(re_eval_sub, 2)

head(re_eval_sub_cnt3)
head(re_eval_sub_cnt2)

col_cnt3 <- colnames(re_eval_sub_cnt3)
col_cnt2 <- colnames(re_eval_sub_cnt2)

re_eval_sub_cnt3$Direction <- factor(re_eval_sub_cnt3$Direction)
re_eval_sub_cnt2$Direction <- factor(re_eval_sub_cnt2$Direction)

getOdds <- function (mainData, subject_c) {
   print (summary(glm(Direction~score3, data = subset(mainData, subject == subject_c) , family = binomial)))
  return (glm(Direction~score3, data = subset(mainData, subject == subject_c) , family = binomial))
}

# subject name
subject_cnt3 <- unique(re_eval_sub_cnt3$subject)
subject_cnt2 <- unique(re_eval_sub_cnt2$subject)

## full subject count3
fit.cnt3 <- glm(Direction~score3, data = re_eval_sub_cnt3,  family = binomial)
summary(fit.cnt3)
exp(0.037246) # 1.037948

## Error Rate, Sensitivity, Specificity
table(re_eval_sub_cnt3$Direction) ## NO : 195 , YES : 217
err.cnt3 <- getCutOff(re_eval_sub_cnt3, fit.cnt3) # cutoff : 0.5249396
xtabs(~re_eval_sub_cnt3$Direction+(fit.cnt3$fitted.values> err.cnt3))
# re_eval_sub_cnt3$Direction FALSE TRUE
#                       NO     147   48
#                       YES    108  109
## Sensitivity = 109 / (108+109) : 0.502
## Specificity = 147 / (147+48) : 0.753
## Error Rate = (48+108) / (195+217) : 0.379

## full subject count2
fit.cnt2 <- glm(Direction~score2, data = re_eval_sub_cnt2, family = binomial)
summary(fit.cnt2)
exp(0.033171) # 1.033727

## Error Rate, Sensitivity, Specificity
table(re_eval_sub_cnt2$Direction) ## NO : 89 , YES : 108
err.cnt2 <- getCutOff(re_eval_sub_cnt2, fit.cnt2) # cutoff : 0.5935224 0.5798496
xtabs(~re_eval_sub_cnt2$Direction+(fit.cnt2$fitted.values> err.cnt2))
# re_eval_sub_cnt3$Direction FALSE TRUE
#                       NO      68   21
#                       YES     43   65
## Sensitivity = 65 / (43+65) : 0.602
## Specificity = 68 / (68+21) : 0.764
## Error Rate = (21+43) / (89+108) : 0.325
