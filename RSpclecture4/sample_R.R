library(dplyr)
r0801 <- read.csv("r0801_sample.csv", na.strings = "NA" , stringsAsFactors = F, header=T)

# example(read.csv)
# ?read.csv

r0801_df <- tbl_df(r0801)

# select(r0801_df, reg1)

# code ~ jjob1 컬럼 지우기
r0801_data <- select(r0801_df, -(code:jjob1))


# r0801_data[which(is.na(r0801_data)),] <- 0
# NA : 9999999
select(r0801_data , D1_2F1)

dim(r0801_data)
r0801_data$폐업체
# 폐업 안함(1) 이면서 설문을 받음(1) 의 상위 5개 업체의 D1_2F1
r0801_data_survey <- filter(r0801_data, r0801_data$폐업체==1, survey==1)

dim(r0801_data_survey)
r0801_data_survey[r0801_data_survey$D1_2F1 == 9999999,] <- 0

r0801_data_survey_order <- r0801_data_survey[order(r0801_data_survey$D1_2F1, decreasing=T),]

survey_select <- select(r0801_data_survey_order, ID, LISTID, A1_1, D1_2F1)
############################################################################

# r0801_data_survey[order(r0801_data_survey$D1_2F1, decreasing=T),]

# 폐업 안함(1) 이면서 설문을 받음(1) 의 상위 5개 업체의 D1_2F1
r0801_data_survey_close <- filter(r0801_data, r0801_data$폐업체 == 2, survey == 1)


r0801_data_survey_close[r0801_data_survey_close$D1_2F1 == 9999999,] <- 0

r0801_data_survey_close_order <- r0801_data_survey_close[order(r0801_data_survey_close$D1_2F1, decreasing=T),]
?order
survey_select_close <- select(r0801_data_survey_close_order, ID, LISTID, A1_1, D1_2F1)

#################################################################################

#########################################################################
# 데이터프레임 추출하는건 찾다 못찾아서 일단 데이터가 적으니 수를 확인하고 상위 5개만 뽑음

# 1~5등 뽑기(폐업안한  data frame)
survey_select <- survey_select[survey_select$D1_2F1 > 5653708, ]

survey_select

# 구분자 추가
survey_select <- mutate(survey_select, gubun = "1")

# 숫자 6e+05 와 같이 출력되는걸 방지 하기 위한 옵션
options(scipen=999)


# 1~5 등 뽑기(폐업한  data frame)
survey_select_close <- survey_select_close[survey_select_close$D1_2F1 > 440000, ]

# 구분자
survey_select_close <- mutate(survey_select_close, gubun="2")
             

mutate(survey_select_close2, gubun="2")

# 설문조사를 응한 회사 중 폐업안한 회사와 폐업한 회사 합침
total <- rbind(survey_select, survey_select_close)

#  output 
write.csv(total, file = "survey_merge_data.csv")

