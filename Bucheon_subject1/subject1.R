library(XLConnect)
# install.packages("igraph")
library(igraph)
# install.packages("sna")
# library(sna)
# install.packages("extrafont")
library(extrafont)
remove.packages("sna", "C:\\Program Files\\R\\R-3.2.1\\library")

except <- c("건축공학과","실내건축학과","토목공학과","디지털콘텐츠학과","컴퓨터소프트웨어공학과","전자공학과","정보통신공학과","컴퓨터제어공학과","e-비즈니스학과","섬유의류비즈니스학과","산업디자인학과","패션디자인학과","생활스포츠학과","식품영양학과","경영학과","관광경영학과","부동산금융정보학과","비서학과","세무회계학과","행정학과","유아교육과","사회복지학과")
engineer <- c("건축과","실내건축디자인과","토목과","컴퓨터정보보안과","컴퓨터소프트웨어과","전자과","정보통신과","지능로봇과","e-비즈니스과","섬유패션비즈니스과","영상&게임콘텐츠과", "실내건축과", "디지털콘텐츠과", "모바일통신과", "컴퓨터제어과", "섬유의류비즈니스과")
social <- c("경영과","호텔관광경영과","부동산유통학과","비서사무행정과","세무회계과","유아교육과","사회복지과","영유아보육과","항공서비스과", "관광경영과", "부동산금융정보과", "비서과", "행정과")
art_psy <- c("광고디자인전공","산업디자인과","재활스포츠학과", "패션디자인과", "만화&2D영상그래픽전공", "3D영상그래픽전공", "쥬얼리디자인전공", "실용사진전공", "생활스포츠과")
science <- c("식품영양과","호텔외식조리학과","간호학과", "호텔외식조리과", "간호과")
jobY <- c("건보", "진학")
jobN <- c("입대", "기타")

# 모든데이터(전체 데이터를 뽑을 때 사용되는 함수)
getAllData_Matrix <- function(mainData) {
  mat <- as.matrix(subset(mainData, select = c(5:length(mainData))))
  res <- t(mat) %*% mat
  diag(res) <- 0
  return (res)
}

# 계열별(계열을 뽑을 때 사용)
getSeriesData_Matrix <- function(mainData, Series) {
  mat <- as.matrix(subset(mainData, select = c(5:length(mainData)), mainData[,1] %in% Series))
  res <- t(mat) %*% mat
  diag(res) <- 0
  return (res)
}

# 계열별&취업여부(계열별 취업 여부를 뽑을 때 사용)
getSeriesJobYnData_Matrix <- function(mainData, Series, Job) {
  mat <- as.matrix(subset(mainData, select = c(5:length(mainData)), mainData[,4] %in% Job & mainData[,1] %in% Series))
  res <- t(mat) %*% mat
  diag(res) <- 0
  return (res)
}

# 취업여부(계열 상관없이 취업여부를 뽑을 때 사용)
getJobYnData_Matrix <- function(mainData,  Job) {
  mat <- as.matrix(subset(mainData, select = c(5:length(mainData)), mainData[,4] %in% Job))
  res <- t(mat) %*% mat
  diag(res) <- 0
  return (res)
}

# graph 출력
printSNAPlot <- function(mat_dat,title) {
  jpeg(paste0(title,".jpeg"), width = 800, height = 600) # jpeg 저장위한 코드
  # pdf.options(family = "Korea1deb")
  g<- graph.data.frame(mat_dat, directed = FALSE) # 방향성 확인하시려면 directed 를 TRUE로 하시면 됩니다.
  g<- simplify(g) # 자기자신을 가르키는 것 제외
  
  V(g)$name <- colnames(mat_dat) # node의 이름 지정
  V(g)$label <- V(g)$name # node의 label 지정
  # print(V(g)$label)
  btw <- betweenness(g) # SNA package(근접중심성)
  btw.score <- round(btw)+1 # 근접중심성을 가지고 점수화
  # length(btw.score)
  btw.colors <- rev(heat.colors(max(btw.score))) # 색깔 지정
  V(g)$color <- btw.colors[btw.score] # 색깔 넣기
  
  # V(g)$size <- degree(g) # SNA package 차수
  V(g)$size <- btw # SNA package 차수 # node의 크기를 중개중심성 점수로 대입
  # print(V(g)$size)
  V(g)$label.cex <- degree(g) / max(degree(g)) + 0.3 # label 크기 지정
  set.seed(1234) # 일정한 graph를 그리기 위한 seed값 지정
  
  E(g)$label <- graph.strength(g) # edge(선) 의 라벨을 지정
  E(g)$label.cex <- degree(g) / max(degree(g)) + 0.2 # edge(선)의 라벨 크기를 지정
  layout = layout.fruchterman.reingold(g) # layout 지정
  plot(g, edge.width = degree(g), layout=layout, main = title) # graph 출력
  # plot(g, edge.width = degree(g), layout=layout, main = title)
  print (btw) # 중개중심성 출력
  dev.off()
}

## All Data ###########################################################################################
all <- loadWorkbook("all.xls")
all_data <- readWorksheet(all,1, check.names = FALSE)
head(all_data)

# 컬럼명
all_col <- colnames(all_data)

all_data <- all_data[,-c(59)]
all_data <- all_data[-which(is.na(all_data$이름)), ]
all_data <- subset(all_data , !(학과 %in% except))

# All
all_mat <- getAllData_Matrix(all_data)
printSNAPlot(all_mat, "전체")
# gplot(all_mat, displaylabels = T, boxed.labels = F, vertex.cex = 1, vertex.col = "red", vertex.sides = 20, edge.lwd = 1, edge.col = "grey", label.pos = 3)

# Series 별
all_engineer_mat <- getSeriesData_Matrix(all_data, engineer)
printSNAPlot(all_engineer_mat, "전체-공학계열")
all_social_mat <- getSeriesData_Matrix(all_data, social)
printSNAPlot(all_social_mat, "전체-인문사회계열")
all_artpsy_mat <- getSeriesData_Matrix(all_data, art_psy)
printSNAPlot(all_artpsy_mat, "전체-예체능계열")
all_science_mat <- getSeriesData_Matrix(all_data, science)
printSNAPlot(all_science_mat, "전체-자연과학계열")

# Job 별
all_joby_mat <- getJobYnData_Matrix(all_data, jobY)
printSNAPlot(all_joby_mat, "전체-취업")
all_jobn_mat <- getJobYnData_Matrix(all_data, jobN)
printSNAPlot(all_jobn_mat, "전체-미취업")

# Series 별 Job 별
all_engineer_jobY_mat <- getSeriesJobYnData_Matrix(all_data, engineer, jobY)
printSNAPlot(all_engineer_jobY_mat, "전체-취업_공학계열")
all_social_jobY_mat <- getSeriesJobYnData_Matrix(all_data, social, jobY)
printSNAPlot(all_social_jobY_mat, "전체-취업_인문사회계열")
all_artpsy_jobY_mat <- getSeriesJobYnData_Matrix(all_data, art_psy, jobY)
printSNAPlot(all_artpsy_jobY_mat, "전체-취업_예체능계열")
all_science_jobY_mat <- getSeriesJobYnData_Matrix(all_data, science, jobY)
printSNAPlot(all_science_jobY_mat, "전체-취업_자연과학계열")

all_engineer_jobN_mat <- getSeriesJobYnData_Matrix(all_data, engineer, jobN)
printSNAPlot(all_engineer_jobN_mat, "전체-미취업_공학계열")
all_social_jobN_mat <- getSeriesJobYnData_Matrix(all_data, social, jobN)
printSNAPlot(all_social_jobN_mat, "전체-미취업_인문사회계열")
all_artpsy_jobN_mat <- getSeriesJobYnData_Matrix(all_data, art_psy, jobN)
printSNAPlot(all_artpsy_jobN_mat, "전체-미취업_예체능계열")
all_science_jobN_mat <- getSeriesJobYnData_Matrix(all_data, science, jobN)
printSNAPlot(all_science_jobN_mat, "전체-미취업_자연과학계열")
###########################################################################################
## All Modi Data ###########################################################################################
all_modi <- loadWorkbook("all_modi.xls")
all_modi_data <- readWorksheet(all_modi,1, check.names = FALSE)
head(all_modi_data)

# 컬럼명
all_modi_col <- colnames(all_modi_data)

all_modi_data <- all_modi_data[,-c(52)]
all_modi_data <- all_modi_data[-which(is.na(all_modi_data$이름)), ]
all_modi_data <- subset(all_modi_data , !(학과 %in% except))

all_modi_mat <- getAllData_Matrix(all_modi_data)
write.csv(file="수정.csv", all_modi_mat)
printSNAPlot(all_modi_mat, "전체(수정)")

# Series 별
all_modi_engineer_mat <- getSeriesData_Matrix(all_modi_data, engineer)
printSNAPlot(all_modi_engineer_mat, "전체(수정)-공학계열")
all_modi_social_mat <- getSeriesData_Matrix(all_modi_data, social)
printSNAPlot(all_modi_social_mat, "전체(수정)-인문사회계열")
all_modi_artpsy_mat <- getSeriesData_Matrix(all_modi_data, art_psy)
printSNAPlot(all_modi_artpsy_mat, "전체(수정)-예체능계열")
all_modi_science_mat <- getSeriesData_Matrix(all_modi_data, science)
printSNAPlot(all_modi_science_mat, "전체(수정)-자연과학계열")

# Job 별
all_modi_joby_mat <- getJobYnData_Matrix(all_modi_data, jobY)
printSNAPlot(all_modi_joby_mat, "전체(수정)-취업")
all_modi_jobn_mat <- getJobYnData_Matrix(all_modi_data, jobN)
printSNAPlot(all_modi_jobn_mat, "전체(수정)-미취업")

# Series 별 Job 별
all_modi_engineer_jobY_mat <- getSeriesJobYnData_Matrix(all_modi_data, engineer, jobY)
printSNAPlot(all_modi_engineer_jobY_mat, "전체(수정)-취업_공학계열")
all_modi_social_jobY_mat <- getSeriesJobYnData_Matrix(all_modi_data, social, jobY)
printSNAPlot(all_modi_social_jobY_mat, "전체(수정)-취업_인문사회계열")
all_modi_artpsy_jobY_mat <- getSeriesJobYnData_Matrix(all_modi_data, art_psy, jobY)
printSNAPlot(all_modi_artpsy_jobY_mat, "전체(수정)-취업_예체능계열")
all_modi_science_jobY_mat <- getSeriesJobYnData_Matrix(all_modi_data, science, jobY)
printSNAPlot(all_modi_science_jobY_mat, "전체(수정)-취업_자연과학계열")

all_modi_engineer_jobN_mat <- getSeriesJobYnData_Matrix(all_modi_data, engineer, jobN)
printSNAPlot(all_modi_engineer_jobN_mat, "전체(수정)-미취업_공학계열")
all_modi_social_jobN_mat <- getSeriesJobYnData_Matrix(all_modi_data, social, jobN)
printSNAPlot(all_modi_social_jobN_mat, "전체(수정)-미취업_인문사회계열")
all_modi_artpsy_jobN_mat <- getSeriesJobYnData_Matrix(all_modi_data, art_psy, jobN)
printSNAPlot(all_modi_artpsy_jobN_mat, "전체(수정)-미취업_예체능계열")
all_modi_science_jobN_mat <- getSeriesJobYnData_Matrix(all_modi_data, science, jobN)
printSNAPlot(all_modi_science_jobN_mat, "전체(수정)-미취업_자연과학계열")
###########################################################################################
## Base Data ###########################################################################################
base <- loadWorkbook("base.xls")
base_data <- readWorksheet(base,1, check.names = FALSE)
head(base_data)

# 컬럼명
base_col <- colnames(base_data)

# base_data <- base_data[,-c(52)]
base_data <- base_data[-which(is.na(base_data$이름)), ]
base_data <- subset(base_data , !(학과 %in% except))

base_mat <- getAllData_Matrix(base_data)
head(base_mat)

printSNAPlot(base_mat, "기초학습")

# Series 별
base_engineer_mat <- getSeriesData_Matrix(base_data, engineer)
head(base_engineer_mat)
printSNAPlot(base_engineer_mat, "기초학습-공학계열")
base_social_mat <- getSeriesData_Matrix(base_data, social)
printSNAPlot(base_social_mat, "기초학습-인문사회계열")
base_artpsy_mat <- getSeriesData_Matrix(base_data, art_psy)
printSNAPlot(base_artpsy_mat, "기초학습-예체능계열")
base_science_mat <- getSeriesData_Matrix(base_data, science)
printSNAPlot(base_science_mat, "기초학습-자연과학계열")

# Job 별
base_joby_mat <- getJobYnData_Matrix(base_data, jobY)
printSNAPlot(base_joby_mat, "기초학습-취업")
base_jobn_mat <- getJobYnData_Matrix(base_data, jobN)
printSNAPlot(base_jobn_mat, "기초학습-미취업")

# Series 별 Job 별
base_engineer_jobY_mat <- getSeriesJobYnData_Matrix(base_data, engineer, jobY)
printSNAPlot(base_engineer_jobY_mat, "기초학습-취업_공학계열")
base_social_jobY_mat <- getSeriesJobYnData_Matrix(base_data, social, jobY)
printSNAPlot(base_social_jobY_mat, "기초학습-취업_인문사회계열")
base_artpsy_jobY_mat <- getSeriesJobYnData_Matrix(base_data, art_psy, jobY)
printSNAPlot(base_artpsy_jobY_mat, "기초학습-취업_예체능계열")
base_science_jobY_mat <- getSeriesJobYnData_Matrix(base_data, science, jobY)
printSNAPlot(base_science_jobY_mat, "기초학습-취업_자연과학계열")

base_engineer_jobN_mat <- getSeriesJobYnData_Matrix(base_data, engineer, jobN)
printSNAPlot(base_engineer_jobN_mat, "기초학습-미취업_공학계열")
base_social_jobN_mat <- getSeriesJobYnData_Matrix(base_data, social, jobN)
printSNAPlot(base_social_jobN_mat, "기초학습-미취업_인문사회계열")
base_artpsy_jobN_mat <- getSeriesJobYnData_Matrix(base_data, art_psy, jobN)
printSNAPlot(base_artpsy_jobN_mat, "기초학습-미취업_예체능계열")
base_science_jobN_mat <- getSeriesJobYnData_Matrix(base_data, science, jobN)
printSNAPlot(base_science_jobN_mat, "기초학습-미취업_자연과학계열")
###########################################################################################
## consult Data ###########################################################################################
consult <- loadWorkbook("consult.xls")
consult_data <- readWorksheet(consult,1, check.names = FALSE)
head(consult_data)

# 컬럼명
consult_col <- colnames(consult_data)

# consult_data <- consult_data[,-c(52)]
consult_data <- consult_data[-which(is.na(consult_data$이름)), ]
consult_data <- subset(consult_data , !(학과 %in% except))

consult_mat <- getAllData_Matrix(consult_data)
printSNAPlot(consult_mat, "상담")

# Series 별
consult_engineer_mat <- getSeriesData_Matrix(consult_data, engineer)
printSNAPlot(consult_engineer_mat, "상담-공학계열")
consult_social_mat <- getSeriesData_Matrix(consult_data, social)
printSNAPlot(consult_social_mat, "상담-인문사회계열")
consult_artpsy_mat <- getSeriesData_Matrix(consult_data, art_psy)
printSNAPlot(consult_artpsy_mat, "상담-예체능계열")
consult_science_mat <- getSeriesData_Matrix(consult_data, science)
printSNAPlot(consult_science_mat, "상담-자연과학계열")

# Job 별
consult_joby_mat <- getJobYnData_Matrix(consult_data, jobY)
printSNAPlot(consult_joby_mat, "상담-취업")
consult_jobn_mat <- getJobYnData_Matrix(consult_data, jobN)
printSNAPlot(consult_jobn_mat, "상담-미취업")

# Series 별 Job 별
consult_engineer_jobY_mat <- getSeriesJobYnData_Matrix(consult_data, engineer, jobY)
printSNAPlot(consult_engineer_jobY_mat, "상담-취업_공학계열")
consult_social_jobY_mat <- getSeriesJobYnData_Matrix(consult_data, social, jobY)
printSNAPlot(consult_social_jobY_mat, "상담-취업_인문사회계열")
consult_artpsy_jobY_mat <- getSeriesJobYnData_Matrix(consult_data, art_psy, jobY)
printSNAPlot(consult_artpsy_jobY_mat, "상담-취업_예체능계열")
consult_science_jobY_mat <- getSeriesJobYnData_Matrix(consult_data, science, jobY)
printSNAPlot(consult_science_jobY_mat, "상담-취업_자연과학계열")

consult_engineer_jobN_mat <- getSeriesJobYnData_Matrix(consult_data, engineer, jobN)
printSNAPlot(consult_engineer_jobN_mat, "상담-미취업_공학계열")
consult_social_jobN_mat <- getSeriesJobYnData_Matrix(consult_data, social, jobN)
printSNAPlot(consult_social_jobN_mat, "상담-미취업_인문사회계열")
consult_artpsy_jobN_mat <- getSeriesJobYnData_Matrix(consult_data, art_psy, jobN)
printSNAPlot(consult_artpsy_jobN_mat, "상담-미취업_예체능계열")
consult_science_jobN_mat <- getSeriesJobYnData_Matrix(consult_data, science, jobN)
printSNAPlot(consult_science_jobN_mat, "상담-미취업_자연과학계열")
###########################################################################################
## language Data ###########################################################################################
language <- loadWorkbook("language.xls")
language_data <- readWorksheet(language,1, check.names = FALSE)
head(language_data)

# 컬럼명
language_col <- colnames(language_data)

# language_data <- language_data[,-c(52)]
language_data <- language_data[-which(is.na(language_data$이름)), ]
language_data <- subset(language_data , !(학과 %in% except))

language_mat <- getAllData_Matrix(language_data)
printSNAPlot(language_mat, "어학")

# Series 별
language_engineer_mat <- getSeriesData_Matrix(language_data, engineer)
printSNAPlot(language_engineer_mat, "어학-공학계열")
language_social_mat <- getSeriesData_Matrix(language_data, social)
printSNAPlot(language_social_mat, "어학-인문사회계열")
language_artpsy_mat <- getSeriesData_Matrix(language_data, art_psy)
printSNAPlot(language_artpsy_mat, "어학-예체능계열")
language_science_mat <- getSeriesData_Matrix(language_data, science)
printSNAPlot(language_science_mat, "어학-자연과학계열")

# Job 별
language_joby_mat <- getJobYnData_Matrix(language_data, jobY)
printSNAPlot(language_joby_mat, "어학-취업")
language_jobn_mat <- getJobYnData_Matrix(language_data, jobN)
printSNAPlot(language_jobn_mat, "어학-미취업")

# Series 별 Job 별
language_engineer_jobY_mat <- getSeriesJobYnData_Matrix(language_data, engineer, jobY)
printSNAPlot(language_engineer_jobY_mat, "어학-취업_공학계열")
language_social_jobY_mat <- getSeriesJobYnData_Matrix(language_data, social, jobY)
printSNAPlot(language_social_jobY_mat, "어학-취업_인문사회계열")
language_artpsy_jobY_mat <- getSeriesJobYnData_Matrix(language_data, art_psy, jobY)
printSNAPlot(language_artpsy_jobY_mat, "어학-취업_예체능계열")
language_science_jobY_mat <- getSeriesJobYnData_Matrix(language_data, science, jobY)
printSNAPlot(language_science_jobY_mat, "어학-취업_자연과학계열")

language_engineer_jobN_mat <- getSeriesJobYnData_Matrix(language_data, engineer, jobN)
printSNAPlot(language_engineer_jobN_mat, "어학-미취업_공학계열")
language_social_jobN_mat <- getSeriesJobYnData_Matrix(language_data, social, jobN)
printSNAPlot(language_social_jobN_mat, "어학-미취업_인문사회계열")
language_artpsy_jobN_mat <- getSeriesJobYnData_Matrix(language_data, art_psy, jobN)
printSNAPlot(language_artpsy_jobN_mat, "어학-미취업_예체능계열")
language_science_jobN_mat <- getSeriesJobYnData_Matrix(language_data, science, jobN)
printSNAPlot(language_science_jobN_mat, "어학-미취업_자연과학계열")
###########################################################################################
## jobemp Data ###########################################################################################
jobemp <- loadWorkbook("jobemp.xls")
jobemp_data <- readWorksheet(jobemp,1, check.names = FALSE)
head(jobemp_data)

# 컬럼명
jobemp_col <- colnames(jobemp_data)

# jobemp_data <- jobemp_data[,-c(52)]
jobemp_data <- jobemp_data[-which(is.na(jobemp_data$이름)), ]
jobemp_data <- subset(jobemp_data , !(학과 %in% except))

jobemp_mat <- getAllData_Matrix(jobemp_data)
printSNAPlot(jobemp_mat, "취업지원")

# Series 별
jobemp_engineer_mat <- getSeriesData_Matrix(jobemp_data, engineer)
printSNAPlot(jobemp_engineer_mat, "취업지원-공학계열")

jobemp_social_mat <- getSeriesData_Matrix(jobemp_data, social)
printSNAPlot(jobemp_social_mat, "취업지원-인문사회계열")

jobemp_artpsy_mat <- getSeriesData_Matrix(jobemp_data, art_psy)
printSNAPlot(jobemp_artpsy_mat, "취업지원-예체능계열")

jobemp_science_mat <- getSeriesData_Matrix(jobemp_data, science)
printSNAPlot(jobemp_science_mat, "취업지원-자연과학계열")

# Job 별
jobemp_joby_mat <- getJobYnData_Matrix(jobemp_data, jobY)
printSNAPlot(jobemp_joby_mat, "취업지원-취업")

jobemp_jobn_mat <- getJobYnData_Matrix(jobemp_data, jobN)
printSNAPlot(jobemp_jobn_mat, "취업지원-미취업")

# Series 별 Job 별
jobemp_engineer_jobY_mat <- getSeriesJobYnData_Matrix(jobemp_data, engineer, jobY)
printSNAPlot(jobemp_engineer_jobY_mat, "취업지원-취업_공학계열")

jobemp_social_jobY_mat <- getSeriesJobYnData_Matrix(jobemp_data, social, jobY)
printSNAPlot(jobemp_social_jobY_mat, "취업지원-취업_인문사회계열")

jobemp_artpsy_jobY_mat <- getSeriesJobYnData_Matrix(jobemp_data, art_psy, jobY)
printSNAPlot(jobemp_artpsy_jobY_mat, "취업지원-취업_예체능계열")

jobemp_science_jobY_mat <- getSeriesJobYnData_Matrix(jobemp_data, science, jobY)
printSNAPlot(jobemp_science_jobY_mat, "취업지원-취업_자연과학계열")


jobemp_engineer_jobN_mat <- getSeriesJobYnData_Matrix(jobemp_data, engineer, jobN)
printSNAPlot(jobemp_engineer_jobN_mat, "취업지원-미취업_공학계열")
printSNAPlot2(jobemp_engineer_jobN_mat, "취업지원-미취업_공학계열")
jobemp_social_jobN_mat <- getSeriesJobYnData_Matrix(jobemp_data, social, jobN)
printSNAPlot(jobemp_social_jobN_mat, "취업지원-미취업_인문사회계열")
printSNAPlot2(jobemp_social_jobN_mat, "취업지원-미취업_인문사회계열")
jobemp_artpsy_jobN_mat <- getSeriesJobYnData_Matrix(jobemp_data, art_psy, jobN)
printSNAPlot(jobemp_artpsy_jobN_mat, "취업지원-미취업_예체능계열")
printSNAPlot2(jobemp_artpsy_jobN_mat, "취업지원-미취업_예체능계열")
jobemp_science_jobN_mat <- getSeriesJobYnData_Matrix(jobemp_data, science, jobN)
printSNAPlot(jobemp_science_jobN_mat, "취업지원-미취업_자연과학계열")
printSNAPlot2(jobemp_science_jobN_mat, "취업지원-미취업_자연과학계열")
###########################################################################################