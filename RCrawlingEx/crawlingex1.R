# XML package : 웹페이지의 테이블 형식 자료 받기 위한 package
install.packages('XML')
library(XML)
install.packages('ggplot2')
library(ggplot2)
install.packages('reshape')
library(reshape2)

# 1.데이터 다운로드
df <- readHTMLTable("http://lol.inven.co.kr/dataninfo/match/playerList.php", header=T)
str(df)

# 2. data frame 형태로 변환
df <- df[[2]]
str(df)

View(df)

# 3. 데이터 전처리 -  필요없는 변수 삭제 및 이름 변환
df <- df[, -c(1,2,5)]
names(df) <- c("name", "cmp", "result", "k", "d", "a", "kda", "help")

View(df)

# 4. 데이터 전처리 - 변수 유형 변경
# name,k,d,a,kda,help
str(df) 
# name,k,d,a,kda
df$name <- as.character(df$name)
df[, 4:7] <- sapply(df[, 4:7], function(a){as.numeric(as.character(a))})
# help
df$help <- as.numeric(sub("%", "", df$help))/100
head(df)
# 5. 데이터 전처리 - 변수 분리(team명과 선수명 분리)
temp <- as.data.frame(do.call(rbind, strsplit(df$name, ' (?=[^ ]+$)', perl=TRUE)))
temp

df$name <- temp$V2
df$team <- temp$V1

head(df)

# 시각화
# 1. 팀별 kda(막대그래프)
mean.df <- as.data.frame(tapply(df$kda, df$team, mean))
mean.df$team <- rownames(mean.df)
names(mean.df) <- c("kda", "team")
mean.df

ggplot(mean.df, aes(team,kda))+geom_bar(stat="identity")

# 2. 팀별 k,d,a (누적막대그래프)
str(df)

# 팀별 k,d,a를 나타내기 위한 일부 변수 추출
df2 <- df[,c(9,4,5,6)]
df2 <- melt(df2, id.vars="team")
str(df)

bar <- ggplot(df2, aes(x=team, y=value, fill=variable))
bar+geom_bar(stat="identity")

# 3. 묶은 막대그래프 그리기
bar+geom_bar(stat="identity", position="dodge")

# 4. K,D,A를 팀별 백분율 형태로 그래프
bar+geom_bar(stat="identity", position="fill")
