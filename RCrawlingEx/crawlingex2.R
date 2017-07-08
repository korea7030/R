# 보배드림 크롤링
install.packages('rvest')
library(rvest)
install.packages('stringr')
library(stringr)
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)

bobae_list <- list()

splitFunction <- function(row) {
  return (str_split(row, 'ㅣ')[[1]])
}


for (i in 1:5) {
  print(i)
  url <- paste0("http://www.bobaedream.co.kr/cyber/CyberCar.php?gubun=K&page=",i)
  usedCar <- read_html(url)
  # print(guess_encoding(usedCar))
  
  carInfos <- html_nodes(usedCar, css='.carinfo')
  print(head(carInfos))
  
  # 글자만 뽑기(1개 샘플)
  # carInfos[1] %>% html_nodes('.title') %>% html_text()
  
  # 페이지 전체에서 title만 뽑기
  titles <- carInfos %>% html_nodes('.title') %>% html_text()
  print(head(titles))
  
  # 변속기, 연료정보 뽑기
  carDetailInfo <- carInfos %>% html_nodes('.sub_01') %>% html_text()
  print(head(carDetailInfo))
  
  # 변속기, 연료정보 분리
  # str_split(carInfos[1] %>% html_nodes('.sub_01') %>% html_text(), 'ㅣ')
  
  carDetailInfo <- lapply(carInfos %>% html_nodes('.sub_01') %>% html_text(), splitFunction)
  print(head(carDetailInfo))
  
  # 데이터 합치기
  print(str(carDetailInfo))
  # list 형태이기 때문에 matrix로 변환 후 합치기
  carDetailInfo <- matrix(unlist(carDetailInfo), ncol=3, byrow=T)
  # head(carDetailInfo)
  
  transmission <- carDetailInfo[,1]
  fuel <- carDetailInfo[,2]
  distance <- as.numeric(str_replace_all(carDetailInfo[,3], '[,a-z]', '')) # 숫자형태로 변환 위함
  
  # 가격, 조회수 가져오기
  price <- as.numeric(str_replace(html_nodes(usedCar, css='.price') %>% html_node('em') %>% html_text(), ',', ''))
  view_num <- as.numeric(str_extract(str_replace(html_nodes(usedCar, css='.check') %>% html_text(), ',', ''), '[0-9]+'))
  
  data <- data.frame(titles, transmission, fuel, distance, price, view_num)  
  bobae_list[[i]] <- data
}

# 시각화
bobae_data <- do.call(rbind, bobae_list)
str(bobae_data)
bobae_data[, 1:3] <- sapply(bobae_data[, 1:3], function(a){as.character(a)})

# 연료별 중고차 현황
ggplot(bobae_data, aes(fuel))+geom_bar()

# 브랜드만 뽑기(실행 시 split 결과 vector 크기가 다르기 때문에 warning 메시지가 뜨지만 문제 없음)
temp <- as.data.frame(do.call(rbind, str_split(bobae_data$titles, " ")))
temp$V1 <- sapply(temp$V1, function(a){as.character(a)})

bobae_data$brand <- temp$V1

bobae_data$titles <- str_replace(bobae_data$titles, paste(temp$V1, ' ',sep="") , '')
bobae_data$titles

# 브랜드 별 중고차 현황
ggplot(bobae_data, aes(brand))+geom_bar()

# 가격대별 현황
# 계약된 데이터 제거
bobae_data[is.na(bobae_data$price),]
bobae_data <- na.omit(bobae_data)

min(bobae_data$price)
max(bobae_data$price)

bobae_data <- within(bobae_data, {
  price_range = character(0)
  price_range[price >= 100 & price < 1000] = "1천미만"
  price_range[price >= 1000 & price < 2000] = "1천이상~2천미만"
  price_range[price >= 2000 & price < 3000] = "2천이상~3천미만"
  price_range[price >= 3000 & price < 4000] = "3천이상~4천미만"
  price_range[price >= 4000 & price < 5000] = "4천이상~5천미만"
  price_range[price >= 5000 & price < 6000] = "5천이상~6천미만"
  price_range[price >= 6000] = "6천이상"
})

price_range_data <- bobae_data %>% group_by(price_range) %>% summarise(count=n()) %>% mutate(pct = count/sum(count))


# 가격별 현황
ggplot(price_range_data, aes(x=price_range, y=pct, fill=price_range))+
  geom_bar(width=1, stat="identity")+
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x='가격 분류', 
       y='비율(%)', 
       title="가격대별 현황")+
  geom_text(data=price_range_data, aes(x= price_range, y=pct, label=paste0(round(pct,3)*100,"%")), size=6)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


