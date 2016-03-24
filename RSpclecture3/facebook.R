install.packages("Rfacebook")
install.packages("Rook")
library(Rfacebook)
library(Rook)
library(RCurl)
library(tm)
fb_oauth <- fbOAuth(app_id="1663246310561944", app_secret="3471fdded9368f291df5ccb99eef582d",extended_permissions = TRUE)

options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE)) 

FQLQuery.facebook <- function(script="",access_token, path) {
  
  fromJSON(getURL(sprintf( "https://graph.facebook.com/%s?%s&access_token=%s", path, script, access_token)))
  # print(sprintf( "https://graph.facebook.com/%s?%s&access_token=%s", path, script, access_token))
}

keyword <- "iPhone"
path <- enc2utf8(keyword)

access_token = "CAACEdEose0cBAB1BWWZBkrfVQkH6JurRGsS8LF0z3uocfKZCX3eFOZABHZA3LXaamrhHes6GqjXViXZB3oJ5tkoi7SZCSDdpbFeiGXM2PU85KyDrcJn1lJePtPPtKQsOG0ZCJZCac5pbzY1jrGKEOyoCpG9xKN9ZCYrZAkzVvM0ZCTTbZBiHrLPp6VWMy0TFZAiQ6LQjfJxasqmlRZBgqm0EQH6BZB7dTh4ifRRcRsZD"
myposts <- FQLQuery.facebook("fields=posts.fields(message,likes)&limit=100&offset=0", access_token, path)

head(myposts)

?strsplit

post.id  <- sapply(myposts$posts$data, function(x) x$id)
post.messages <- sapply(myposts$posts$data, function(x) x$message)
# post.likes <- sapply(myposts$posts$data, function(x) x$likes$count)
# post.createdtime <- sapply(myposts$post$data, function(x) x$created_time)

# 문자열 정리

post.messages <- gsub("\n","",post.messages)
post.messages <- gsub("\r","",post.messages)
post.messages <- removePunctuation(post.messages)
result <- data.frame(post.id = post.id,message = post.messages)

head(result)

library(KoNLP)
library(wordcloud)

wordcloud2 <- function (data, min.count, pal) {

  # text mining을 위한 형태로 정제
  data.dt <- data
  data.dt <- gsub("\n","",data.dt)
  data.dt <- gsub("\r","",data.dt)
  data.dt <- gsub("RT","",data.dt)

  # 명사 분리
  result_nouns <- Map(extractNoun,data.dt)
  result_word <- unlist(result_nouns,use.name=F)

  # 글자수가 2 이상인 것만 남김
  result_word <- Filter(function(x) { nchar(x) >=2 }, result_word)
  
  # 글자수가 과도하게 긴 것은 제외
  result_word <- Filter(function(x) { nchar(x) <=10 }, result_word)

  # 문자 Counting
  result_word.count <- table(result_word)
  # 일정 Count 이상 되는 것으로 제한
  result_word.count <- result_word.count[result_word.count >= min.count]
  # 폰트 세팅
  windowsFonts(malgun=windowsFont("맑은 고딕"))  
  # Wordcloud
  wordcloud(names(result_word.count), 
            freq=result_word.count, 
            scale=c(5,.5), min.freq=5, random.order=F, family="malgun", colors=pal)
  
}


pal <- brewer.pal(12, "Paired")
wordcloud2(result[,'message'], 2,pal)
