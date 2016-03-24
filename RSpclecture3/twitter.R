rm(list=ls())
install.packages("ROAuth")
install.packages("twitteR")
install.packages("tm")
install.packages("Unicode")
install.packages("RCurl")
install.packages("KoNLP")
install.packages("wordcloud")
install.packages("plyr")
install.packages("httr")

library(ROAuth)
library(RCurl)
library(KoNLP)
library(wordcloud)
library(plyr)
library(twitteR)
library(tm)
library(Unicode)
library(httr)

consumerKey <- "Rbu80S62tnZKVPuGJE3CKkP8A"
consumerSecret <- "CB62bN1KanhXSuOP1ruAtSFIVKn5owg7XJc763RFmbMAX1obwe"
request <- "https://api.twitter.com/oauth/request_token"
access <- "https://api.twitter.com/oauth/access_token"
authorize <- "https://api.twitter.com/oauth/authorize"
access_token <- "183506593-souNoesasolvZUDYIJ8QkI9gjt0Va8ISKzT9tw1Y"
access_tokensec <- "4pkb709ybWtVaMX5qPGmpDeIddjCv3GpJtUbuzJFHkPP5"

## twitter 권한 설정
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_tokensec)

keyword <- enc2utf8("등산화")
# 1. keyword에 대한 검색결과 받기
result <- searchTwitter(keyword, n=2000, lang="ko")

result

# 2.결과중 텍스트에 해당하는 부분만 뽑기 
?twListToDF
## 2.1 dataframe으로 변경  
result.df <- twListToDF(result)
result.df

## 2.2 변경한 dataframe에서 text만 뽑아
result.text <- result.df$text
result.text

# 3.불필요한 문자 제거
result.text <- gsub('\n', '', result.text)
result.text <- gsub('\r', '', result.text)
result.text <- gsub('RT', '', result.text)
result.text <- gsub('http', '', result.text)

# 4.문자 분리
?Map
## 명사 형태의 문자만 분리 
result_nouns <- Map(extractNoun, result.text)

# 5.쓸모없는 문자 제거 
?stopwords
## 5.1 list로 되어있는 nouns를 list해제 
result_wordsvec <- unlist(result_nouns,use.name=F)
## 5.2 english인 문자를 제외
result_wordsvec <- result_wordsvec[-which(result_wordsvec %in% stopwords("english"))]
## 5.3 punctuation (구두점) 제거 
result_wordsvec <- gsub('[[:punct:]]','', result_wordsvec)
## 5.4 두글자 이상인 것만  뽑기
result_wordsvec <- Filter(function(x) { nchar(x) >= 2}, result_wordsvec)

# 6. 문자 카운팅
result_wordcount <- table(result_wordsvec)

# 7. 색깔 세팅 
pal <- brewer.pal(12,'Paired')

# 8. font 세팅
windowsFonts(malgun=windowsFont('맑은 고딕'))

result_wordcount
# 9. wordcloud
wordcloud(names(result_wordcount), freq=result_wordcount, scale=c(5,0,10), min.freq=5, random.order=F, rot.per=.1, colors=pal, family='malgun')

vignette(package="twitteR")

example("vignette")
?vignette
