install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")
library(ROAuth)
library(twitteR)
library(RCurl)

##각자 트위터 API 컨슈머키를 활용하여, 인증에 필요한 내용을 변수에 저장합니다.
credential <- OAuthFactory$new(consumerKey="컨슈머 키 입력",
                               consumerSecret="컨슈머 시크릿 입력",
                               requestURL="https://api.twitter.com/oauth/request_token",
                               accessURL="https://api.twitter.com/oauth/access_token",
                               authURL="https://api.twitter.com/oauth/authorize")


credential

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
credential$handshake(cainfo="cacert.pem")

setup_twitter_oauth(credential$consumerKey,credential$consumerSecret,
                    credential$oauthKey,credential$oauthSecret)

##일단 handshake가 되고, OAuth가 성립되면, 이후에는 트위터에서 허용한
##횟수이내에서 여러번 트위터 키워드 검색 등이 가능합니다.

##iconv함수는 한글의 엔코딩을 변경시켜주는 함수로, 윈도우 사용자들에게만
##해당되는 내용입니다.

##iconv함수의 첫번째 인자에 트위터에서 검색하고자 하는 키워드를 영문이나,
##한글로 넣으시면 트위터에서 검색이 됩니다.
##시사성이 떨어지는 키워드의 경우에는 관련 트윗이 없어서, 검색결과가 
##하나도 없을 수 있습니다.

searchWord <- iconv("#climate","CP949","UTF-8")

searchWord2 <- iconv("#박지성","CP949","UTF-8")

searchWord

searchWord2

##일반적인 api 등급으로는 최대한도 n=1500입니다.

tweetList <- searchTwitter(searchWord, n=500)

tweetList2 <- searchTwitter(searchWord2, n=1000)

head(tweetList) #상위 6개 항목만 확인

head(tweetList2) #상위 6개 항목만 확인

tail(tweetList) #하위 6개 항목만 확인

tail(tweetList2) #하위 6개 항목만 확인

length(tweetList)

str(head(tweetList,1))

tweetList[[1]] #숫자를 활용하여 리스트 데이터 특정 원소를 찾아가는 방법입니다.

tweetDf <- do.call("rbind", lapply(tweetList, as.data.frame))

dim(tweetDf)

names(tweetDf)

str(tweetDf)

##리스트 데이터를 데이터프레임으로 합칠 필요가 있는 경우,
##do.call함수를 사용하지 않으면 에러가 납니다. 그 차이를 비교해 보실수 있습니다.
tweetDfErr <- rbind(lapply(tweetList, as.data.frame))

