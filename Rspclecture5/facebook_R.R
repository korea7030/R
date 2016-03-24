install.packages("Rfacebook")
install.packages("RCurl")
install.packages("Rook")
install.packages("dplyr")

library(Rfacebook)
# library(devtools)
library(RCurl)
library(Rook)
library(dplyr)

# fbOAuth(app_id="1663246310561944", app_secret="3471fdded9368f291df5ccb99eef582d",extended_permissions = TRUE)
token <- "CAACEdEose0cBAGpw7dKvBIwSdjlzYEnqi24ldkMany7IQ4F4Qt3XjS5TUOhAIwg1hNCWY53RcMtYEzfyA2JSINyNz5QUpIsHbccaTvBzQt4rWm7zTwYStLn8odEFb8RAmidyxQKZAGdv4jy2GJjYZBjkrSovYXl7wl502cSBpq26gFZBZBRXdkIAmT6p2grPliquJowPCbNz9Wk2HPX4MAZAyJsZBzUqYZD" 

# 내 정보
me <- getUsers("me", token, private_info = TRUE)
str(me)
# getFriends(token, simplify = TRUE)

# 자신의 좋아요 목록 가져오기
likes <- getLikes(token = token, "me")
head(likes)

# 내 거의 newsfeed 가져오기
feed <- getNewsfeed(token=token, n=200)
head(tbl_df(feed))
# my_friends <- getFriends(token, simplify = FALSE)
# getL
# length(my_friends)


# getFriends(token = token, simplify = FALSE)

# 검색페이지 가져오기
searchPage <- searchPages(string = "아웃도어", token, n = 5)
head(tbl_df(searchPage))
head(searchPage)


## 위의 검색페이지 중 특정 페이지의 게시글 확인
page <- getPage(page = "eiderfriends", token , n = 100)

str(page)
nrow(page)
count_page <- page[order(page$likes_count,decreasing = T), ]

# 특정 페이지의 게시글 중에 하나의 포스트의 정보
post <- getPost("218498028179742_1097971946899008", token, n = 500, comments = TRUE, likes = TRUE, n.likes = 100, n.comments = 100)

# searchFacebook("iPhone", token, n = 200, since = NULL, until = NULL)

