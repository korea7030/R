install.packages("XLConnect")
library(XLConnect)
file_1 <- loadWorkbook("2009.xlsx")
file_2 <- loadWorkbook("2010.xlsx")

file_1.df <- readWorksheet(file_1, 1)
file_2.df <- readWorksheet(file_2, 1)

## data frame 합치기
file_merge <- merge(file_2.df, file_1.df, by= "id", all.x = TRUE)

## Null 인 값에만 이전 파일의 year값 넣기
file_merge[is.na(file_merge$year1),]$year1 <- file_merge[is.na(file_merge$year1), ]$year.x
colnames(file_merge) <- c("id", "year", "status", "year1")

file_merge

write.csv(file = "result.csv", file_merge)
