## 실행전에 working directory 체크해야함
## setwd('Data/sigungu') 

## read data
library(xlsx)
start <- Sys.time()
data <- read.xlsx('노인장기요양보험_등급판정현황_2011.xls',sheetIndex=1,
                  header=FALSE, startRow = 6,colIndex=c(1:9),encoding = 'UTF-8')
loading_time <- Sys.time() - start
str(data)
loading_time
d1 <- data.frame()
d2 <- data.frame()
d3 <- data.frame()
grade_datas <- list(d1,d2,d3)
years <- c('2014','2015')
start <- Sys.time()
for(i in c(1:2)){
    grade_datas[i] <- read.xlsx(
        paste('노인장기요양보험_등급판정현황_자격별_',years[i],'.xls',sep=""),
        sheetIndex=1, header=FALSE, startRow = 6, 
        colIndex=c(1:11),encoding='UTF-8')
}
loading_time <- Sys.time() - start
str(grade_datas[2])
