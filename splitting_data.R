data <- read.csv('Data/공유재산현황.csv',stringsAsFactors=FALSE)
str(data)

## 시별로 set 나누기
sigungu <- unique(strtrim(data$시군구명,13))
?strtrim
sigungu2 <- sigungu[strtrim(sigungu,6)=='경기도']
data_splited <- list(data.frame())
for (i in 1:length(sigungu2)){
    data_splited[[i]] <- data[strtrim(data$시군구명,13) %in% sigungu2[i],]
}

## Save data frame as csv
for(i in 1:length(sigungu2)){
    write.csv(data_splited[[i]],
              file=paste("Data/ggpa2/",
                         sigungu2[i],".csv",sep=""))
}