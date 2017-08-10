## Make datasets for clustering

## Read dataset and make subset with 2 columns
data <- read.csv('Data/ggpa2/경기도 고양시.csv',stringsAsFactor=FALSE)
str(data)
sub <- data[!is.na(data$재산면적),c(2,7,8,21)]
head(sub)
sub <- sub[sub$실지목명  != '도로' & sub$공부지목명 != '도로',]
head(sub)
sub2 <- sub[,-c(2,3)]
head(sub2) ; str(sub2)
adr_i <- sub2$address[10]
## get longitude and latitude using naver api
source('get_longlat.R')
lonlat <- get_geocode(sub2$address)
str(lonlat)
