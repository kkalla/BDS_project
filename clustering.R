source('get_longlat.R')
source('using_kakaoapi.R')
source('make_data_for_clustering.R',encoding = "UTF-8")
data <- read.csv('Data/ggpa2/경기도 구리시.csv',stringsAsFactor = FALSE)
sub <- make_subset(data)
lonlat <- get_geocode(sub$address)
sub <- cbind(sub,lonlat)
total <- sum_by_category(sub)

ana_data1 <- cbind(sub,total)
ana_data2 <- cbind(sub,total)


ana_data2$재산면적 <- as.factor(size_factor)
str(ana_data2)

## clustering
library(cluster)
d_matrix <- daisy(ana_data2[,-c(1,2,4,5)],metric="gower")
h <- hclust(d_matrix)
plot(h)

