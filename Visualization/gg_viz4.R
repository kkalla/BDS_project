#leaflet 개발자 패키지
install.packages("devtools", dependencies = T)
library(devtools)
install_github('rCharts', 'ramnathv') 
install_github('rMaps', 'ramnathv')
devtools::install_github("chgrl/leafletR")
library(leafletR)
library(rMaps)
library(rCharts)

#1 가평군
m1 <- read.csv("Gapyeong.csv")
m1 <- m1[sample(nrow(m1),100),]
m1$avg_price = (m1$대장가액.원./m1$재산면적)
m1$대장가액.원. <- ifelse(is.na(m1$대장가액.원.), 0, m1$대장가액.원.)
m1 <- data.frame(m1$latitude, m1$longitude, m1$공부지목명, m1$재산면적, m1$대장가액.원., m1$avg_price)
colnames(m1) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
m1$size <- ifelse(is.na(m1$size), 1, m1$size)
m1$avg_price <- ifelse(is.na(m1$avg_price), 0, m1$avg_price)
m1$avg_price <- ifelse(m1$avg_price == "Inf", 1, m1$avg_price)
m1$avg_price <- ifelse(m1$avg_price > 5000000, 1, m1$avg_price)
m1$avg_price = round(m1$avg_price)
m1$assets_name <- as.character(m1$assets_name)

# store data in GeoJSON file (just a subset here) leafletR패키지 필요
q.dat1 <- toGeoJSON(data=m1, dest = tempdir(), name="Gapyeong")

#make style based on avg_price
#공유재산별로 시각화
#평당 가격(0만원~100만원, 10만원씩 분류)
q.style <- styleCat(prop="assets_name", 
                    val= c("ddae", "ddap", "forest","jeon", "levee", "random", "river", "road","unknown"), 
                    style.val= c("navy", "gold", "green", "blue", "red", "black", "white", "pink", "brown"), 
                    leg="assets_name")
#q.style <- styleGrad(prop="avg_price", breaks=seq(0, 1000000, by= 100000), 
#                     style.val=rev(heat.colors(10)), leg="avg_price",
#                     fill.alpha=0.7, rad=8)

#create map
q.map1 <- leaflet(data=q.dat1, dest = tempdir(), title="Gapyeong", 
                  base.map="osm", style=q.style, popup="*")

#view map in browser
q.map1

#2 고양시
#토지가격별 시각화
m2 <- read.csv("Goyang.csv")
m2$대장가액.원. <- ifelse(is.na(m2$대장가액.원.), 0, m2$대장가액.원.)
m2$avg_price = (m2$대장가액.원./m2$재산면적)
m2$avg_price <- ifelse(is.na(m2$avg_price), 0, m2$avg_price)
m2$avg_price = round(m2$avg_price)
m2 <- data.frame(m2$latitude, m2$longitude, m2$공부지목명, m2$재산면적, m2$대장가액.원., m2$avg_price)
colnames(m2) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
m2$size <- ifelse(is.na(m2$size), 1, m2$size)
m2$avg_price <- ifelse(is.na(m2$avg_price), 0, m2$avg_price)
m2$assets_name <- as.character(m2$assets_name)

# store data in GeoJSON file (just a subset here)
q.dat2 <- toGeoJSON(data=m2[1:200,], dest = tempdir(), name="Goyang")

#make style based on avg_price
#평당 가격(0만원~100만원, 10만원씩 분류)
q.style <- styleGrad(prop="avg_price", breaks=seq(0, 1000000, by= 100000), 
                     style.val=rev(heat.colors(10)), leg="avg_price",
                     fill.alpha=1, rad=8)

#create map
q.map2 <- leaflet(data=q.dat2, dest = tempdir(), title="Goyang", 
                  base.map="osm", style=q.style, popup="*")

#view map in browser
q.map2

#3 광주시
m3 <- read.csv("Gwangju.csv")
m3$대장가액.원. <- ifelse(is.na(m3$대장가액.원.), 0, m3$대장가액.원.)
m3$avg_price = (m3$대장가액.원./m3$재산면적)
m3$avg_price <- ifelse(is.na(m3$avg_price), 0, m3$avg_price)
m3$avg_price = round(m3$avg_price)
m3 <- data.frame(m3$latitude, m3$longitude, m3$공부지목명, m3$재산면적, m3$대장가액.원., m3$avg_price)
colnames(m3) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
m3$size <- ifelse(is.na(m3$size), 1, m3$size)
m3$avg_price <- ifelse(is.na(m3$avg_price), 0, m3$avg_price)
m3$assets_name <- as.character(m3$assets_name)

# store data in GeoJSON file (just a subset here)
q.dat3 <- toGeoJSON(data= m3[1:200,], dest = tempdir(), name="Gwangju")
  
#make style based on avg_price
#평당 가격(0만원~100만원, 10만원씩 분류)
q.style <- styleGrad(prop="avg_price", breaks=seq(0, 1000000, by= 100000), 
                     style.val=rev(heat.colors(10)), leg="avg_price",
                     fill.alpha=1, rad=8)

#create map
q.map3 <- leaflet(data=q.dat3, dest = tempdir(), title="Gwangju", 
                  base.map="osm", style=q.style, popup="*")

#view map in browser
q.map3

#4 김포시
m4 <- read.csv("Gimpo.csv")
m4$대장가액.원. <- ifelse(is.na(m4$대장가액.원.), 0, m4$대장가액.원.)
m4$avg_price = (m4$대장가액.원./m4$재산면적)
m4$avg_price <- ifelse(is.na(m4$avg_price), 0, m4$avg_price)
m4$avg_price = round(m4$avg_price)
m4 <- data.frame(m4$latitude, m4$longitude, m4$공부지목명, m4$재산면적, m4$대장가액.원., m4$avg_price)
colnames(m4) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
m4$size <- ifelse(is.na(m4$size), 1, m4$size)
m4$avg_price <- ifelse(is.na(m4$avg_price), 0, m4$avg_price)
m4$assets_name <- as.character(m4$assets_name)

# store data in GeoJSON file (just a subset here)
q.dat4 <- toGeoJSON(data=m4, dest = tempdir(), name="Gimpo")

#make style based on avg_price
#평당 가격(0만원~100만원, 10만원씩 분류)
q.style <- styleGrad(prop="avg_price", breaks=seq(0, 1000000, by= 100000), 
                     style.val=rev(heat.colors(10)), leg="avg_price",
                     fill.alpha=1, rad=8)

#create map
q.map4 <- leaflet(data=q.dat4, dest = tempdir(), title="Gimpo",
                  base.map="osm", style=q.style, popup="*")

#view map in browser
q.map4

#5 남양주시
m5 <- read.csv("Namyangju.csv")
m5$대장가액.원. <- ifelse(is.na(m5$대장가액.원.), 0, m5$대장가액.원.)
m5$avg_price = (m5$대장가액.원./m5$재산면적)
m5$avg_price <- ifelse(is.na(m5$avg_price), 0, m5$avg_price)
m5$avg_price = round(m5$avg_price)
m5 <- data.frame(m5$latitude, m5$longitude, m5$공부지목명, m5$재산면적, m5$대장가액.원., m5$avg_price)
colnames(m5) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
m5$size <- ifelse(is.na(m5$size), 1, m5$size)
m5$avg_price <- ifelse(is.na(m5$avg_price), 0, m5$avg_price)
m5$assets_name <- as.character(m5$assets_name)

# store data in GeoJSON file (just a subset here)
q.dat5 <- toGeoJSON(data=m5[1:200,], dest = tempdir(), name="Namyangju")

#make style based on avg_price
#평당 가격(0만원~100만원, 10만원씩 분류)
q.style <- styleGrad(prop="avg_price", breaks=seq(0, 1000000, by= 100000), 
                     style.val=rev(heat.colors(10)), leg="avg_price",
                     fill.alpha=1, rad=8)
#create map
q.map5 <- leaflet(data=q.dat5, dest = tempdir(), title="Namyangju", 
                  base.map="osm", style=q.style, popup="*")

#view map in browser
q.map5

#클러스터 예시
m3 <- read.csv("clusterd.csv")
m3 <- data.frame(m3)

#store data in GeoJSON file (just a subset here)
q.dat <- toGeoJSON(data=m3, dest= tempdir(), name="cluster")

#make style based on clustercut
q.style <- styleCat(prop="clustercut", val= c("1", "2", "3", "4"), 
                    style.val= c("red", "blue", "green", "yellow"), leg="clustercut")

#create map
q.map <- leaflet(data=q.dat, dest= tempdir(), title="cluster", 
                 base.map="osm", style=q.style, popup="*")

#view map in browser
q.map