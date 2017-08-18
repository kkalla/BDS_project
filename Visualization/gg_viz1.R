#현황 시각화를 위한 라이브러리 로딩
library(ggplot2)
library(wordcloud)
library(ggmap)
library(googleVis)

##########1. 경기도 현황 시각화 및 전처리##########
# 경기도 데이터 불러오기
d0 <- read.csv("경기도.csv")
d0 <- subset(d0, d0$재산구분명 =="토지")
d0 <- data.frame(d0$시군구명, d0$공부지목명, d0$재산면적, d0$대장가액.원.)
colnames(d0) <- c("area_name", "assets_name", "size", "price")
d0$size <- ifelse(is.na(d0$size), 1, d0$size)
d0$price<- ifelse(is.na(d0$price), 0, d0$price)
d0$price = round(d0$price)
summary(d0)

# 시군구 현황(막대 그래프)+지역명 단일화
d1 <- grep("경기도", d0$area_name)
d0 <- d0[d1,]
d0$area_name <- gsub("경기도 안양시 동안구", "안양시", d0$area_name)
d0$area_name <- gsub("경기도 안양시 만안구", "안양시", d0$area_name)
d0$area_name <- gsub("경기도 안산시 단원구", "안산시", d0$area_name)
d0$area_name <- gsub("경기도 안산시 상록구", "안산시", d0$area_name)
d0$area_name <- gsub("경기도 부천시 소사구", "부천시", d0$area_name)
d0$area_name <- gsub("경기도 부천시 오정구", "부천시", d0$area_name)
d0$area_name <- gsub("경기도 부천시 원미구", "부천시", d0$area_name)
d0$area_name <- gsub("경기도 성남시 수정구", "성남시", d0$area_name)
d0$area_name <- gsub("경기도 성남시 중원구", "성남시", d0$area_name)
d0$area_name <- gsub("경기도 성남시 분당구", "성남시", d0$area_name)
d0$area_name <- gsub("경기도 수원시 장안구", "수원시", d0$area_name)
d0$area_name <- gsub("경기도 수원시 팔달구", "수원시", d0$area_name)
d0$area_name <- gsub("경기도 수원시 영통구", "수원시", d0$area_name)
d0$area_name <- gsub("경기도 수원시 권선구", "수원시", d0$area_name)
d0$area_name <- gsub("경기도 용인시 처인구", "용인시", d0$area_name)
d0$area_name <- gsub("경기도 용인시 수지구", "용인시", d0$area_name)
d0$area_name <- gsub("경기도 고양시 일산동구", "고양시", d0$area_name)
d0$area_name <- gsub("경기도 고양시 일산서구", "고양시", d0$area_name)
d0$area_name <- gsub("경기도 고양시 덕양구", "고양시", d0$area_name)
d0$area_name <- gsub("경기도", "", d0$area_name)

pal = brewer.pal(8, "Accent")
c1 = c(table(d0$area_name))
c1 = head(sort(c1, decreasing = T), 10)
barplot(c1, col = pal, xlab = "시군구명", ylab = "공유재산량", main = "시군구별 공유재산 상위top10",
        ylim=(c(0,15000)))

c11 = c(table(d0$area_name))
c11 = head(sort(c11), 10)
barplot(c11, col = pal, xlab = "시군구명", ylab = "공유재산량", main = "시군구별 공유재산 하위top10",
        ylim=(c(0,15000)))

# 시군구 비율(구글비스 파이차트)
c2 = table(d0$area_name)
c2 = head(sort(c2, decreasing = T), 15)
c2 = data.frame(c2)
pc2 = gvisPieChart(c2)
header = pc2$html$header
header = gsub("charset=utf-8","charset=euc-kr", header)
pc2$html$header = header
plot(pc2)

# 공부지목 수량(워드클라우드)
c3 = c(table(d0$assets_name))
c3 = sort(c3, decreasing = T)
pal = brewer.pal(8, "Set1")
wordcloud(names(c3), c3, colors = pal, random.order = T)

# 공부지목 비율(구글비스 파이차트)
c4 = table(d0$assets_name)
c4 = data.frame(c4)
pc4 = gvisPieChart(c4)
header = pc4$html$header
header = gsub("charset=utf-8","charset=euc-kr", header)
pc4$html$header = header
plot(pc4)

##########2. 경기도 지역별 분포##########
#성장형 도시(총 12개)
#가평, 고양, 광주, 김포, 남양주, 수원
#용인, 오산, 의왕, 파주, 평택, 화성
#재산면적이 10000 이상이면 개발제한이 예상되므로 분석에서 제외

#1 가평군
g1 <- read.csv("경기도 가평군.csv")
g1 <- data.frame(g1)
g1$재산면적 <- ifelse(is.na(g1$재산면적), 0, g1$재산면적)
g1$재산면적 <- ifelse((g1$재산면적>10000), 0, g1$재산면적)
g1$재산면적 <- round(g1$재산면적)
#1 지도 시각화
(g1_map <- qmap(enc2utf8("경기도 가평군"), zoom = 11, maptype= "roadmap"))
(g1_map <- g1_map+geom_point(data=g1, aes(x=longitude, y=latitude), size=2, alpha=0.5, color="black"))
#2 면적 확인
(g1_map <- g1_map+geom_point(data=g1, aes(x=longitude, y=latitude), size=(g1$재산면적)/5000, alpha=0.2, color="yellow"))
ggmap(g1_map, extent = "normal")

#3 텍스트표기 예시(조건문 사용해서 걸러내면 될듯)
#(g1_map + geom_text(data = g1, aes(x=longitude, y=latitude), size = 2, label = (g1$address), hjust= -.2, vjust= -1))

#2 고양시
g2 <- read.csv("경기도 고양시.csv")
g2 <- data.frame(g2)
g2$재산면적 <- ifelse(is.na(g2$재산면적), 0, g2$재산면적)
g2$재산면적 <- ifelse((g2$재산면적>10000), 0, g2$재산면적)
g2$재산면적 <- round(g2$재산면적)
#1 지도 시각화
(g2_map <- qmap(enc2utf8("경기도 고양시"), zoom = 11, maptype= "roadmap"))
(g2_map <- g2_map+geom_point(data=g2, aes(x=longitude, y=latitude), size=2, alpha=0.5, color="black"))
#2 면적 확인
(g2_map <- g2_map+geom_point(data=g2, aes(x=longitude, y=latitude), size=(g2$재산면적)/5000, alpha=0.2, color="yellow"))

#3 광주시
g3 <- read.csv("경기도 광주시.csv")
g3 <- data.frame(g3)
g3$재산면적 <- ifelse(is.na(g3$재산면적), 0, g3$재산면적)
g3$재산면적 <- ifelse((g3$재산면적>10000), 0, g3$재산면적)
g3$재산면적 <- round(g3$재산면적)
#1 지도 시각화
(g3_map <- qmap(enc2utf8("경기도 광주시"), zoom = 11, maptype= "roadmap"))
(g3_map <- g3_map+geom_point(data=g3, aes(x=longitude, y=latitude), size=2, alpha=0.5, color="black"))
#2 면적 확인
(g3_map <- g3_map+geom_point(data=g3, aes(x=longitude, y=latitude), size=(g3$재산면적)/5000, alpha=0.2, color="yellow"))

#4 김포시
g4 <- read.csv("경기도 김포시.csv")
g4 <- data.frame(g4)
g4$재산면적 <- ifelse(is.na(g4$재산면적), 0, g4$재산면적)
g4$재산면적 <- ifelse((g4$재산면적>10000), 0, g4$재산면적)
g4$재산면적 <- round(g4$재산면적)
#1 지도 시각화
(g4_map <- qmap(enc2utf8("김포시"), zoom = 11, maptype= "roadmap"))
(g4_map <- g4_map+geom_point(data=g4, aes(x=longitude, y=latitude), size=2, alpha=0.5, color="black"))
#2 면적 확인
(g4_map <- g4_map+geom_point(data=g4, aes(x=longitude, y=latitude), size=(g4$재산면적)/5000, alpha=0.2, color="yellow"))

#5 남양주시
g5 <- read.csv("경기도 남양주.csv")
g5 <- data.frame(g5)
g5$재산면적 <- ifelse(is.na(g5$재산면적), 0, g5$재산면적)
g5$재산면적 <- ifelse((g5$재산면적>10000), 0, g5$재산면적)
g5$재산면적 <- round(g5$재산면적)
#1 지도 시각화
(g5_map <- qmap(enc2utf8("남양주"), zoom = 11, maptype= "roadmap"))
(g5_map <- g5_map+geom_point(data=g5, aes(x=longitude, y=latitude), size=2, alpha=0.5, color="black"))
#2 면적 확인
(g5_map <- g5_map+geom_point(data=g5, aes(x=longitude, y=latitude), size=(g5$재산면적)/5000, alpha=0.2, color="yellow"))

#6 동두천시
g6 <- read.csv("경기도 동두천.csv")
g6 <- data.frame(g6)
g6$재산면적 <- ifelse(is.na(g6$재산면적), 0, g6$재산면적)
g6$재산면적 <- ifelse((g6$재산면적>10000), 0, g6$재산면적)
g6$재산면적 <- round(g6$재산면적)
#1 지도 시각화
(g6_map <- qmap(enc2utf8("동두천"), zoom = 11, maptype= "roadmap"))
(g6_map <- g6_map+geom_point(data=g6, aes(x=longitude, y=latitude), size=2, alpha=0.5, color="black"))
#2 면적 확인
(g6_map <- g6_map+geom_point(data=g6, aes(x=longitude, y=latitude), size=(g6$재산면적)/5000, alpha=0.2, color="yellow"))

#시각화 - 구글비스 사용 - 가독성 문제로 폐기
#구글비스 사용법 참고 vignette("googleVis")
#m1 <- read.csv("경기도 광주시.csv")
#region = paste("KR",c(41),sep="-")
#m1$latlong <- paste(m1$latitude, sep=":", m1$longitude)
#m1 <- data.frame(region, m1)
#G <- gvisGeoChart(m1, locationvar = "latlong", sizevar = "대장가액.원.", 
#                  options = list(region="KR", displayMode = "auto",resolution = "provinces", 
#                                 colorAxis="{colors:['lightgreen','red']}"))
#header = G$html$header
#header = gsub("charset=utf-8","charset=euc-kr", header)
#G$html$header = header
#plot(G)

##########3. leaflet 사용 지도시각화##########
#데이터 영문으로 바꿔야 인식함
#leaflet 사용자 패키지
install.packages("htmltools")
library(htmltools)
library(dplyr)
install.packages("leaflet")
library(leaflet)

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
m1$popup <- paste("assets_name: ", m1$assets_name, " size: ", m1$size, " tot_price: ", m1$tot_price, " avg_price: ", m1$avg_price)

#공유재산별 분포 시각화
#m1 <- m1[which(m1$assets_name==c("ddap","jeon","unknown")), 
#         c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')]

#1 가평군의 공유재산 수량 및 분포
op <- leaflet(m1)
op <- op %>% addTiles() %>% setView(lng = 127.4962945, lat = 37.8372685, zoom = 11)
op %>% addTiles() %>% addMarkers(~lon, ~lat, clusterOptions = markerClusterOptions(), popup = ~htmlEscape(popup))

#2 가평군의 공유재산별 컬러 시각화
pal <- c("dae"="navy", "ddap"="gold", "forest"="green", "jeon"="blue", "levee"="red", "random"="black",
         "river"="white", "road"="pink", "unknown"="brown")
op %>% addTiles() %>%addCircleMarkers(radius = 5, color = ~pal(m1$assets_name), 
                                      stroke = FALSE, fillOpacity = 0.5, popup = ~htmlEscape(popup) )
#3 shiny 연결
library(shiny)
library(leaflet)
library(RColorBrewer)
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "avg_price", min(m1$avg_price), max(m1$avg_price),
                            value = range(m1$avg_price), step = 10000),
                selectInput("name", "assets_name", col),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    sname <- input$name
    m1 <- m1[m1$avg_price >= input$range[1] & m1$avg_price <= input$range[2],]
    m1 <- m1[sname %in% m1$assets_name,]
    return(m1)
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$name, m1$avg_price)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(m1) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~m1$avg_price/1000, weight = 5, color = "#777777",
                 fillColor = ~pal(m1$avg_price), fillOpacity = 0.7, popup = ~paste(popup)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = m1)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~m1$avg_price
      )
    }
  })
}
shinyApp(ui, server)
