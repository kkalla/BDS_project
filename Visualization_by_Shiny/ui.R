library(shiny)
library(leaflet)

vars1 <- c(
  "모두" = "All",
  "가평" = "Gapyeong",
  "김포" = "Gimpo",
  "고양" = "Goyang",
  "광주" = "Gwangju",
  "남양주" = "Namyangju"
)

vars2 <- c(
  "크기" = "radius",
  "면적 당 평균가" = "avg_price_size"
)

vars3 <- c(
  "모두" = "",
  "묘지" = "cemetery",
  "대" = "dae",
  "답" = "dap",
  "공장용" = "factory",
  "양어장" = "fishery",
  "임야" = "forest",
  "구거" = "googeo",
  "전" = "jeon",
  "제방" = "levee",
  "잡종지" = "mixed",
  "유지" = "oil",
  "과수원" = "orchard",
  "공원" = "park",
  "주차장" = "parking",
  "목장용지" = "pasture",
  "철도용지" = "rail",
  "종교용지" = "religion",
  "하천" = "river",
  "도로" = "road",
  "학교용지" = "school",
  "체육용지" = "sports",
  "미등록" = "unknown",
  "수도용지" = "water",
  "창고용지" = "garage"
)

shinyUI(navbarPage("경기도 공유재산", id="nav",
                   
                   tabPanel("인터랙티브 맵",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("공유재산"),
                                              
                                              selectInput("city", "도시", vars1, selected = "All"),
                                              selectInput("size", "크기", vars2, selected = "radius"),
                                              checkboxInput("cluster", "클러스터링"),
                                              helpText("각 지역에 해당하는 전체 공유재산의 수 클러스터링",
                                                       "",
                                                       "('모두' 유형에만 적용가능)"),
                                              radioButtons("type", "재산 유형 선택", vars3, selected = '')
                                )
                                )
                              )
                            ))
