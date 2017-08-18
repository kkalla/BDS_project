library(shiny)
library(leaflet)

vars1 <- c(
  "All" = "All",
  "Gapyeong" = "Gapyeong",
  "Gimpo" = "Gimpo",
  "Goyang" = "Goyang",
  "Gwangju" = "Gwangju",
  "Namyangju" = "Namyangju"
)

vars2 <- c(
  "Site Area" = "radius",
  "Avg. price" = "avg_price_size"
)

vars3 <- c(
  "All Types" = "",
  "Cemetery" = "cemetery",
  "Dae" = "dae",
  "Dap" = "dap",
  "Factory" = "factory",
  "Fishery" = "fishery",
  "Forest" = "forest",
  "Googeo" = "googeo",
  "Jeon" = "jeon",
  "Levee" = "levee",
  "Mixed" = "mixed",
  "Oil" = "oil",
  "Orchard" = "orchard",
  "Park" = "park",
  "Parking" = "parking",
  "Pasture" = "pasture",
  "Rail" = "rail",
  "Random" = "random",
  "Religion" = "religion",
  "River" = "river",
  "Road" = "road",
  "School" = "school",
  "Sports" = "sports",
  "Unknown" = "unknown",
  "Water" = "water",
  "Garage" = "garage"
)

shinyUI(navbarPage("Gyeonggi-do Public Assets Map", id="nav",
                   
                   tabPanel("Interactive map",
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
                                              
                                              h2("Public Assets"),
                                              
                                              selectInput("city", "City", vars1, selected = "All"),
                                              selectInput("size", "Size", vars2, selected = "radius"),
                                              checkboxInput("cluster", "Add Cluster"),
                                              helpText("Cluster numbers show total Public assets for each area",
                                                       "",
                                                       "(applies to 'All Types' only)"),
                                              radioButtons("type", "Show Just One Type", vars3, selected = '')
                                )
                                )
                              )
                            ))
