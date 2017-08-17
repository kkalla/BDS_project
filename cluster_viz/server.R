library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)
pal <- brewer.pal(11,'BrBG')
pal <- colorRampPalette(pal)(19)
apikey <- thunder_api_key

shinyServer(function(input,output,session){
    
    ## Interative Map
    
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles(
                urlTemplate=
                    paste0("http://{s}.tile.thunderforest.com/transport/",
                           "{z}/{x}/{y}.png?apikey=",apikey)) %>%
            setView(lng=127.015358,lat=37.279293,zoom=12)
    })
})
