library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

draw <- Public_Assets

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 127.1949, lat = 37.6454, zoom = 10)
  })
  
  #Choose just one city
  drawvalue <- reactive({
    if (input$city == 'All') {
      t <- Public_Assets
      if (input$type == ''){return(t)}else{
        t <- filter(t, 재산명칭 == input$type)
        return(t)}
    }
    else if (input$city == 'Gapyeong') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, 재산명칭 == input$type)
        return(t)}
    }
    else if (input$city == 'Gimpo') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, 재산명칭 == input$type)
        return(t)}
      }
    else if (input$city == 'Goyang') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, 재산명칭 == input$type)
        return(t)}
      }
    else if (input$city == 'Gwangju') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, 재산명칭 == input$type)
        return(t)}
      }
    else {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, 재산명칭 == input$type)
        return(t)}
      }
  })
  
  # Choose just one type
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    sizeBy <- input$size
    draw <- drawvalue()
    colorBy <- "재산명칭"
    
    colorData <- draw[[colorBy]]
    set = c(brewer.pal(12,"Paired"),brewer.pal(6,"Set2"),brewer.pal(7, "Set3"))
  
    if(length(unique(colorData))==1){
      pal <- colorFactor(set[sample((1:25),1)], colorData)
    }
    else{
      pal <- colorFactor(set, colorData)
    }
       
    if (input$cluster == TRUE){
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        showGroup('Cluster') %>%
        addCircles(~lon, ~lat, radius=draw[[sizeBy]], group = "Circle",
                    stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addCircleMarkers(~lon, ~lat, radius = 0, group = "Cluster",
                         clusterOptions = markerClusterOptions())%>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }else{
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        hideGroup('Cluster') %>%
        addCircles(~lon, ~lat, radius=draw[[sizeBy]], group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }
  })
  
  # Show a popup at the given location
  showvcPopup <- function(eventid, latitude, longitude) {
    draw <- drawvalue()
    selectedvc <- filter(draw, lat == latitude, lon == longitude)
    entry <- function(row){
      result <- as.character(tagList(
        tags$strong(sprintf("재산명칭: %s", row[4])), tags$br(),
        sprintf("위도 | 경도: %s | %s", row[2],row[3]), tags$br(),
        sprintf("크기: %s", row[5]), tags$br(),
        sprintf("대장가액: %s", row[6]), tags$br(),
        sprintf("평균가격: %s", row[7]), tags$br()))
      return(result)
    }
    content <- apply(selectedvc, 1, entry)
    content <- paste0(content, collapse = "\n")
    leafletProxy("map") %>% addPopups(longitude, latitude, content, layerId = eventid)
  }

  # When map is clicked, show a popup with assets info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showvcPopup(event$id, event$lat, event$lng)
    })
  })
})

