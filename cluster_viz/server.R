library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

apikey <- thunder_api_key
pbfc_summary <- pbfc_summary
suwon_r500 <- suwon_r500
suwon_all <- suwon_all
suwon_cat_2 <- suwon_cat_2
shinyServer(function(input,output,session){
    
    ## Public Facility
    
    # # Create the map
    # output$map_pf <- renderLeaflet({
    #     leaflet() %>% addTiles() %>%
    #         setView(lng=127.162767,lat=37.556368,zoom=9)
    # })
    # 
    # # Output information of each category
    # observe({
    #     cat <- input$category
    #     if(cat==""){
    #         data<-data.frame()
    #         output$cat_info <- renderDataTable(data)
    #     }else{
    #         data <- pbfc_summary[pbfc_summary$category==cat,]
    #         data <- data.frame(t(data))
    #         output$cat_info <- renderDataTable(data)
    #     }
    #     
    #     
    # })
    # 
    # Add circles
    # observe({
    #     cat <- input$category
    #     
    #     if(cat ==""){
    #         leafletProxy("map_pf",)
    #     }
    # })
    
    ## Public assets in the Suwon
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles(
                urlTemplate=
                    paste0("http://{s}.tile.thunderforest.com/transport/",
                           "{z}/{x}/{y}.png?apikey=",apikey),
                attribution = paste0('Maps ©<a href="http://www.thunderforest.com/">Thunderforest</a>, ',
                                     'Data ©<a href="http://www.openstreetmap.org/copyright">OpenStreetMap contributors</a>')) %>%
            setView(lng=127.015358,lat=37.279293,zoom=12)
    })
    
    data_select <- reactive({
        if(input$clust == "all"){return(suwon_all)}
        else{data <- suwon_all[suwon_all$cluster==as.numeric(input$clust),]
        return(data)}
    })
    
    observe({
        
            selected_Data <- data_select()
            colorData <- as.factor(as.character(selected_Data$cluster))
            pal <- colorFactor(brewer.pal(11,'Paired')[sample(c(2,4,6,8,10,12),4)],colorData)
            leafletProxy("map",data=selected_Data) %>%
                clearShapes() %>% hideGroup('Cluster') %>%
                addCircles(~longitude,~latitude,radius=100,group='Circle',fillOpacity = 0.8,
                           fillColor = pal(colorData),stroke=FALSE) %>%
                addLegend("bottomleft",pal=pal,values=colorData,title="Clusters",layerId = "colorLegend")
        
        
    })
    output$map2 <- renderLeaflet({
        leaflet() %>% 
            addTiles(
                urlTemplate=
                    paste0("http://{s}.tile.thunderforest.com/transport/",
                           "{z}/{x}/{y}.png?apikey=",apikey),
                attribution = paste0('Maps ©<a href="http://www.thunderforest.com/">Thunderforest</a>, ',
                                     'Data ©<a href="http://www.openstreetmap.org/copyright">OpenStreetMap contributors</a>')) %>%
            setView(lng=127.015358,lat=37.279293,zoom=12)
    })
    data_select2 <- reactive({
        if(input$clust2 == "all"){return(suwon_cat_2)}
        else{data <- suwon_cat_2[suwon_cat_2$cluster==as.numeric(input$clust2),]
        return(data)}
    })
    
    observe({
        
        selected_Data2 <- data_select2()
        colorData2 <- as.factor(as.character(selected_Data2$cluster))
        pal <- colorFactor(brewer.pal(8,'Dark2')[sample(c(1:8),7)],colorData2)
        leafletProxy("map2",data=selected_Data2) %>%
            clearShapes() %>% 
            addCircles(~longitude,~latitude,radius=100,group='Circle',fillOpacity = 0.8,
                       fillColor = pal(colorData2),stroke=FALSE) %>%
            addLegend("bottomleft",pal=pal,values=colorData2,title="Clusters",layerId = "colorLegend")
        
        
    })
})

