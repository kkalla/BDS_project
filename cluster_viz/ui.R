library(shiny)
library(leaflet)

vars_cat = c(
    "All" = "",
    "Culture" = "culture",
    "Library" = "library",
    "Medical" = "medical",
    "Public" = "public",
    "Social" = "social",
    "Sports" = "sports"
)
vars_size = c(
    "Land Size" = "PAREA",
    "Land Value" = "PNILP"
)

vars_clust = c(
    "All" = "all",
    "1" = 1,
    "2" = 2,
    "3" = 3,
    "4" = 4
)
vars_clust2 = c(
    "All" = "all",
    "1" = 1,
    "2" = 2,
    "3" = 3,
    "4" = 4,
    "5" = 5,
    "6" = 6,
    "7" = 7
)

shinyUI(navbarPage("GG", id= "nav",
                   # tabPanel("Public Facility",
                   #          div(class="outer",
                   #              tags$head(includeCSS("styles.css")),
                   #              leafletOutput("map_pf",width="100%",
                   #                            height="100%"),
                   #              
                   #              absolutePanel(id="controller",class="panel panel-default",fixed=TRUE,
                   #                            draggable=TRUE,top=60,left="auto",right = 20,bottom="auto",width=330,
                   #                            height="auto",
                   #                            
                   #                            h3("Clustering by Nearby Search"),
                   #                            
                   #                            selectInput("category","Category",vars_cat),
                   #                            selectInput("size","Size",vars_size),
                   #                            dataTableOutput("cat_info")
                   #                            )
                   #              )
                   #          ),
                   
                   tabPanel("Public assets in the Suwon",
                            div(class="outer",
                                tags$head(includeCSS("styles.css")),
                                leafletOutput("map",width="100%",height="100%"),
                                
                                absolutePanel(id="controller2",class="panel panel-default",fixed = TRUE,
                                              draggable=TRUE,top=60,left="auto",right = 20,bottom="auto",width=330,
                                              height="auto",
                                              
                                              selectInput("clust","Cluster",vars_clust)
                                              
                                )
                            )),
                   tabPanel("Public assets in the Suwon(subset)",
                            div(class="outer",
                                tags$head(includeCSS("styles.css")),
                                leafletOutput("map2",width="100%",height="100%"),
                                
                                absolutePanel(id="controller3",class="panel panel-default",fixed = TRUE,
                                              draggable=TRUE,top=60,left="auto",right = 20,bottom="auto",width=330,
                                              height="auto",
                                              
                                              selectInput("clust2","Cluster",vars_clust2)
                                              
                                )
                            ))
                   )
        )