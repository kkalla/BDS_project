library(shiny)
library(leaflet)

shinyUI(navbarPage("Suwon", id= "nav",
                   tabPanel("Interactive map",
                            div(class="outer",
                                tags$head(includeCSS("styles.css")),
                                leafletOutput("map",width="100%",height="100%")
                                )
                            )
                   )
        )