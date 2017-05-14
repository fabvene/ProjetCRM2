#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(leaflet)

locgeo <- readRDS("../locgeo.RDS")

# OK - version standard ---------------------------------
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Plan de Paris"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("ListeClients","Ref. Client", locgeo$id),
      sliderInput("DistMax", "Distance Maximale (en km)", 0, 10,
                  10, 0.5 ),
      checkboxGroupInput("Score", "Probab. Succes",
                         c("Tres Bon","Bon","Moyen","Faible"))
      # numericInput("NumObs", "Test: nombre d'obs", nrow(locgeo),
      #             1, nrow(locgeo), 1) 
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("Map")
      
      
      
      
    )
  )
))
# end standard version -------------------------------------


# bootstrapPage version (marche pas) ------------------------------------

# ui <- bootstrapPage(
#   tags$style(type="text/css", "html, body {width:100%;height:100%}"),
#   leafletOutput("Map", width="100%", height="100%"),
#   absolutePanel(top=10, right=10,
#                 selectInput("ListeClients","Ref. Client", locgeo$id),
#                 sliderInput("DistMax", "Distance Maximale (en km)", 0, 10,
#                             10, 0.5 ),
#                 checkboxGroupInput("Score", "Probab. Succes",
#                                    c("Tres Bon","Bon","Moyen","Faible")),
#                 conditionalPanel("input.location !== null && input.location !== ''",
#                                  actionButton("button_plot_and_table", "View Plot/Table", class="btn-block"))
#   )
# )