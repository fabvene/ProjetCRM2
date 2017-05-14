#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(leaflet)
library(geosphere)


locgeo <- readRDS("../locgeo.RDS")
locgeo <- locgeo[which(locgeo$id != '6395679'),] # observation localisee aux USA (!!)
sub_locgeo <- locgeo


shinyServer(function(input, output, session) {

  
# Calculer voisins -------------------------------------  
  # distances <- NULL
  # sub_locgeo <- reactive({ 
  #   # print("Calcul distance") pour debug
  #   # print(input$ListeClients)
  #   
  #   for (i in 1:nrow(locgeo)) {
  #     
  #     distances[i] <- distm (c(locgeo$lon[which(locgeo$id == as.numeric(input$ListeClients))], 
  #                              locgeo$lat[which(locgeo$id == as.numeric(input$ListeClients))]),
  #                            c(locgeo$lon[i], locgeo$lat[i]),
  #                            fun = distHaversine) / 1000
  #   }
  #   
  #   # distances # en km
  #   
  #   
  #   ids <- which(distances < input$DistMax)
  #   ids <- ids[-1]
  #   sub_locgeo <-  locgeo[ids,]
  #   
  #   sub_locgeo
  #   
  # })
# end calculer voisins ---------------------------------------  
  
  sub_locgeo <- locgeo # ok, mais besoin de version dynamique (ci-dessus)
  
  p <- reactiveValues(clickedMarker=NULL)
  
  
  acm_defaults <- function(map, x, y) 
    addCircleMarkers(map, x, y, radius=6, 
                     color="black", fillColor="orange", fillOpacity=1, opacity=1,
                     weight=2, stroke=TRUE,  layerId = "Selected")
                     
  
  
  
  output$Map <-
    renderLeaflet({
      
      leaflet() %>% addTiles() %>%
        setView(lng = 2.35 , lat =  48.87, zoom = 12) %>%
        addCircleMarkers(data=sub_locgeo, radius=6, color="black", 
                         stroke=FALSE, fillOpacity=0.5, group="locations",layerId=~id)
      

  })
  
  # # observe the marker click info and print to console when it is changed.
  # observeEvent(input$Map_marker_click,{
  #   data$clickedMarker <- input$Map_marker_click
  #   print(data$clickedMarker)}
  # )
  # observeEvent(input$Map_click,{
  #   data$clickedMarker <- NULL
  #   print(data$clickedMarker)})
  
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p$clickedMarker <- input$Map_marker_click
    # print(p$clickedMarker) # for debugging
    proxy <- leafletProxy("Map")
     if(p$clickedMarker$id=="Selected"){
       proxy %>% removeMarker(layerId="Selected")
     } else {
      proxy %>% setView(lng=p$clickedMarker$lng, lat=p$clickedMarker$lat, input$Map_zoom) %>%
         acm_defaults(p$clickedMarker$lng, p$clickedMarker$lat)
    }

  })
  
  observeEvent(input$Map_marker_click, { # update the ListeClients selectInput on map clicks
    p$clickedMarker <- input$Map_marker_click
    if(!is.null(p$clickedMarker$id)){
      if(is.null(input$ListeClients) || input$ListeClients!=p$clickedMarker$id) updateSelectInput(session, inputId="ListeClients", selected=p$clickedMarker$id)
    }
  })
  
  
  
  
  observeEvent(input$ListeClients, { # update the map markers and view on location selectInput changes
    p$clickedMarker <- input$Map_marker_click
    p2 <- subset(sub_locgeo, id==input$ListeClients)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$clickedMarker$id) && input$ListeClients!=p$clickedMarker$id){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    } else if(!length(p$clickedMarker$id)){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    }
  })
  
  # 
  # observeEvent(input$ListeClients, { 
  #   # print("test")
  #   # refClient <- input$ListeClients
  #   ids <- which(monreactive() < input$DistMax)
  #   # print(ids)
  #   clients <- locgeo[ids, ]
  #   proxy <- leafletProxy()
  #   proxy %>% clearMarkers()
  #   #proxy %>% setView(lng = 2.35 , lat =  48.87, zoom = 12) 
  #   proxy %>% addCircleMarkers(data=sub_locgeo(), radius=6, color="black", 
  #                              stroke=FALSE, fillOpacity=0.5, group="locations",layerId=~id)
  #   
  #   
  # })
  
  
  
})