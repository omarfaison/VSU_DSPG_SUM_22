library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(tmap)
library(tmaptools)
library(maps)
library(leaflet)
library(viridis)
library(RColorBrewer)
library(stringr)
library(readxl)
library(shiny)
library(DT)
library(mapview)
library(leafsync)
library(ggspatial)

#pburg_merged RDS used as data 


pburg_tract_centroids<-st_centroid(pburg_merged) %>%
  cbind(st_coordinates(.))

pop_locations<-read_excel("POP_Market_Locations_04.2022_Lat_Long.xlsx") %>%
  setNames(c("latitude", "longitude", "site", "address", "city", "state", "zip"))
pop_locations_sf<-st_as_sf(pop_locations, coords=c("longitude","latitude"), crs="WGS84") %>%
  cbind(st_coordinates(.))


columns<-as.data.frame(names(select_if(pburg_merged,is.numeric))) %>%
  filter(row_number() != n()) 
columns<-setNames(columns,"var_name")

pal <- colorNumeric("BuPu", domain = NULL)

ui<-fluidPage(
  fluidRow(
    column(6,
           selectInput("demo1", "Select a demographic variable", columns$var_name, NULL)),
    column(6,
           selectInput("demo2", "Select a demographic variable", columns$var_name, NULL))
  ),
  fluidRow(
    column(6,
           leafletOutput('demo_map_1')),
    column(6,
           leafletOutput('demo_map_2'))
  ),
  fluidRow(
    column(6,
           plotOutput('scatter')),
    column(6,
           DT::DTOutput('demo_table'))
  )
)

server<-function(input, output, session) {
  
  output$demo_map_1<-renderLeaflet({
       leaflet(data = pburg_merged) %>%
      addTiles() %>%
      setView(lng = -77.4019, lat = 37.2279, zoom = 12) %>%
      addPolygons(weight = 2,
                  color = ~pal,
                  label = ~paste0(input$demo1),
                  highlight = highlightOptions(weight = 10,
                                               color = "blue",
                                               bringToFront = TRUE)) %>%
      addMarkers(lng = pop_locations$longitude, lat = pop_locations$latitude)
   
  })
  
  output$demo_map_2<-renderLeaflet({
    leaflet(data = pburg_merged) %>%
      addTiles() %>%
      setView(lng = -77.4019, lat = 37.2279, zoom = 12) %>%
      addMarkers(lng = pop_locations$longitude, lat = pop_locations$latitude)
    
  })  
  
  output$scatter<-renderPlot({
    ggplot(pburg_merged, aes(.data[[input$demo1]], .data[[input$demo2]]))+
      geom_text(aes(label=tract))
  })
  
  output$demo_table<-DT::renderDT({
    pburg_merged%>%as.data.frame() %>% dplyr::select(tract,input$demo1,input$demo2)
  })  
}


shinyApp(ui, server)