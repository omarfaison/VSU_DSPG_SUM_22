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


ui<-fluidPage(
  fluidRow(
    column(6,
           selectInput("demo1", "Select a demographic variable", columns$var_name, NULL)),
    column(6,
           selectInput("demo2", "Select a demographic variable", columns$var_name, NULL))
  ),
  fluidRow(
    column(6,
           plotOutput('demo_map_1')),
    column(6,
           plotOutput('demo_map_2'))
  ),
  fluidRow(
    column(6,
           plotOutput('scatter')),
    column(6,
           DT::DTOutput('demo_table'))
  )
)

server<-function(input, output, session) {
  output$demo_map_1<-renderPlot({
    tm_shape(pburg_merged)+
      tm_polygons(input$demo1, palette=c("gray","red"))+
      tm_text("tract", size=0.7)+
      tm_shape(pop_locations_sf)+
      tm_dots(size=1)
  })
  
  output$demo_map_2<-renderPlot({
    tm_shape(pburg_merged)+
      tm_polygons(input$demo2, palette=c("gray","red"))+
      tm_text("tract", size=0.7)+
      tm_shape(pop_locations_sf)+
      tm_dots(size=1)
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