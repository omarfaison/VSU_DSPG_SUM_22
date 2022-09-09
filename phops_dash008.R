library(tidyverse)
library(tigris)
library(sf)
library(shiny)
library(leaflet)

pburg_merged <- readRDS("D:/code/git/VSU_DSPG_SUM_22/pburg_merged_data/pburg_merged.RDS")
pburg_merged<-pburg_merged %>% mutate(tract_ID = str_extract(NAME, "[[:digit:]]{4}"))
pburg_tract_centroids<-st_centroid(pburg_merged) %>%
  cbind(st_coordinates(.))

pburg_food <- read.csv("phops_dashboard/pburg_food.csv", header=T)
pburg_food$type<-factor(pburg_food$type, levels=c("cdb", "chain_groc", "local_groc", "pop"))
pburg_food_sf<-st_as_sf(pburg_food, coords=c("X","Y"), crs="WGS84") %>%
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
snap_pal<-colorFactor(palette=c("yellow", "green"), domain=pburg_food_sf$snap)
  output$demo_map_1<-renderLeaflet({
    leaflet_data1<-pburg_merged %>%
      dplyr::select(GEOID, tract_ID, input$demo1)%>%
      rename(variable=input$demo1)
    
    pal1<-colorNumeric(palette=c("grey","red"), domain=leaflet_data1$variable)
    leaflet(leaflet_data1)%>%
      addTiles()%>%
      addPolygons(fillColor=~pal1(leaflet_data1$variable), fillOpacity = 0.7, weight=0.1)%>%
      addCircles(data=pburg_tract_centroids, ~X, ~Y, fillOpacity=0, weight=0, label = ~tract_ID, labelOptions = labelOptions(noHide=T, textOnly = T)) %>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="cdb"), group="cdb", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="chain_groc"), group="chain_groc", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="local_groc"), group="local_groc", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="pop"), group="pop", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addLayersControl(overlayGroups = c("cdb", "chain_groc", "local_groc", "pop"), options = layersControlOptions(collapsed = FALSE))
  })
  
  output$demo_map_2<-renderLeaflet({
    leaflet_data2<-pburg_merged %>%
      dplyr::select(GEOID, tract_ID, input$demo2)%>%
      rename(variable=input$demo2)
    
    pal2<-colorNumeric(palette=c("grey","red"), domain=leaflet_data2$variable)
    leaflet(leaflet_data2)%>%
      addTiles()%>%
      addPolygons(fillColor=~pal2(leaflet_data2$variable), fillOpacity = 0.7, weight=0.1)%>%
      addCircleMarkers(data=pburg_tract_centroids, ~X, ~Y, fillOpacity=0, weight=0, label = ~tract_ID, labelOptions = labelOptions(noHide=T, textOnly = T)) %>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="cdb"), group="cdb", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="chain_groc"), group="chain_groc", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="local_groc"), group="local_groc", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="pop"), group="pop", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addLayersControl(overlayGroups = c("cdb", "chain_groc", "local_groc", "pop"), options = layersControlOptions(collapsed = FALSE))
  })  
  
  output$scatter<-renderPlot({
    ggplot(pburg_merged, aes(.data[[input$demo1]], .data[[input$demo2]]))+
      geom_text(aes(label=tract_ID))
  })
  
  output$demo_table<-DT::renderDT({
    pburg_merged%>%as.data.frame() %>% dplyr::select(tract_ID,input$demo1,input$demo2)
  })  
}


shinyApp(ui, server)