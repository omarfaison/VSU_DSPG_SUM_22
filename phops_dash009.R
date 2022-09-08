library(readr)
library(tidyverse)
library(tigris)
library(sf)
library(shiny)
library(leaflet)
library(readxl)
library(DT)

pop_locations<-read_excel("POP_Market_Locations_04.2022_Lat_Long.xlsx") %>%
  setNames(c("latitude", "longitude", "site", "address", "city", "state", "zip"))
pop_locations_sf<-st_as_sf(pop_locations, coords=c("longitude","latitude"), crs="WGS84") %>%
  cbind(st_coordinates(.))

pburg_merged <- readRDS("C:/users/brian/Documents/R Stuff/VSU_DSPG_SUM_22/pburg_merged_data/pburg_merged.RDS")
pburg_merged<-pburg_merged %>% mutate(tract_ID = str_extract(NAME, "[[:digit:]]{4}"))
pburg_tract_centroids<-st_centroid(pburg_merged) %>%
  cbind(st_coordinates(.))

pburg_food <- read_csv("phops_dashboard/pburg_food.csv")
pburg_food_sf<-st_as_sf(pburg_food, coords=c("X","Y"), crs="WGS84") %>%
  cbind(st_coordinates(.))

columns<-as.data.frame(names(select_if(pburg_merged,is.numeric))) %>%
  filter(row_number() != n()) 
columns<-setNames(columns,"var_name")

grocery_sf <- filter(pburg_food_sf, type == "grocery")
snap_grocery_sf <- filter(grocery_sf, snap == "y")
no_snap_grocery_sf <- filter(grocery_sf, snap == "n")

cdb_sf <- filter(pburg_food_sf, type == "cdb")
snap_cdb_sf <- filter(cdb_sf, snap == "y")
no_snap_cdb_sf <- filter(cdb_sf, snap == "n")

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
      addMarkers(data=pop_locations_sf, ~X, ~Y, label= ~site, group = "POP Markets") %>%
      addCircleMarkers(data=pburg_tract_centroids, ~X, ~Y, fillOpacity=0, weight=0, label = ~tract_ID, labelOptions = labelOptions(noHide=T, textOnly = T)) %>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="grocery"), group="Grocery Stores", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="cdb"), group="Covenience, Deli, or Bodega", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
    addLayersControl(
      baseGroups = "POP Markets",
      overlayGroups = c("Grocery Stores", "Covenience, Deli, or Bodega"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
  output$demo_map_2<-renderLeaflet({
    leaflet_data2<-pburg_merged %>%
      dplyr::select(GEOID, tract_ID, input$demo2)%>%
      rename(variable=input$demo2)
    
    pal2<-colorNumeric(palette=c("grey","red"), domain=leaflet_data2$variable)
    leaflet(leaflet_data2)%>%
      addTiles()%>%
      addPolygons(fillColor=~pal2(leaflet_data2$variable), fillOpacity = 0.7, weight=0.1)%>% 
      addMarkers(data=pop_locations_sf, ~X, ~Y, label= ~site, group = "POP Markets") %>%
      addCircleMarkers(data=pburg_tract_centroids, ~X, ~Y, fillOpacity=0, weight=0, label = ~tract_ID, labelOptions = labelOptions(noHide=T, textOnly = T)) %>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="grocery"), group="Grocery Stores", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="cdb"), group="Covenience, Deli, or Bodega", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addLayersControl(
        baseGroups = "POP Markets",
        overlayGroups = c("Grocery Stores", "Covenience, Deli, or Bodega"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })  
  
  output$scatter<-renderPlot({
    ggplot(pburg_merged, aes(.data[[input$demo1]], .data[[input$demo2]]))+
      geom_text(aes(label=tract_ID))
  })
  
  output$demo_table<-DT::renderDT({
    datatable(pburg_merged %>%as.data.frame() %>% dplyr::select(tract_ID,input$demo1,input$demo2), options = list(
      pageLength = 11)) %>% formatRound(c(input$demo1, input$demo2), 2)
})
  
}  


shinyApp(ui, server)