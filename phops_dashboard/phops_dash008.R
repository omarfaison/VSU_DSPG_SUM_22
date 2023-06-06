library(tidyverse)
library(tigris)
library(sf)
library(shiny)
library(leaflet)
library(DT)

pburg_merged <- readRDS("pburg_merged.RDS")
pburg_merged<-pburg_merged %>% mutate(tract_ID = str_extract(NAME, "[[:digit:]]{4}"))
pburg_tract_centroids<-st_centroid(pburg_merged) %>%
  cbind(st_coordinates(.))

pburg_food <- read.csv("pburg_food.csv", header=T)
pburg_food$type<-factor(pburg_food$type, levels=c("cdb", "chain_groc", "local_groc", "pop"))
pburg_food_sf<-st_as_sf(pburg_food, coords=c("X","Y"), crs="WGS84") %>%
  cbind(st_coordinates(.))

columns<-as.data.frame(names(select(pburg_merged,pop_density:med_income, ACCESS2:TEETHLOST))) %>%
  filter(row_number() != n()) 
columns<-setNames(columns,"var_name")

ui<-fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/pburgdash.css")
  ),
  #These tags will allow us to use CSS to alter the visuals of the dashboard for aesthetic purposes after implementing HTML
  #The stylesheet will be in the "www" folder and called pburgdash.css
  
  titlePanel(tags$header(tags$h1("Petersburg Health and Demographic Data Dashboard"))),
  
  withTags({
    div(checked=NA,
        p("This dashbaord is designed to help visualize data regarding the population of Petersburg on a census tract level. This allows you to select 2 different traits and compare them for your own edification."),
        br(),
        p("In order to use this dashboard, you can select a trait from the drop-down menu above both maps which will display those traits in various ways. The graphs below will change based on the traits you are comparing."),
        br(),
        )
    }),
  
  
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
  ),
  
  tags$footer(tags$p("This dashboard was created with the collaboration of Petersbirg Healthy Options Partnerships(PHOPs), Virginia State University(VSU), and the University of Virginia Biocomplexity Institute's Data Science for The Public Good Young Scholars Program."))
  
)

server<-function(input, output, session) {
snap_pal<-colorFactor(palette=c("yellow", "green"), domain=pburg_food_sf$snap)
  output$demo_map_1<-renderLeaflet({
    leaflet_data1<-pburg_merged %>%
      dplyr::select(GEOID, tract_ID, input$demo1)%>%
      rename(variable=input$demo1)
    
    pal1<-colorNumeric(palette=c("grey","red"), domain=leaflet_data1$variable)
    leaflet(leaflet_data1)%>%
      setView(lng = -77.401924, lat = 37.227928, zoom=12)%>%
      setMaxBounds( lng1 = -77.466595
                    , lat1 = 37.258476
                    , lng2 = -77.319825
                    , lat2 = 37.159903 ) %>%
      addTiles(options = providerTileOptions(minZoom = 12))%>%
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
    leaflet(leaflet_data2)%>%setView(lng = -77.401924, lat = 37.227928, zoom=12)%>%
      setMaxBounds( lng1 = -77.466595
                    , lat1 = 37.258476
                    , lng2 = -77.319825
                    , lat2 = 37.159903 ) %>%
      addTiles(options = providerTileOptions(minZoom = 12))%>%
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
      geom_text(aes(label=tract_ID))+
      theme_minimal()
  })
  
  output$demo_table<-DT::renderDT({
    datatable(pburg_merged %>%as.data.frame() %>% dplyr::select(tract_ID,input$demo1,input$demo2), options = list(
      pageLength = 11)) %>% formatRound(c(input$demo1, input$demo2), 2)
  })  
}


shinyApp(ui, server)