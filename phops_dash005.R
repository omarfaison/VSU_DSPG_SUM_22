pburg_demos %>%
  dplyr::select(GEOID, tract, pct_minority)%>%
  rename(variable=pct_minority)%>%
ggplot()+
  geom_sf(aes(fill=variable))+
  scale_fill_gradient(low="gray", high="red")+ 
  geom_sf(data=pop_locations_sf, size=5)+
  geom_text(data=pburg_tract_centroids, aes(X,Y,label=tract))+
  theme_void()

pal<-colorNumeric(palette=c("grey","red"), domain=pburg_demos[[pct_minority]])
leaflet(pburg_demos) %>%
  addTiles()%>%
  addPolygons(fillColor=~pal(pct_minority), fillOpacity = 0.7, weight=0.1)%>%
  addCircleMarkers(data=pburg_tract_centroids, ~X, ~Y, fillOpacity=0, weight=0, label = ~tract, labelOptions = labelOptions(noHide=T, textOnly = T))


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
    tm_shape(pburg_demos)+
      tm_polygons(input$demo1, palette=c("gray","red"))+
      tm_text("tract", size=0.7)+
      tm_shape(pop_locations_sf)+
      tm_dots(size=1)
  })
  
  output$demo_map_2<-renderPlot({
    tm_shape(pburg_demos)+
      tm_polygons(input$demo2, palette=c("gray","red"))+
      tm_text("tract", size=0.7)+
      tm_shape(pop_locations_sf)+
      tm_dots(size=1)
  })  
  
  output$scatter<-renderPlot({
    ggplot(pburg_demos, aes(.data[[input$demo1]], .data[[input$demo2]]))+
      geom_text(aes(label=tract))
  })
  
  output$demo_table<-DT::renderDT({
  pburg_demos%>%as.data.frame() %>% dplyr::select(tract,input$demo1,input$demo2)
  })  
}


shinyApp(ui, server)