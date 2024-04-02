library(tidyverse)
library(tigris)
library(sf)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

#READ IN HEAT DATA
pburg_merged <- readRDS("pburg_merged.RDS")
pburg_tract_centroids<-st_centroid(pburg_merged) %>%
  cbind(st_coordinates(.))

pburg_food <- read.csv("pburg_food.csv", header=T)
pburg_food$type<-factor(pburg_food$type, levels=c("cdb", "chain_groc", "local_groc", "pop"))
pburg_food_sf<-st_as_sf(pburg_food, coords=c("X","Y"), crs="WGS84") %>%
  cbind(st_coordinates(.))

columns<-as.data.frame(names(select_if(pburg_merged,is.numeric))) %>%
  filter(row_number() != n()) 
columns<-setNames(columns,"var_name")

ui <- dashboardPage(
  dashboardHeader(
    title = "Petersburg Health and Demographic Data Dashboard"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map"),
      menuItem("Abbreviations", tabName = "abbreviations"),
      menuItem("Contact Us", tabName = "contact_us"),
      menuItem("About Website", tabName = "about_website")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(
          h2("Petersburg Health and Demographic Data Dashboard"),
          column(6,
                 selectInput("demo1", "Select a demographic variable", columns$var_name, NULL)),
          column(6,
                 selectInput("demo2", "Select a demographic variable", columns$var_name, NULL)),
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
      ),
      tabItem(
        tabName = "abbreviations",
        withTags({
          div(
            p("Here you can see the meaning of the abbreviation of the drop down iteams."),
            br(),
            p("pop_density = Population Density"),
            br(),
            p("pct_minority = Percentage Minority"),
            br(),
            p("pct_black = Percentage Black"),
            br(),
            p("pct_latin = Percentage Latin"),
            br(),
            p("pct_under_18 = Percentage under 18"),
            br(),
            p("pct_65_over = Percentage 65 and over"),
            br(),
            p("pct_poverty = Percentage Poverty"),
            br(),
            p("pct_income100more = Percentage income of 100 and more"),
            br(),
            p("pct_snap = Percentage Snap"),
            br(),
            p("pct_not_in_labor_force = Percentage not in labor force"),
            br(),
            p("pct_no_cars = Percentage of no cars"),
            br(),
            p("pct_no_bach = Percentage No Bachelor"),
            br(),
            p("pct_travel_under_Hr = Percentage travel under an Hour"),
            br(),
            p("pct_stayed_1yr = Percentage stayed 1 year"),
            br(),
            p("pct_homevaule250more = Percentage homevaule of 250K or more"),
            br(),
            p("med_income = Median Income"),
            br(),
            p("ACCESS2 = Current lack of health insurance among adults aged 18–64 years"),
            br(),
            p("ARTHRITIS = Arthritis among adults aged ≥18 years"),
            br(),
            p("BINGE = Binge drinking among adults aged ≥18 years"),
            br(),
            p("BPHIGH = High blood pressure among adults aged ≥18 years"),
            br(),
            p("BPMED = Taking medicine for high blood pressure control among adults aged >=18 years with high blood pressure"),
            br(),
            p("CANCER = Cancer among adults aged ≥18 years"),
            br(),
            p("CASTHMA = Current asthma prevalence among adults aged ≥18 years"),
            br(),
            p("CERVICAL = Cervical cancer screening among adult women aged 21-65 years "),
            br(),
            p("CHD = Coronary heart disease among adults aged ≥18 years "),
            br(),
            p("CHECKUP = Visits to doctor for routine checkup within the past year among adults aged ≥18 years "),
            br(),
            p("CHOLSCREEN = Cholesterol screening among adults aged >=18 years"),
            br(),
            p("CHOLON_SCREEN = Cholesterol screening among adults aged >=18 years"),
            br(),
            p("COPD = Chronic obstructive pulmonary disease among adults aged ≥18 years"),
            br(),
            p("COREM =  Older adult men aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening"),
            br(),
            p("COREW =  Older adult women aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 years"),
            br(),
            p("CSMOKING = Current smoking among adults aged ≥18 years"),
            br(),
            p("DENTAL = Visits to dentist or dental clinic among adults aged ≥18 years"),
            br(),
            p("DEPRESSION = Depression among adults aged ≥18 years"),
            br(),
            p("DIABETES =  Diagnosed diabetes among adults aged ≥18 years"),
            br(),
            p("GHLTH =  Fair or poor self-rated health status among adults aged >=18 years"),
            br(),
            p("HIGHCHOL =  High cholesterol among adults aged ≥18 years who have been screened in the past 5 years"),
            br(),
            p("KIDNEY =  Chronic kidney disease among adults aged ≥18 years"),
            br(),
            p("LPA = No leisure-time physical activity among adults aged >=18 yearss"),
            br(),
            p("MAMMOUSE = Mammography use among women aged 50-74 years"),
            br(),
            p("MHLTH = Mental health not good for >=14 days among adults aged >=18 years"),
            br(),
            p("OBESITY = Obesity among adults aged ≥18 years"),
            br(),
            p("PHLTH = Physical health not good for >=14 days among adults aged >=18 years"),
            br(),
            p("SLEEP = Sleeping less than 7 hours among adults aged ≥18 years"),
            br(),
            p("STROKE = Stroke among adults aged ≥18 years"),
            br(),
            p("TEETHLOST = All teeth lost among adults aged ≥65 years"),
            br(),
            p("heat_mean = Mean of the temperature "),
            br()
          )
        })
      ),
      tabItem(
        tabName = "contact_us",
        withTags({
          div(
            p("Contact the Center for Social Data Analytics by phone at 804-524-3690 or by ",(a("email", href="mailto:research@vsu.edu"))))
        })
      ),
      tabItem(
        tabName = "about_website",
        withTags({
          div(
            p("This dashbaord is designed to help visualize data regarding the population of Petersburg on a census tract level. This allows you to select 2 different demographic or diseaese related variabled and compare them."),
            br(),
            p("In order to use this dashboard, you can select a variable from the drop-down menu above both maps and visualize differences betwen those variables across census tracts in Petersburg, as well as viewing the statistical relationship between those variables on a scatter plot."),
            br(),
            p("The checkboxes on the map allow you to toggle the location of non-restaurant food sources across Petersburg."),
            br(),
            p("POP! Market(Petersburg Offers Produce) is a mobile market offering fresh, local food that presents Petersburg residents in low healthy food access areas with the opportunity to purchase healthy produce and maximize their SNAP benefits through the Virginia Fresh Match program"),
            p(" \"cbd\" stands for Corner Stores, Dollar Stores, and Bodegas"),
            br()
          )
        })
      )
    ),
    tags$br(),
    tags$footer(tags$p("This dashboard was created with the collaboration of Petersbirg Healthy Options Partnerships(PHOPs), the Virginia State University(VSU) Data Science for The Public Good Young Scholars Program, and the VSU Center for Social Data Analytics with the suport of USDA-NIFA award #2023-38821-39588."))
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
      setView(lng = -77.401924, lat = 37.227928, zoom=12)%>%
      setMaxBounds( lng1 = -77.466595
                    , lat1 = 37.258476
                    , lng2 = -77.319825
                    , lat2 = 37.159903 ) %>%
      addTiles(options = providerTileOptions(minZoom = 12))%>%
      addPolygons(fillColor=~pal1(leaflet_data1$variable), fillOpacity = 0.7, weight=0.1)%>%
      addCircles(data=pburg_tract_centroids, ~X, ~Y, fillOpacity=0, weight=0, label = ~tract_ID, labelOptions = labelOptions(noHide=T, textOnly = T)) %>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="cdb"), group="cdb", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="chain_groc"), group="Grocery-Chain", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="local_groc"), group="Grocery-Local", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addCircleMarkers(data=filter(pburg_food_sf, type=="pop"), group="POP Market", ~X, ~Y, color= ~snap_pal(snap), label= ~Name, radius = 3)%>%
      addLayersControl(overlayGroups = c("cdb", "Grocery-Chain", "Grocery-Local", "POP Market"), options = layersControlOptions(collapsed = FALSE))
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
  
  output$scatter <- renderPlot({
    ggplot(pburg_merged, aes(.data[[input$demo1]], .data[[input$demo2]])) +
      geom_text(aes(label = tract_ID)) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", na.rm= T) +
      theme_minimal()
    
    #model <- lm(as.formula(paste(input$demo2, input$demo1, sep = "~")), data = pburg_merged)
    #r_squared <- summary(model)$r.squared
    #r_squared_label <- paste("R-squared: ", round(r_squared, 3))
    
    #gg <- gg + annotate("text", x = max(pburg_merged[[input$demo1]]),
    #                    y = min(pburg_merged[[input$demo2]]), label = r_squared_label, hjust = 1, vjust = -5, color = "red")
    
   
  })
  
  output$demo_table<-DT::renderDT({
    datatable(pburg_merged %>%as.data.frame() %>% dplyr::select(tract_ID,input$demo1,input$demo2), options = list(
      pageLength = 11)) %>% formatRound(c(input$demo1, input$demo2), 2)
  })  
}


shinyApp(ui, server)


