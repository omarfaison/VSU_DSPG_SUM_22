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

pburg_data<-get_acs(geography="tract", state="51", county="730",
                          variables = (c(travel_all = "B08303_001",
                                         travel_less_5 =  "B08303_002",
                                         travel_5_9 ="B08303_003",
                                         travel_10_14 = "B08303_004",
                                         travel_15_19 = "B08303_005",
                                         travel_20_24 = "B08303_006",
                                         travel_25_29 = "B08303_007",
                                         travel_30_34 = "B08303_008",
                                         travel_35_39 = "B08303_009",
                                         travel_40_44 = "B08303_010",
                                         travel_45_59 = "B08303_011",
                                         travel_60_89 = "B08303_012",
                                         travel_90_plus = "B08303_013",
                                         vehicles_all = "B25044_001",
                                         vehicles_own_all = "B25044_002",
                                         vehicles_own_0 = "B25044_003",
                                         vehicles_own_1 = "B25044_004",
                                         vehicles_own_2 = "B25044_005",
                                         vehicles_own_3 = "B25044_006",
                                         vehicles_own_4 = "B25044_007",
                                         vehicles_own_5more = "B25044_008",
                                         vehicles_rent_all = "B25044_009",
                                         vehicles_rent_0 = "B25044_010",
                                         vehicles_rent_1 = "B25044_011",
                                         vehicles_rent_2 = "B25044_012",
                                         vehicles_rent_3 = "B25044_013",
                                         vehicles_rent_4 = "B25044_014",
                                         vehicles_rent_5more = "B25044_015",
                                         travelmeans_all = "B08101_001",
                                         travelmeans_alone = "B08101_009",
                                         travelmeans_carpool = "B08101_017",
                                         travelmeans_publicnotaxi = "B08101_025",
                                         travelmeans_walked = "B08101_033",
                                         travelmeans_other = "B08101_041",
                                         travelmeans_athome = "B08101_049",
                                         pop_race_total = "B02001_001",
                                         pop_race_white = "B02001_002",
                                         pop_race_black = "B02001_003",
                                         pop_race_native = "B02001_004",
                                         pop_race_asian = "B02001_005",
                                         pop_race_pacific = "B02001_006",
                                         pop_race_other = "B02001_007",
                                         pop_race_mixed = "B02001_008",
                                         pop_edu_total = "B06009_001",
                                         pop_edu_less_high = "B06009_002",
                                         pop_edu_high = "B06009_003",
                                         pop_edu_ad_sd = "B06009_004",
                                         pop_edu_bachelor = "B06009_005",
                                         pop_edu_grad_prof = "B06009_006",
                                         poverty_all = "B06012_001",
                                         poverty_less100 = "B06012_002",
                                         poverty_100_149 = "B06012_003",
                                         poverty_150more = "B06012_004",
                                         geographic_mobility_total = "B07001_001",
                                         geographic_mobility_same1yr = "B07001_017",
                                         geographic_mobility_samecounty = "B07001_033",
                                         geographic_mobility_samestate = "B07001_049",
                                         geographic_mobility_samecountry = "B07001_065",
                                         geographic_mobility_abroad = "B07001_081",
                                         vehiclesavailable_total = "B08014_001",
                                         vehiclesavailable_0 = "B08014_002",
                                         vehiclesavailable_1 = "B08014_003",
                                         vehiclesavailable_2 = "B08014_004",
                                         vehiclesavailable_3 = "B08014_005",
                                         vehiclesavailable_4 = "B08014_006",
                                         vehiclesavailable_5more = "B08014_007",
                                         hhtype_total = "B11001_001",
                                         hhtype_married = "B11001_003",
                                         hhtype_other = "B11001_004",
                                         hhtype_nofam_alone = "B11001_008",
                                         hhtype_nofam_notalone = "B11001_009",
                                         med_hh_income_total = "B19001_001",
                                         med_hh_income_less10k = "B19001_002",
                                         med_hh_income_10_14k = "B19001_003",
                                         med_hh_income_15_19k = "B19001_004",
                                         med_hh_income_20_24k = "B19001_005",
                                         med_hh_income_25_29k = "B19001_006",
                                         med_hh_income_30_34k = "B19001_007",
                                         med_hh_income_35_39k = "B19001_008",
                                         med_hh_income_40_44k = "B19001_009",
                                         med_hh_income_45_49k = "B19001_010",
                                         med_hh_income_50_59k = "B19001_011",
                                         med_hh_income_60_74k = "B19001_012",
                                         med_hh_income_75_99k = "B19001_013",
                                         med_hh_income_100_124k = "B19001_014",
                                         med_hh_income_125_149k = "B19001_015",
                                         med_hh_income_150_199k = "B19001_016",
                                         med_hh_income_200kmore = "B19001_017",
                                         vet_status_total = "B21001_001",
                                         vet_status_vet = "B21001_002",
                                         vet_status_nonvet = "B21001_003",
                                         employ_total = "B23025_001",
                                         employ_working = "B23025_002",
                                         employ_notworking = "B23025_003",
                                         rent_own_total = "B25003_001",
                                         rent_own_own = "B25003_002",
                                         rent_own_rent = "B25003_003",
                                         home_values_total = "B25075_001",
                                         home_values_10kless = "B25075_002",
                                         home_values_10_14k = "B25075_003",
                                         home_values_15_19k = "B25075_004",
                                         home_values_20_24k = "B25075_005",
                                         home_values_25_29k = "B25075_006",
                                         home_values_30_34k = "B25075_007",
                                         home_values_35_39k = "B25075_008",
                                         home_values_40_49k = "B25075_009",
                                         home_values_50_59k = "B25075_010",
                                         home_values_60_69k = "B25075_011",
                                         home_values_70_79k = "B25075_012",
                                         home_values_80_89k = "B25075_013",
                                         home_values_90_99k = "B25075_014",
                                         home_values_100_124k = "B25075_015",
                                         home_values_125_149k = "B25075_016",
                                         home_values_150_174k = "B25075_017",
                                         home_values_175_199k = "B25075_018",
                                         home_values_200_249k = "B25075_019",
                                         home_values_250_299k = "B25075_020",
                                         home_values_300_399k = "B25075_021",
                                         home_values_400_499k = "B25075_022",
                                         home_values_500_749k = "B25075_023",
                                         home_values_750_999k = "B25075_024",
                                         home_values_1_1.4m = "B25075_025",
                                         home_values_1.5_1.9m = "B25075_026",
                                         home_values_2mmore = "B25075_027"
                          )),
                          geometry = TRUE, cache = TRUE)     

pburg_data_wide <- pburg_data %>%
  dplyr::select(-moe) %>%
  spread(variable, estimate)
pburg_areas <- pburg_data_wide %>%
  mutate(area = as.numeric(st_area(.)))
pburg_calculate<-pburg_areas %>%
  transmute(GEOID= GEOID,
            NAME=NAME,
            tract=str_extract(NAME, "[[:digit:]]{4}"),
            pct_minority = (pop_race_total - pop_race_white)/pop_race_total * 100,
            pct_travel_1hrless = (travel_all - travel_90_plus - travel_60_89)/ travel_all * 100,
            pct_stayed_1yr = geographic_mobility_same1yr/geographic_mobility_total * 100,
            pct_homevaluemore250k = (home_values_250_299k + home_values_300_399k + home_values_400_499k + home_values_500_749k + home_values_750_999k + home_values_1_1.4m + home_values_1.5_1.9m + home_values_2mmore)/home_values_total * 100,
            pct_homeowner = rent_own_own / rent_own_total * 100,
            pct_employed = employ_working / employ_total *100,
            pct_incomemore100k = (med_hh_income_100_124k + med_hh_income_125_149k + med_hh_income_150_199k + med_hh_income_200kmore) / med_hh_income_total * 100,
            pct_nofamalone = hhtype_nofam_alone / hhtype_total * 100,
            pct_married = hhtype_married / hhtype_total * 100,
            density = pop_race_total/(area/2.59e6)
  )

pburg_demos<-st_transform(pburg_calculate, crs="+proj=longlat +datum=WGS84")
pburg_tract_centroids<-st_centroid(pburg_demos) %>%
  cbind(st_coordinates(.))

pop_locations<-read_excel("POP_Market_Locations_04.2022_Lat_Long.xlsx") %>%
  setNames(c("latitude", "longitude", "site", "address", "city", "state", "zip"))
pop_locations_sf<-st_as_sf(pop_locations, coords=c("longitude","latitude"), crs="WGS84") %>%
  cbind(st_coordinates(.))


columns<-as.data.frame(names(select_if(pburg_demos,is.numeric))) %>%
  filter(row_number() != n()) 
columns<-setNames(columns,"var_name")


  
ui<-fluidPage(
  selectInput("demographic", "Select a demographic variable", columns$var_name, NULL),
  plotOutput('demo_graph_ggplot'),
  plotOutput('demo_graph_tmaps'),
  leafletOutput('demo_graph_leaflet')

)

server<-function(input, output, session) {
  output$demo_graph_ggplot<-renderPlot({
    pburg_demos %>%
      dplyr::select(GEOID, tract, input$demographic)%>%
      rename(variable=input$demographic)%>%
      ggplot()+
      geom_sf(aes(fill=variable))+
      labs(fill=input$demographic)+
      scale_fill_gradient(low="gray", high="red")+ 
      geom_text(data=pburg_tract_centroids, aes(X,Y,label=tract))+
      geom_sf(data=pop_locations_sf, size=5)+
      theme_void()
  })

  output$demo_graph_tmaps<-renderPlot({
    tm_shape(pburg_demos)+
      tm_polygons(input$demographic, palette=c("gray","red"))+
      tm_text("tract", size=0.7)+
      tm_shape(pop_locations_sf)+
      tm_dots(size=1)
  })
  
  output$demo_graph_leaflet<-renderLeaflet({
    leaflet_data<-pburg_demos %>%
      dplyr::select(GEOID, tract, input$demographic)%>%
      rename(variable=input$demographic)
    
    pal<-colorNumeric(palette=c("grey","red"), domain=leaflet_data$variable)
    leaflet(leaflet_data)%>%
      addTiles()%>%
      addPolygons(fillColor=~pal(leaflet_data$variable), fillOpacity = 0.7, weight=0.1)%>%
      addCircleMarkers(data=pburg_tract_centroids, ~X, ~Y, fillOpacity=0, weight=0, label = ~tract, labelOptions = labelOptions(noHide=T, textOnly = T)) %>%
      addCircleMarkers(data=pop_locations_sf, ~X, ~Y, label= ~site)
  })
  
}

shinyApp(ui, server)
