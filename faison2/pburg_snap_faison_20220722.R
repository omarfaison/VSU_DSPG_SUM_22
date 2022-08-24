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
library(osmdata)
library(corrplot)

#read petersburg data at tract level
pburg_data<-get_acs(geography="tract", state="51", county="730",
                    variables=c(all_pop="B03002_001",
                                white_pop="B03002_003",
                                black_pop="B02001_003",
                                latin_pop="B03002_012",
                                male_under_5="B01001_003",
                                male_6_to_9="B01001_004",
                                male_10_to_14="B01001_005",
                                male_15_to_17="B01001_006",
                                female_under_5="B01001_027",
                                female_6_to_9="B01001_028",
                                female_10_to_14="B01001_029",
                                female_15_to_17="B01001_030",
                                male_65_to_66="B01001_020",
                                male_67_to_69="B01001_021",
                                male_70_to_74="B01001_022",
                                male_75_to_79="B01001_023",
                                male_80_to_84="B01001_024",
                                male_85_over="B01001_025",
                                female_65_to_66="B01001_044",
                                female_67_to_69="B01001_045",
                                female_70_to_74="B01001_046",
                                female_75_to_79="B01001_047",
                                female_80_to_84="B01001_048",
                                female_85_over="B01001_049",
                                poverty_all = "B06012_001",
                                poverty_less100 = "B06012_002",
                                med_hh_income_total = "B19001_001",
                                med_hh_income_100_124k = "B19001_014",
                                med_hh_income_125_149k = "B19001_015",
                                med_hh_income_150_199k = "B19001_016",
                                med_hh_income_200kmore = "B19001_017",
                                snap_total = "B09010_001",
                                snap_with_snap = "B09010_002",
                                employ_total = "B23025_001",
                                employ_in_labor_force = "B23025_002",
                                employ_not_in_labor_force = "B23025_007",
                                vehicles_total_household = "B08201_001",
                                vehicles_no_cars = "B08201_002",
                                pop_edu_total = "B06009_001",
                                pop_edu_bachelor = "B06009_005",
                                pop_edu_grad_prof = "B06009_006",
                                travel_all = "B08303_001",
                                travel_60_89 = "B08303_012",
                                travel_90_plus = "B08303_013",
                                geographic_mobility_total = "B07001_001",
                                geographic_mobility_same1yr = "B07001_017",  
                                home_values_total = "B25075_001",
                                home_values_250_299k = "B25075_020",
                                home_values_300_399k = "B25075_021",
                                home_values_400_499k = "B25075_022",
                                home_values_500_749k = "B25075_023",
                                home_values_750_999k = "B25075_024",
                                home_values_1_1.4m = "B25075_025",
                                home_values_1.5_1.9m = "B25075_026",
                                home_values_2mmore = "B25075_027",
                                rent_own_total = "B25003_001",
                                rent_own_own = "B25003_002",
                                med_income = "B19013_001"),
                    geometry = TRUE, year = 2020, cache = TRUE)     

#make data wide
pburg_data_wide <- pburg_data %>%
  dplyr::select(-moe) %>%
  spread(variable, estimate)

#adds area (in m2)
pburg_areas <- pburg_data_wide %>%
  mutate(area = as.numeric(st_area(.)))

#calculates variables
pburg_calculate<-pburg_areas %>%
  transmute(GEOID= GEOID,
            NAME=NAME,
            tract=str_extract(NAME, "[[:digit:]]{4}"),
            sq_mi = area/2.59e6,
            population = all_pop,
            pop_density = all_pop/sq_mi,
            pct_minority = (all_pop - white_pop)/all_pop * 100,
            pct_black = black_pop/all_pop * 100,
            pct_latin = latin_pop/all_pop * 100,
            pct_under_18 = (male_under_5+male_6_to_9+male_10_to_14+male_15_to_17+female_under_5+female_6_to_9+female_10_to_14 +female_15_to_17)/all_pop*100,
            pct_65_over=(male_65_to_66+male_67_to_69+male_70_to_74+male_75_to_79+male_80_to_84+male_85_over+female_65_to_66+female_67_to_69+female_70_to_74+female_75_to_79+female_80_to_84+female_85_over)/all_pop*100,
            pct_poverty = poverty_less100/poverty_all * 100,
            pct_income100more = (med_hh_income_100_124k + med_hh_income_125_149k + med_hh_income_150_199k + med_hh_income_200kmore) / med_hh_income_total * 100,
            pct_snap = snap_with_snap/snap_total*100,
            pct_not_in_labor_force = employ_not_in_labor_force/employ_total * 100,
            pct_no_cars = vehicles_no_cars/vehicles_total_household * 100,
            pct_no_bach = (pop_edu_total - (pop_edu_bachelor + pop_edu_grad_prof))/pop_edu_total * 100,
            pct_travel_under_hr = (travel_all - travel_90_plus - travel_60_89)/ travel_all * 100,
            pct_stayed_1yr = geographic_mobility_same1yr/geographic_mobility_total * 100,
            pct_homevalue250more = (home_values_250_299k + home_values_300_399k + home_values_400_499k + home_values_500_749k + home_values_750_999k + home_values_1_1.4m + home_values_1.5_1.9m + home_values_2mmore)/home_values_total * 100,
            pct_homeowner = rent_own_own / rent_own_total * 100,
            med_income = med_income)

#set demo crs to WGS84
pburg_demos<-st_transform(pburg_calculate, crs="+proj=longlat +datum=WGS84")

#establish centroids for census tracts
pburg_tract_centroids<-st_centroid(pburg_demos) %>%
  cbind(st_coordinates(.))

#read in snap locations and set crs to WGS84
food_locations<-read.csv("pburg_food.csv") 
food_locations_sf<-st_as_sf(food_locations, coords=c("X","Y"), crs="WGS84") 

#isolate numeric data and create correlation matrix
pburg_numeric<-select_if(pburg_demos, is.numeric) %>% st_drop_geometry()
corrplot(cor(pburg_numeric), method = "number")

#view stores by pct_minority
tm_shape(pburg_demos)+
  tm_polygons("pct_minority", palette=c("grey", "purple"))+
  tm_text("tract")+
  tm_shape(food_locations_sf)+
  tm_symbols(col="type", palette=c("red", "green", "yellow"),shape="snap", shapes=c(21,24), size=.6)

#count stores by census tract
snap_in_tract<-st_join(snap_locations_sf, pburg_demos, join=st_within)
snap_count<-snap_in_tract %>% count(tract) %>% st_drop_geometry()
snap_with_demos<-left_join(pburg_demos, snap_count) %>% 
  mutate(stores_sq_mi=n/(area/2.59e6))

#rerun correlation with store information
pburg_store_count_numeric<-select_if(snap_with_demos, is.numeric) %>% st_drop_geometry()
corrplot(cor(pburg_store_count_numeric), method="number")

#plot store counts by pct_minority
ggplot(snap_with_demos, aes(x=reorder(tract, pct_minority), y=n))+
  geom_col()+
  geom_text(aes(label=round(pct_minority, digits=2)))+
  coord_flip()

ggplot(snap_with_demos, aes(x=reorder(tract, pct_minority), y=stores_sq_mi))+
  geom_col()+
  geom_text(aes(label=round(pct_minority, digits=2)))+
  coord_flip()

#add roads data from osm
big_streets <- getbb("Petersburg United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("Petersburg United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

#redraw stores by pct_minority with roads
tm_shape(pburg_demos)+
  tm_polygons("pct_minority")+
  tm_text("tract")+
  tm_shape(snap_locations_sf)+
  tm_dots(size=.2)+
  tm_shape(big_streets$osm_lines)+
  tm_lines(col="green")+
  tm_shape(med_streets$osm_lines)+
  tm_lines(col="blue")

pov_pal<-colorNumeric(palette=c("grey","red"), domain=pburg_demos$pct_minority)
leaflet(pburg_demos)%>%
  addTiles()%>%
  addPolygons(fillColor=~pov_pal(pct_minority), fillOpacity = 0.7, weight=0.1)
  