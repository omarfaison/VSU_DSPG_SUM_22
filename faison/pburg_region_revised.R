# READ IN PACKAGES
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

#SET UP CENSUS CALL
census_api_key("ea0442429a5972eface00da5f17f47cc37721031", install=T, overwrite=T)
options(tigris_use_cache=T)
options(tigris_class="sf")
readRenviron('~/.Renviron')

#ISOLATE COUNTIES AROUND PETERSBURG
va_counties<-counties("VA", cb=T)
pburg_region<-filter(va_counties, COUNTYFP %in% c("730","570","041","053","149"))

#CALCULATE CENTERS OF COUNTIES
pburg_region<-cbind(pburg_region, st_coordinates(st_centroid(pburg_region)))


#PULL ACS DATA AT COUNTY LEVEL FOR RACE, INCOME, EMPLOYMENT, INSURANCE FOR VA
va_data<-get_acs(geography="county", state="51",
                    variables=(c(all_pop = "B03002_001", 
                                 white_pop = "B03002_003", 
                                 black_pop = "B03002_004",
                                 latin_pop = "B03002_012",
                                 pov_total = "B06012_001",
                                 pov_count = "B06012_002",
                                 med_income = "B19013_001",
                                 work_household_total = "B08202_001",
                                 work_household_nowork = "B08202_002",
                               ins_universe= "B27011_001",
                               emp_noins= "B27011_007",
                               unemp_noins= "B27011_012",
                               notlabor_noins= "B27011_017")),
                    geometry=T, cache=T )

#REMOVE MOE AND MAKE DATA WIDE
va_data_wide<-va_data %>%
  select(-moe) %>%
  spread(variable, estimate)

#CALCULATE PERCENTAGES FOR RACE, INCOME, EMPLOYMENT, INSURANCE
va_data_calc<-va_data_wide %>%
  transmute(GEOID= GEOID,
            NAME=NAME,
            population = all_pop,
            pct_white = white_pop/all_pop * 100,
            pct_black = black_pop/all_pop * 100,
            pct_latin = latin_pop/all_pop * 100,
            pct_minority = (all_pop - white_pop)/all_pop * 100,
            pct_poverty = pov_count/pov_total * 100,
            hhold_nowork = work_household_nowork/work_household_total * 100,
            med_income = med_income,
            pct_uninsured = (emp_noins+unemp_noins+notlabor_noins)/ins_universe * 100)

#ISOLATE PETERSBURG REGION AND GRAPH IMPORTANT STATS
pburg_region_calc<-filter(va_data_calc, GEOID %in% c("51730","51570","51041","51053","51149"))
ggplot(pburg_region_calc, aes(x=reorder(NAME,pct_uninsured), y=pct_uninsured, fill=pct_uninsured))+
  geom_col()+
  labs(x="NAME")+
  coord_flip()

ggplot(pburg_region_calc, aes(x=reorder(NAME,hhold_nowork), y=hhold_nowork, fill=hhold_nowork))+
  geom_col()+
  labs(x="NAME")+
  coord_flip()

ggplot(pburg_region_calc, aes(x=reorder(NAME,pct_poverty), y=pct_poverty, fill=pct_poverty))+
  geom_col()+
  labs(x="NAME")+
  geom_text(aes(label=round(pct_poverty,1)), hjust=0)+
  coord_flip()

#GRAPH REGION WITH TMAPS
tm_shape(pburg_region_calc)+
  tm_fill("pct_poverty", palette="BuPu")+
  tm_borders(col="black")+
  tm_shape(pburg_region)+
  tm_text("NAME")

tm_shape(pburg_region_calc)+
  tm_fill("pct_black", palette="BuPu")+
  tm_borders(col="black")+
  tm_shape(pburg_region)+
  tm_text("NAME")

tm_shape(pburg_region_calc)+
  tm_fill("pct_uninsured", palette="BuPu")+
  tm_borders(col="black")+
  tm_shape(pburg_region)+
  tm_text("NAME")

#DRAW MAPS WITH GGPLOT
ggplot(pburg_region_calc)+
  geom_sf(aes(fill=pct_poverty), color="black")+
  scale_fill_gradient(low="grey", high="blue")+
  geom_text(data=pburg_region, aes(x=X, y=Y, label=NAME))+
  theme_void()

#DRAW MAPS WITH LEAFLET
pburg_region_calc_leaflet<-st_transform(pburg_region_calc, crs="+proj=longlat +datum=WGS84")
pburg_region_leaflet<-st_transform(pburg_region, crs="+proj=longlat +datum=WGS84")


pov_pal<-colorNumeric(palette="BuPu", domain=pburg_region_calc$pct_poverty)

leaflet()%>%
  addTiles()%>%
  addPolygons(data=pburg_region_calc_leaflet, fillOpacity=0.8, color=~pov_pal(pct_poverty))%>%
  addPolygons(data=pburg_region_leaflet, fillOpacity=0, weight=0, label=~NAME)

pburg_data<-get_acs(geography="tract", state="51", county="730",
                    variables=(c(all_pop = "B03002_001", 
                                 white_pop = "B03002_003", 
                                 black_pop = "B03002_004",
                                 latin_pop = "B03002_012",
                                 pov_total = "B06012_001",
                                 pov_count = "B06012_002",
                                 med_income = "B19013_001",
                                 work_household_total = "B08202_001",
                                 work_household_nowork = "B08202_002",
                                 ins_universe= "B27011_001",
                                 emp_noins= "B27011_007",
                                 unemp_noins= "B27011_012",
                                 notlabor_noins= "B27011_017")),
                    geometry=T, cache=T )

pburg_data_wide<-pburg_data %>%
  select(-moe) %>%
  spread(variable, estimate)

pburg_data_calc<-pburg_data_wide %>%
  transmute(GEOID= GEOID,
            NAME=NAME,
            tract=str_extract(NAME, "[[:digit:]]{4}"),
            population = all_pop,
            pct_white = white_pop/all_pop * 100,
            pct_black = black_pop/all_pop * 100,
            pct_latin = latin_pop/all_pop * 100,
            pct_minority = (all_pop - white_pop)/all_pop * 100,
            pct_poverty = pov_count/pov_total * 100,
            hhold_nowork = work_household_nowork/work_household_total * 100,
            med_income = med_income,
            pct_uninsured = (emp_noins+unemp_noins+notlabor_noins)/ins_universe * 100)


tm_shape(pburg_data_calc)+
  tm_fill("pct_uninsured", palette="BuPu")+
  tm_borders(col="black")+
  tm_text("tract")

tm_shape(pburg_data_calc)+
  tm_fill("pct_black", palette="BuPu")+
  tm_borders(col="black")+
  tm_text("tract")

tm_shape(pburg_data_calc)+
  tm_fill("pct_poverty", palette="BuPu")+
  tm_borders(col="black")+
  tm_text("tract")