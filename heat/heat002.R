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
library(rgdal)
library(raster)
library(rasterVis)

heat_data<-raster("_hi_f.tif")
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
  dplyr::select(-moe) %>%
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

#transform crs systems
pburg_data_wgs<-st_transform(pburg_data_calc, crs=4326)
heat_data_wgs<-projectRaster(heat_data, crs="+init=epsg:4326")


#works better
heat_mean<-raster::extract(heat_data_wgs, pburg_data_wgs, fun=mean, na.rm=T)
pburg_heat<-cbind(pburg_data_wgs, heat_mean)

tm_shape(pburg_heat)+
  tm_polygons("heat_mean", palette="BuPu")+
  tm_layout(legend.outside=TRUE)


#does temp vary with black or poor population
ggplot(pburg_heat, aes(x=heat_mean, y=pct_black))+
  geom_point()+
  labs(x="Mean Temp", y="% Black")+
  theme_bw()


ggplot(pburg_heat, aes(x=heat_mean, y=pct_poverty))+
  geom_point()+
  labs(x="Mean Temp", y="% in Poverty")+
  theme_bw()

ggplot(pburg_heat, aes(x=heat_mean, y=med_income))+
  geom_point()

#look at proportion of hot points
heat_points<-raster::extract(heat_data_wgs, pburg_data_wgs, df=T)

heat_props<-heat_points %>%
  na.omit(pm_hi_f)%>%
  mutate(hot=case_when(pm_hi_f >= 98~ 1,
                       TRUE ~0 ))%>%
  group_by(ID) %>%
  summarize(mean=mean(pm_hi_f, na.rm=T),
            hot_count=sum(hot),
            ID_count=n(),
            prop_hot=hot_count/ID_count)


ggplot(heat_props, aes(x=mean, y=prop_hot))+
  geom_point()

pburg_heat<-cbind(pburg_heat, prop_hot=heat_props$prop_hot)

ggplot(pburg_heat, aes(x=prop_hot, y=pct_black))+
  geom_point()

ggplot(pburg_heat, aes(x=prop_hot, y=pct_white))+
  geom_point()

ggplot(pburg_heat, aes(x=prop_hot, y=pct_poverty))+
  geom_point()

ggplot(pburg_heat, aes(x=prop_hot, y=med_income))+
  geom_point()
