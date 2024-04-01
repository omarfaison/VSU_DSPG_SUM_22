library(tidyverse)
library(tigris)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(raster)
library(rasterVis)


#READ IN HEAT DATA
heat_data<-raster("pm_hi_f.tif")
heat_data_wgs<-projectRaster(heat_data, crs="+init=epsg:4326")

pburg_merged <- readRDS("pburg_merged.RDS")

heat_mean<-raster::extract(heat_data_wgs, pburg_merged, fun=mean, na.rm=T)
pburg_merged <- cbind(pburg_merged, heat_mean)
pburg_merged<-pburg_merged %>% mutate(tract_ID = str_extract(NAME, "[[:digit:]]{4}"))

saveRDS(pburg_merged,"pburg_merged.RDS")