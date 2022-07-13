library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(mapview)
library(viridis)
library(tmap)
library(ggplot2)
library(ggmap)
library(shiny)


#set up census key
census_api_key("7f9f27c9e5d1ccfe38c62246c5f964df59beff97", install=T, overwrite=T)
readRenviron("~/.Renviron")


##Pull ACS data at tract level for poverty in petersburg
pburg_poverty <- get_acs(geography = "tract", state = "VA",county="730",
                      variables = (c(
                        total = "B06012_001",
                        poverty_below_100 = "B06012_002")),geometry=T, cache=T)

#Remove MOE and make wide
pburg_poverty_wide<-pburg_poverty %>%
  select(-moe) %>%
  spread(variable, estimate)

#Calculate % poverty in Petersburg
pburg_poverty_calc<-pburg_poverty_wide %>%
  transmute(GEOID= GEOID,
            NAME=NAME,
            poverty_below = poverty_below_100/total * 100)

#Graph poverty in Petersburg
ggplot(pburg_poverty_calc, aes(x=reorder(NAME,poverty_below), y=poverty_below, fill=poverty_below))+
  geom_col()+
  labs(x="NAME")+
  coord_flip()



#Mapping poverty in Petersburg tracts
tm_shape(pburg_poverty_calc)+
  tm_fill("poverty_below", palette="Pastel1")+
  tm_borders(col="black")+
  tm_text("NAME", size = 0.5)