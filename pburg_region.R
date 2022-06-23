setwd("D:/code/pburg_p20")
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

census_api_key("ea0442429a5972eface00da5f17f47cc37721031", install=T, overwrite=T)
options(tigris_use_cache=T)
options(tigris_class="sf")
readRenviron('~/.Renviron')

va_counties<-counties("VA", cb=T)
pburg_region<-filter(va_counties, COUNTYFP %in% c("730","570","041","053","149"))

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

va_data_wide<-va_data %>%
  select(-moe) %>%
  spread(variable, estimate)

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

va_data_calc%>%
  top_n(10, pct_black)%>%
ggplot(aes(x=reorder(NAME,pct_black), y=pct_black, fill=pct_black))+
  geom_col()+
  labs(x="NAME")+
  coord_flip()

va_data_calc%>%
  top_n(25, pct_poverty)%>%
  ggplot(aes(x=reorder(NAME,pct_poverty), y=pct_poverty, fill=pct_poverty))+
  geom_col()+
  labs(x="NAME")+
  coord_flip()

va_data_calc%>%
  top_n(25, hhold_nowork)%>%
  ggplot(aes(x=reorder(NAME,hhold_nowork), y=hhold_nowork, fill=hhold_nowork))+
  geom_col()+
  labs(x="NAME")+
  coord_flip()

va_data_calc%>%
  top_n(25, pct_uninsured)%>%
  ggplot(aes(x=reorder(NAME,pct_uninsured), y=pct_uninsured, fill=pct_uninsured))+
  geom_col()+
  labs(x="NAME")+
  coord_flip()

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