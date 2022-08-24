library(ggplot2)
library(tidyverse)
library(tidycensus)
install.packages("SASxport")
library(SASxport)
Alcohol <- read.xport("data/P_ALQ.XPT")
#set up census key
census_api_key("insert key here", install=T, overwrite=T)
readRenviron("~/.Renviron")

#ISOLATE COUNTIES AROUND PETERSBURG
va_counties<-counties("VA", cb=T)
pburg_region<-filter(va_counties, COUNTYFP %in% c("730","570","041","053","149"))

##PULL ACS DATA AT COUNTY LEVEL FOR EMPLOYMENT
va_home_data <- get_acs(geography = "county", state = "VA",
                     variables = (c(
                       all_homes = "B25003_001",
                       home_owners = "B25003_002",
                       home_renters = "B25003_003")),geometry=T, cache=T)
head(va_home_data)
