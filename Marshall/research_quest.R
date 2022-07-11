library(ggplot2)
library(tidyverse)
library(tidycensus)
# install.packages("SASxport")
library(SASxport)
Alcohol <- read.xport("data/P_ALQ.XPT")
#set up census key
census_api_key("7f9f27c9e5d1ccfe38c62246c5f964df59beff97", install=T, overwrite=T)
readRenviron("~/.Renviron")

#ISOLATE COUNTIES AROUND PETERSBURG
va_counties<-counties("VA", cb=T)
pburg_region<-filter(va_counties, COUNTYFP %in% c("730","570","041","053","149"))

##PULL ACS DATA AT COUNTY LEVEL FOR EMPLOYMENT
va_home_data <- get_acs(geography = "county", state = "VA",
                     variables = (c(
                       home_owners = "DP04_0090E",
                       home_renters = "DP04_0126E")),geometry=T, cache=T)
head(va_home_data)