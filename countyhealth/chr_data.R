 library(tidyverse)
 library(readxl)
 library(dplyr)
 
twotwochrdata <- read_excel("C:/Users/brian/Documents/R Stuff/countyhealth/2022 County Health Rankings Virginia Data - v1.xlsx", sheet = "Outcomes & Factors Rankings", col_names = c("FIPS", "State", "County", "2022 Health Outcomes ZScore", "2022 O Rank", "2022 Health Factors ZScore", "2022 F Rank"), skip = 3)
oneninechrdata <- read_excel("C:/Users/brian/Documents/R Stuff/countyhealth/2019 County Health Rankings Virginia Data - v1_0.xls", sheet = "Outcomes & Factors Rankings", col_names = c("FIPS", "State", "County", "2019 Health Outcomes ZScore", "2019 O Rank", "2019 Health Factors ZScore", "2019 F Rank"), skip = 3)
onesixchrdata <- read_excel("C:/Users/brian/Documents/R Stuff/countyhealth/2016 County Health Rankings Virginia Data - v3.xls", sheet = "Outcomes & Factors Rankings", col_names = c("FIPS", "State", "County", "2016 Health Outcomes ZScore", "2016 O Rank", "2016 Health Factors ZScore", "2016 F Rank"), skip = 3)

joinedchr <- inner_join(twotwochrdata, oneninechrdata, by = "County")
joinedchr1 <- inner_join(joinedchr, onesixchrdata, by = "County")
joinedchr2 <- subset(joinedchr1, select = -c(FIPS.x, FIPS.y, State.x, State.y)) 
joinedchr2 <- joinedchr2[, c(10,11,1,2,3,4,5,6,7,8,9,12,13,14,15)]                     
                    