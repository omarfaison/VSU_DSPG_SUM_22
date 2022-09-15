library(foreign)
library(survey)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
library(tidycensus)
library(corrplot)
library(dplyr)

places_shp<-st_read("geo_export_c91b91a6-c41e-41ae-8e0f-47e9d9df6ccd.shp")
places_va<-filter(places_shp, stateabbr=="VA")
places_pburg<-filter(places_va, countyname=="Petersburg")
diabetes_pburg<-filter(places_pburg, measureid=="DIABETES")
crude_pburg<-places_pburg %>%
  filter(datavaluet=="CrdPrv") %>%
  select(GEOID=locationid, tract=locationna, pct=data_value, measure=measureid)
crude_pburg_wide<-crude_pburg %>%
  spread(measure, pct)

saveRDS(crude_pburg_wide, "places21_crude_pburg.RDS")


pburg_data<-get_acs(geography="tract", state="51",county="730",
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
                    geometry=T, cache=T)  

pburg_data_wide <- pburg_data %>%
  dplyr::select(-moe) %>%
  spread(variable, estimate)

pburg_areas<-pburg_data_wide %>%
  mutate(area = as.numeric(st_area(.)))

pburg_areas$NAME_cap=str_to_title(pburg_areas$NAME)


pburg_calculate<-pburg_areas %>%
  transmute(GEOID= GEOID,
            NAME=NAME,
            county=str_extract(NAME_cap, ".+(?= C)"),
            sq_mi = area/2.59e6,
            population = all_pop,
            population_density = all_pop/sq_mi,
            percent_minority = (all_pop - white_pop)/all_pop * 100,
            percent_black = black_pop/all_pop * 100,
            percent_latin = latin_pop/all_pop * 100,
            percent_under_18 = (male_under_5+male_6_to_9+male_10_to_14+male_15_to_17+female_under_5+female_6_to_9+female_10_to_14 +female_15_to_17)/all_pop*100,
            percent_65_over=(male_65_to_66+male_67_to_69+male_70_to_74+male_75_to_79+male_80_to_84+male_85_over+female_65_to_66+female_67_to_69+female_70_to_74+female_75_to_79+female_80_to_84+female_85_over)/all_pop*100,
            percent_poverty = poverty_less100/poverty_all * 100,
            percent_income100kmore = (med_hh_income_100_124k + med_hh_income_125_149k + med_hh_income_150_199k + med_hh_income_200kmore) / med_hh_income_total * 100,
            percent_snap = snap_with_snap/snap_total*100,
            percent_not_in_labor_force = employ_not_in_labor_force/employ_total * 100,
            percent_no_cars = vehicles_no_cars/vehicles_total_household * 100,
            percent_no_bachelor = (pop_edu_total - (pop_edu_bachelor + pop_edu_grad_prof))/pop_edu_total * 100,
            percent_travel_under_hour = (travel_all - travel_90_plus - travel_60_89)/ travel_all * 100,
            percent_stayed_1yr = geographic_mobility_same1yr/geographic_mobility_total * 100,
            percent_homevalue250more = (home_values_250_299k + home_values_300_399k + home_values_400_499k + home_values_500_749k + home_values_750_999k + home_values_1_1.4m + home_values_1.5_1.9m + home_values_2mmore)/home_values_total * 100,
            percent_homeowner = rent_own_own / rent_own_total * 100,
            median_income = med_income)



pburg_demos<-st_transform(pburg_calculate, crs="+proj=longlat +datum=WGS84")

crude_pburg_vals<-crude_pburg_wide %>% st_drop_geometry()

pburg_merged<-left_join(pburg_demos, crude_pburg_vals, by="GEOID") %>%
  select(-county)
pburg_merged <-rename(pburg_merged, access_to_health = ACCESS2)
pburg_merged <-rename(pburg_merged, binge_drink = BINGE)
pburg_merged <-rename(pburg_merged, high_blood_pressure = BPHIGH)
pburg_merged <-rename(pburg_merged, arthritis = ARTHRITIS)
pburg_merged <-rename(pburg_merged, blood_pressure_meds = BPMED)
pburg_merged <-rename(pburg_merged, cancer = CANCER)
pburg_merged <-rename(pburg_merged, child_asthma = CASTHMA)
pburg_merged <-rename(pburg_merged, cervical_screen = CERVICAL)
pburg_merged <-rename(pburg_merged, chronic_smoke = CSMOKING)
pburg_merged <-rename(pburg_merged, yearly_checkup = CHECKUP)
pburg_merged <-rename(pburg_merged, cholesterol_screen = CHOLSCREEN)
pburg_merged <-rename(pburg_merged, colon_screen = COLON_SCREEN)
pburg_merged <-rename(pburg_merged, dental_visit = DENTAL)
pburg_merged <-rename(pburg_merged, depression = DEPRESSION)
pburg_merged <-rename(pburg_merged, diabetes = DIABETES)
pburg_merged <-rename(pburg_merged, good_health = GHLTH)
pburg_merged <-rename(pburg_merged, high_cholesterol = HIGHCHOL)
pburg_merged <-rename(pburg_merged, kidney_disease= KIDNEY)
pburg_merged <-rename(pburg_merged, no_leisure_phys_activity = LPA)
pburg_merged <-rename(pburg_merged, mammography_use_50_74 = MAMMOUSE)
pburg_merged <-rename(pburg_merged, mental_health_issues = MHLTH)
pburg_merged <-rename(pburg_merged, obesity = OBESITY)
pburg_merged <-rename(pburg_merged, poor_health = PHLTH)
pburg_merged <-rename(pburg_merged, sleep_issues = SLEEP)
pburg_merged <-rename(pburg_merged, pulmonary_disease = COPD)
pburg_merged <-rename(pburg_merged, coronary_heart_disease = CHD)
pburg_merged <-rename(pburg_merged, stroke = STROKE)
pburg_merged <-rename(pburg_merged, teeth_lost = TEETHLOST)

pburg_merged = subset(pburg_merged, select = -c(sq_mi) )

tm_shape(pburg_merged)+
  tm_polygons("DIABETES")

ggplot(pburg_merged, aes(x=DIABETES, y=pct_black))+
  geom_point()

saveRDS(pburg_merged, "pburg_merged_named.RDS")
st_write(pburg_merged, "pburg_merged_named.shp")

test2<-readRDS("pburg_merged_named.RDS")

test<-st_read("va_merged.shp")