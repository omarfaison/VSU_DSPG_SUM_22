library(foreign)
library(survey)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
library(tidycensus)
library(corrplot)

brfss<-read.xport("LLCP2020.XPT")

sort(colnames(brfss))

test_cols<-c("CHILDREN","X_CHLDCNT","X_BMI5", "X_BMI5CAT", "X_STATE")
data<-brfss[test_cols]
va_data<-filter(data, X_STATE==51)

va_counties<-counties("VA",cb=T)

places_shp<-st_read("PLACES_2021.shp")
places_va<-filter(places_shp, stateabbr=="VA")
diabetes_va<-filter(places_va, measureid=="DIABETES")
crude_va<-places_va %>%
  filter(datavaluet=="CrdPrv") %>%
  select(GEOID=locationid, county=locationna, pct=data_value, measure=measureid)
crude_va_wide<-crude_va %>%
  spread(measure, pct)

saveRDS(crude_va_wide, "places21_crude.RDS")


va_data<-get_acs(geography="county", state="51",
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

va_data_wide <- va_data %>%
  dplyr::select(-moe) %>%
  spread(variable, estimate)

va_areas<-va_data_wide %>%
  mutate(area = as.numeric(st_area(.)))

va_areas$NAME_cap=str_to_title(va_areas$NAME)

va_calculate<-va_areas %>%
  transmute(GEOID= GEOID,
            NAME=NAME,
            county=str_extract(NAME_cap, ".+(?= C)"),
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

va_rural<-va_calculate %>%
  mutate(rural_class=case_when(pop_density <= 250 ~ "rural",
                               pop_density >250 & pop_density < 750 ~ "suburban",
                               pop_density >=750 ~ "urban")) %>%
  mutate(rural_class=factor(rural_class, levels=c("rural","suburban","urban")))

va_demos<-st_transform(va_rural, crs="+proj=longlat +datum=WGS84")

crude_va_vals<-crude_va_wide %>% st_drop_geometry()

va_merged<-left_join(va_demos, crude_va_vals, by="GEOID") %>%
  select(-county.y)

tm_shape(va_merged)+
  tm_polygons("DIABETES")

ggplot(va_merged, aes(x=DIABETES, y=pct_black))+
  geom_point()

saveRDS(va_merged, "va_merged.RDS")
st_write(va_merged, "va_merged.shp")

test2<-readRDS("va_merged.RDS")

test<-st_read("va_merged.shp")
 