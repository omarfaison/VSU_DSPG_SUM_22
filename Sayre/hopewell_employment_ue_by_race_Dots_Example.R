# Load the tidycensus package into your R session
library(tidyverse)
library(tidycensus)
library(tigris)
library(ggplot2)
library(sf)
library(RColorBrewer)

# Define your Census API key and set it with census_api_key()
api_key <- "a3b50491f4b7fbc864617dee7ae7996a3c15d603"
census_api_key(api_key)

# Set tigris envirnment codes
options(tigris_use_cache=T)
options(tigris_class="sf")


#053 is the FIPS code for Dinwiddie
#149 is Prince George County
#570 is Colonial heights
#670 is Hopewell
#730 is Petersburg

# Get employment data for Hopewell and surrounding counties
county_vars <- c("Hopewell city", "Prince George", "Dinwiddie", "Petersburg city", "Colonial Heights city")
county_fp <- c("053", "149", "570", "670", "730")

county_employ_race <- get_acs(geography = "tract", state ="VA", county = county_vars,
                     variables = c(emp_all_black_a_total = "C23002B_001",
                                   emp_all_native_a_total ="C23002C_001",
                                   emp_all_asian_a_total = "C23002D_001",
                                   emp_all_other_a_total = "C23002F_001",
                                   emp_all_mixed_a_total = "C23002G_001",
                                   emp_all_whxhis_a_total = "C23002H_001",
                                   emp_all_hispanic_a_total = "C23002I_001",
                                   emp_in_black_m_16264 = "C23002B_004",
                                   emp_in_native_m_16264 ="C23002C_004",
                                   emp_in_asian_m_16264 = "C23002D_004",
                                   emp_in_other_m_16264 = "C23002F_004",
                                   emp_in_mixed_m_16264 = "C23002G_004",
                                   emp_in_whxhis_m_16264 = "C23002H_004",
                                   emp_in_hispanic_m_16264 = "C23002I_004",
                                   emp_in_black_m_65 = "C23002B_011",
                                   emp_in_native_m_65 ="C23002C_011",
                                   emp_in_asian_m_65 = "C23002D_011",
                                   emp_in_other_m_65 = "C23002F_011",
                                   emp_in_mixed_m_65 = "C23002G_011",
                                   emp_in_whxhis_m_65 = "C23002H_011",
                                   emp_in_hispanic_m_65 = "C23002I_011",
                                   emp_in_black_f_16264 = "C23002B_017",
                                   emp_in_native_f_16264 ="C23002C_017",
                                   emp_in_asian_f_16264 = "C23002D_017",
                                   emp_in_other_f_16264 = "C23002F_017",
                                   emp_in_mixed_f_16264 = "C23002G_017",
                                   emp_in_whxhis_f_16264 = "C23002H_017",
                                   emp_in_hispanic_f_16264 = "C23002I_017",
                                   emp_in_black_f_65 = "C23002B_024",
                                   emp_in_native_f_65 ="C23002C_024",
                                   emp_in_asian_f_65 = "C23002D_024",
                                   emp_in_other_f_65 = "C23002F_024",
                                   emp_in_mixed_f_65 = "C23002G_024",
                                   emp_in_whxhis_f_65 = "C23002H_024",
                                   emp_in_hispanic_f_65 = "C23002I_024",
                                   ue_in_black_m_16264 = "C23002B_008",
                                   ue_in_native_m_16264 ="C23002C_008",
                                   ue_in_asian_m_16264 = "C23002D_008",
                                   ue_in_other_m_16264 = "C23002F_008",
                                   ue_in_mixed_m_16264 = "C23002G_008",
                                   ue_in_whxhis_m_16264 = "C23002H_008",
                                   ue_in_hispanic_m_16264 = "C23002I_008",
                                   ue_in_black_m_65 = "C23002B_013",
                                   ue_in_native_m_65 ="C23002C_013",
                                   ue_in_asian_m_65 = "C23002D_013",
                                   ue_in_other_m_65 = "C23002F_013",
                                   ue_in_mixed_m_65 = "C23002G_013",
                                   ue_in_whxhis_m_65 = "C23002H_013",
                                   ue_in_hispanic_m_65 = "C23002I_013",
                                   ue_in_black_f_16264 = "C23002B_021",
                                   ue_in_native_f_16264 ="C23002C_021",
                                   ue_in_asian_f_16264 = "C23002D_021",
                                   ue_in_other_f_16264 = "C23002F_021",
                                   ue_in_mixed_f_16264 = "C23002G_021",
                                   ue_in_whxhis_f_16264 = "C23002H_021",
                                   ue_in_hispanic_f_16264 = "C23002I_021",
                                   ue_in_black_f_65 = "C23002B_026",
                                   ue_in_native_f_65 ="C23002C_026",
                                   ue_in_asian_f_65 = "C23002D_026",
                                   ue_in_other_f_65 = "C23002F_026",
                                   ue_in_mixed_f_65 = "C23002G_026",
                                   ue_in_whxhis_f_65 = "C23002H_026",
                                   ue_in_hispanic_f_65 = "C23002I_026"
                                   ), 
                     cache_table = TRUE, geometry = TRUE)

# Separate variable names in to columns
county_employ_race_grps <- county_employ_race %>%
  separate(variable, c("Type","Labor","Race","Sex","Age_group"), "_") %>%
  separate(NAME, c("TRACT","COUNTY","STATE"), ",") %>%
  mutate(TRACT = str_replace(TRACT, "Census Tract ", "")) %>%
  select(-moe)
  

# Consolidate to create specific data sets
## Separate all employment sums from 'in labor force' sums
county_employ_race_all <- county_employ_race_grps %>%
  filter(Labor == "all") 

county_employ_race_in <- county_employ_race_grps %>%
  filter(Labor == "in") 

## Combine sex for age groups
county_employ_race_by_age <- county_employ_race_in %>%
  group_by(COUNTY, TRACT, Type, Labor, Race, Age_group) %>%
  summarize(sums = sum(estimate))

## Combine age groups for race
county_employ_race_by_race <- county_employ_race_by_age %>%
  group_by(COUNTY, TRACT, Type, Labor, Race) %>%
  summarize(sums = sum(sums))

## Clean up and put the variables back together
county_employ_race_in <- county_employ_race_by_race %>%
  select(-Labor) %>%
  mutate(Group = paste(Type,Race, sep="_"),
         Race = case_when(Race == "black"~"Black",
                          Race == "native"~"Native",
                          Race == "asian"~"Asian",
                          Race == "mixed"~"Mixed",
                          Race == "other"~"Other",
                          Race == "whxhis"~"White",
                          Race == "hispanic"~"Hispanic",
                          TRUE~Race))

# Convert the cleaned data into wide format - both tidy and wide versions are used for graphing
county_employ_race_in_wide <- county_employ_race_in %>%
  select(-Type,-Race) %>%
  group_by(TRACT) %>%
  spread(Group, sums)



# Random dot generation using sf -> sf_sample()
# Generate dots, create a group column, and group by group column
ue_dots <- map(c("White", "Black", "Hispanic", "Asian"), function(group) {
  county_employ_race_in %>%
    filter(Race == group) %>%
    st_sample(., size = .$sums ) %>%
    st_sf() %>%
    mutate(group = group) 
  }) 

%>%
    reduce(rbind) %>%
    group_by(group) %>%
    summarize()

# a much faster more efficient way to accomplish the above code
ue_dots_df <- map_dfr(c("White", "Black", "Hispanic", "Asian"), function(group) {
  county_employ_race_in %>%
    filter(Race == group) %>%
    st_sample(., size = .$sums ) %>%
    st_sf() %>%
    mutate(group = group) 
}) 

# Get Ancillary Map Data sets from tigris
# Filter the roads object for major roads only
roads <- roads("VA", county_vars) %>%
  filter(RTTYP %in% c("I", "S", "U"))

# Get an area water dataset 
water <- area_water("VA", county_vars)

# Get the boundary of DC
boundary_all <- counties("VA", cb = TRUE)
  
boundary <- counties("VA", cb = TRUE) %>%
  filter(COUNTYFP %in% county_fp)

# Put it all together into a map plot
# Plot your datasets and give your map an informative caption
ggplot() + 
  geom_sf(data = boundary, color = "black", size=1) + #, fill = "white"
  geom_sf(data = ue_dots_df, aes(color = group, fill = group), size = 0.2) + 
  geom_sf(data = water, color = "lightblue", fill = "lightblue") + 
  geom_sf(data = roads, color = "grey") + 
  coord_sf(datum = NA) + 
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial unemployment of Hopewell and Surrounding Regions", 
       subtitle = "5-year ACS data", 
       fill = "", 
       caption = "Data points are randomly placed and are a representation of the data variation, not actual individual locations.\nData acquired with the R tidycensus and tigris packages.")

display.brewer.all()

# -------------- within race ---------------#

# Calculate Hopewell unemployment percentages - within race
pct_ue_county_race <- county_employ_race_wide %>%
  separate(NAME, c("TRACT","COUNTY","STATE"), ",") %>%
  mutate(TRACT = str_replace(TRACT, "Census Tract ", "")) %>%
  group_by(COUNTY, TRACT) %>%
  summarize(pct_ue_black = (ue_in_black_m_16_64 + ue_in_black_m_65)/employ_total_black,
            pct_ue_native = (ue_in_native_m_16_64 + ue_in_native_m_65)/employ_total_native,
            pct_ue_asian = (ue_in_asian_m_16_64 + ue_in_asian_m_65)/employ_total_asian,
            pct_ue_whxhis = (ue_in_whxhis_m_16_64 + ue_in_whxhis_m_65)/employ_total_whxhis,
            pct_ue_hispanic = (ue_in_hispanic_m_16_64 + ue_in_hispanic_m_65)/employ_total_hispanic,
            pct_ue_mixed = (ue_in_mixed_m_16_64 + ue_in_mixed_m_65)/employ_total_mixed,
            pct_ue_other = (ue_in_other_m_16_64 + ue_in_other_m_65)/employ_total_other)
pct_ue_county_race

# Calculate the mean percentages by county - within race
county_ue_means_race <- pct_ue_county_race %>%
  group_by(COUNTY) %>%
  summarize(pct_ue_mean_black = mean(pct_ue_black, na.rm = TRUE),
            pct_ue_se_black = sd(pct_ue_black, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_native = mean(pct_ue_native, na.rm = TRUE),
            pct_ue_se_native = sd(pct_ue_native, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_asian = mean(pct_ue_asian, na.rm = TRUE),
            pct_ue_se_asian = sd(pct_ue_asian, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_whxhis = mean(pct_ue_whxhis, na.rm = TRUE),
            pct_ue_se_whxhis = sd(pct_ue_whxhis, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_hispanic = mean(pct_ue_hispanic, na.rm = TRUE),
            pct_ue_se_hispanic = sd(pct_ue_hispanic, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_mixed = mean(pct_ue_mixed, na.rm = TRUE),
            pct_ue_se_mixed = sd(pct_ue_mixed, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_other = mean(pct_ue_other, na.rm = TRUE),
            pct_ue_se_other = sd(pct_ue_other, na.rm = TRUE)/sqrt(n()))
county_ue_means_race

# Plot the unemployment data for Hopewell census tracts
ggplot(pct_ue_county_race, aes(TRACT, pct_ue_black, fill=COUNTY)) +
  geom_col()

ggplot(county_ue_means_race, aes(pct_ue_mean_black, reorder(COUNTY, pct_ue_mean_black))) +
  geom_errorbarh(aes(xmin = pct_ue_mean_black - pct_ue_se_black, xmax = pct_ue_mean_black + pct_ue_se_black)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 14) + 
  labs(title = "Percentage of Unempolyed Black Labor Force", 
       subtitle = "Hopewell & Surrounding Counties", 
       x = "Percent Unemployment (bars represent standard error)", 
       y = "")

ggplot(county_ue_means_race, aes(COUNTY, pct_ue_mean_black)) +
  geom_col()

# Map the unemployment percentages
ggplot(county_ue_means_race, aes(fill=pct_ue_mean_black)) +
  geom_sf()

ggplot(pct_ue_county_race, aes(fill=pct_ue_black)) +
  geom_sf()



# -------------- within tract ---------------#

# Calculate Hopewell unemployment percentages - within tract labor
pct_ue_county_race_all <- county_employ_race_wide %>%
  separate(NAME, c("TRACT","COUNTY","STATE"), ",") %>%
  mutate(TRACT = str_replace(TRACT, "Census Tract ", "")) %>%
  group_by(COUNTY, TRACT) %>%
  summarize(total_inlabor = (employ_total_black + employ_total_native + employ_total_asian + employ_total_whxhis +
                               employ_total_hispanic + employ_total_mixed + employ_total_other),
            pct_ue_black = (ue_in_black_m_16_64 + ue_in_black_m_65)/total_inlabor,
            pct_ue_native = (ue_in_native_m_16_64 + ue_in_native_m_65)/total_inlabor,
            pct_ue_asian = (ue_in_asian_m_16_64 + ue_in_asian_m_65)/total_inlabor,
            pct_ue_whxhis = (ue_in_whxhis_m_16_64 + ue_in_whxhis_m_65)/total_inlabor,
            pct_ue_hispanic = (ue_in_hispanic_m_16_64 + ue_in_hispanic_m_65)/total_inlabor,
            pct_ue_mixed = (ue_in_mixed_m_16_64 + ue_in_mixed_m_65)/total_inlabor,
            pct_ue_other = (ue_in_other_m_16_64 + ue_in_other_m_65)/total_inlabor)
pct_ue_county_race_all

# Calculate the mean percentages by county - within race
county_ue_means_race_all <- pct_ue_county_race_all %>%
  group_by(COUNTY) %>%
  summarize(pct_ue_mean_black = mean(pct_ue_black, na.rm = TRUE),
            pct_ue_se_black = sd(pct_ue_black, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_native = mean(pct_ue_native, na.rm = TRUE),
            pct_ue_se_native = sd(pct_ue_native, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_asian = mean(pct_ue_asian, na.rm = TRUE),
            pct_ue_se_asian = sd(pct_ue_asian, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_whxhis = mean(pct_ue_whxhis, na.rm = TRUE),
            pct_ue_se_whxhis = sd(pct_ue_whxhis, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_hispanic = mean(pct_ue_hispanic, na.rm = TRUE),
            pct_ue_se_hispanic = sd(pct_ue_hispanic, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_mixed = mean(pct_ue_mixed, na.rm = TRUE),
            pct_ue_se_mixed = sd(pct_ue_mixed, na.rm = TRUE)/sqrt(n()),
            pct_ue_mean_other = mean(pct_ue_other, na.rm = TRUE),
            pct_ue_se_other = sd(pct_ue_other, na.rm = TRUE)/sqrt(n()))
county_ue_means_race_all

# Plot the unemployment data for Hopewell census tracts
ggplot(pct_ue_county_race_all, aes(TRACT, pct_ue_black, fill=COUNTY)) +
  geom_col()

ggplot(county_ue_means_race_all, aes(pct_ue_mean_black, reorder(COUNTY, pct_ue_mean_black))) +
  geom_errorbarh(aes(xmin = pct_ue_mean_black - pct_ue_se_black, xmax = pct_ue_mean_black + pct_ue_se_black)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 14) + 
  labs(title = "Percentage of Unempolyed Black Labor Force", 
       subtitle = "Hopewell & Surrounding Counties", 
       x = "Percent Unemployment (bars represent standard error)", 
       y = "")

ggplot(county_ue_means_race_all, aes(COUNTY, pct_ue_mean_black)) +
  geom_col()

# Map the unemployment percentages
ggplot(county_ue_means_race_all, aes(fill=pct_ue_mean_black)) +
  geom_sf()

ggplot(pct_ue_county_race_all, aes(fill=pct_ue_black)) +
  geom_sf()
