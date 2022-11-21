library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(janitor)
library(cluster)
library(factoextra)
library(dendextend)
library(purrr)

#importing data
twentytwomeasures <- read_excel("C:/Users/brian/Documents/R Stuff/countyhealth/2022 County Health Rankings Virginia Data - v1.xlsx", sheet = "Ranked Measure Data")
nineteenmeasures <- read_excel("C:/Users/brian/Documents/R Stuff/countyhealth/2019 County Health Rankings Virginia Data - v1_0.xls", sheet = "Ranked Measure Data")

#save as rds
saveRDS(twentytwomeasures, file = "twentytwomeaures.rds")
saveRDS(nineteenmeasures, file = "nineteenmeaures.rds")

# selecting specified columns
twentytwo <- twentytwomeasures[,c(1,2,3,30,34,38,43,62,66,70,72,76,78,84,89,111,117,125,127,150,156,162,164,175,179,187,190,209,211,213,246)]
nineteen <- nineteenmeasures[,c(1,2,3,11,15,19,24,31,35,39,41,45,47,53,58,68,74,82,84,100,104,110,112,121,125,133,136,140,142,144,159)]

#turning first row to column names
twentytwo <- janitor::row_to_names(twentytwo, 1, remove_rows_above = FALSE)
nineteen <- janitor::row_to_names(nineteen, 1, remove_rows_above = FALSE)

#make numeric, drop water violation column, drop provider and mental health ratio, county, state, and fips
twentytwo_numeric <- twentytwo[c(-1,-2,-3, -17, -18, -29)]
nineteen_numeric <- nineteen[c(-1,-2,-3, -17, -18, -29)]

# renaming columns 2022
colnames(twentytwo_numeric)[1] = "pct_fairpoor_health"
colnames(twentytwo_numeric)[2] = "avg_unhealthy_days"
colnames(twentytwo_numeric)[3] = "avg_ment_unhealthy_days"
colnames(twentytwo_numeric)[4] = "pct_low_birthweight"
colnames(twentytwo_numeric)[5] = "pct_smoker"
colnames(twentytwo_numeric)[6] = "pct_adult_obesity"
colnames(twentytwo_numeric)[7] = "food_env_index"
colnames(twentytwo_numeric)[8] = "pct_phys_inactive"
colnames(twentytwo_numeric)[9] = "pct_access_exercise"
colnames(twentytwo_numeric)[10] = "pct_excess_drink"
colnames(twentytwo_numeric)[11] = "pct_death_alc_drive"
colnames(twentytwo_numeric)[12] = "chlamydia_rate"
colnames(twentytwo_numeric)[13] = "pct_uninsured"
colnames(twentytwo_numeric)[14] = "prevent_hospit_rate"
colnames(twentytwo_numeric)[15] = "pct_complete_hs"
colnames(twentytwo_numeric)[16] = "pct_some_college"
colnames(twentytwo_numeric)[17] = "pct_unemployed"
colnames(twentytwo_numeric)[18] = "pct_child_poverty"
colnames(twentytwo_numeric)[19] = "income_ratio"
colnames(twentytwo_numeric)[20] = "pct_child_single_parent"
colnames(twentytwo_numeric)[21] = "violent_crime_rate"
colnames(twentytwo_numeric)[22] = "injury_death_rate"
colnames(twentytwo_numeric)[23] = "avg_daily_pm"
colnames(twentytwo_numeric)[24] = "pct_severe_housing_prob"
colnames(twentytwo_numeric)[25] = "pct_long_comm_alone"

#hierarchical clustering 2022
dist_twentytwo <- dist(twentytwo_numeric, method = 'euclidean')
hc_twentytwo <- hclust(dist_twentytwo)
clusters_k4 <- cutree(hc_twentytwo, k = 4)
twentytwo_k4_complete <- mutate(twentytwo_numeric, cluster = clusters_k4)
count(twentytwo_k4_complete, cluster)

#hierarchical clustering 2019
#dist_nineteen <- dist(nineteen_numeric, method = 'euclidean')
#hc_nineteen<- hclust(dist_nineteen)
#nineteen_clusters_k4 <- cutree(hc_nineteen, k = 4)
#nineteen_k4_complete <- mutate(nineteen_numeric, cluster = nineteen_clusters_k4)
#count(nineteen_k4_complete, cluster)

#make twentytwo_k4_complete numeric
twentytwo_k4_complete<- as.data.frame(apply(twentytwo_k4_complete, 2, as.numeric))
sapply(twentytwo_k4_complete, class) 
str(twentytwo_k4_complete)

#make nineteen_k4_complete numeric
#nineteen_k4_complete<- as.data.frame(apply(nineteen_k4_complete, 2, as.numeric))
#sapply(nineteen_k4_complete, class) 
#str(nineteen_k4_complete)

#more hierarchical 2022
hc_twentytwo_complete <- hclust(dist_twentytwo, method = 'complete')
hc_twentytwo_single <- hclust(dist_twentytwo, method = 'single')
hc_twentytwo_average <- hclust(dist_twentytwo, method = 'average')

#more hierarchical 2019
#hc_nineteen_complete <- hclust(dist_twentytwo, method = 'complete')
#hc_nineteen_single <- hclust(dist_twentytwo, method = 'single')
#hc_nineteen_average <- hclust(dist_twentytwo, method = 'average')

#dendrograms
plot(hc_twentytwo_complete, main = 'Complete Linkage')
plot(hc_twentytwo_single, main = 'Single Linkage')
plot(hc_twentytwo_average, main = 'Average Linkage')

#dendrograms
#plot(hc_nineteen_complete, main = 'Complete Linkage')
#plot(hc_nineteen_single, main = 'Single Linkage')
#(hc_nineteen_average, main = 'Average Linkage')

#dendrogram object and colored dendrogram at height 1500
dend_twentytwo <- as.dendrogram(hc_twentytwo)
plot(dend_twentytwo)
dend_1500 <- color_branches(dend_twentytwo, h = 1500)
plot(dend_1500)

#dendrogram object and colored dendrogram at height 1500
#dend_nineteen <- as.dendrogram(hc_nineteen)
#plot(dend_nineteen)
#nineteen_dend_1500 <- color_branches(dend_nineteen, h = 1500)
#plot(nineteen_dend_1500)

#mean for each category/cluster
twentytwo_mean <- twentytwo_k4_complete %>% 
  group_by(cluster) %>% 
  summarize_if(is.numeric(mean) na.rm=TRUE))

#mean for each category/cluster
#nineteen_mean <- twentytwo_k4_complete %>% 
 # group_by(cluster) %>% 
  #summarize_all(list(mean))

#looping 22

titles<-colnames(twentytwo_mean)[2:26]



for (i in titles){
  
  graph<-ggplot(twentytwo_mean, aes_string(x="cluster", y=i))+
    
    geom_col()
  
  print(graph)
  
}


#specify path to save PDF to
destination = 'C:/Users/brian/Documents/R Stuff/VSU_DSPG_SUM_22'

#open PDF
pdf(file="county_pdf", 'C:/Users/brian/Documents/R Stuff/VSU_DSPG_SUM_22')


#save plots to PDF
titles<-colnames(twentytwo_mean)[2:26]



for (i in titles){
  
  graph<-ggplot(twentytwo_mean, aes_string(x="cluster", y=i))+
    
    geom_col()
  
  print(graph)
  
}

#turn off PDF plotting
dev.off() 



                           


