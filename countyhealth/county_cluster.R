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

# selecting specified columns
twentytwo <- twentytwomeasures[,c(1,2,3,30,34,38,43,62,66,70,72,76,78,84,89,111,117,125,127,150,156,162,164,175,179,187,190,209,211,213,246)]
nineteen <- nineteenmeasures[,c(1,2,3,11,15,19,24,31,35,39,41,45,47,53,58,68,74,82,84,100,104,110,112,121,125,133,136,140,142,144,159)]

#turning first row to column names
twentytwo <- janitor::row_to_names(twentytwo, 1, remove_rows_above = FALSE)
nineteen <- janitor::row_to_names(nineteen, 1, remove_rows_above = FALSE)

#make numeric, drop water violation column, drop provider and mental health ratio, county, state, and fips
twentytwo_numeric <- twentytwo[c(-1,-2,-3, -17, -18, -29)]
#twentytwo_numeric <- na.omit(twentytwo_numeric)
#twentytwo_numeric<- as.numeric(unlist(twentytwo_numeric))
#twentytwo_numeric <- scale(twentytwo_numeric)
#str(twentytwo_numeric)


#hierarchical clustering
dist_twentytwo <- dist(twentytwo_numeric, method = 'euclidean')
hc_twentytwo <- hclust(dist_twentytwo)
clusters_k4 <- cutree(hc_twentytwo, k = 4)
twentytwo_k4_complete <- mutate(twentytwo_numeric, cluster = clusters_k4)
count(twentytwo_k4_complete, cluster)

#make twentytwo_k4_complete numeric
#twentytwo_k4_complete <- na.omit(twentytwo_k6_complete)
twentytwo_k4_complete<- as.data.frame(apply(twentytwo_k4_complete, 2, as.numeric))
sapply(twentytwo_k4_complete, class) 
#twentytwo_k6_complete <- as.numeric(unlist(twentytwo_k6_complete))
#twentytwo_k6_complete<- scale(twentytwo_k6_complete)
str(twentytwo_k6_complete)

#more hierarchical
hc_twentytwo_complete <- hclust(dist_twentytwo, method = 'complete')
hc_twentytwo_single <- hclust(dist_twentytwo, method = 'single')
hc_twentytwo_average <- hclust(dist_twentytwo, method = 'average')

#dendrograms
plot(hc_twentytwo_complete, main = 'Complete Linkage')
plot(hc_twentytwo_single, main = 'Single Linkage')
plot(hc_twentytwo_average, main = 'Average Linkage')

#dendrogram object and colored dendrogram at height 3000
dend_twentytwo <- as.dendrogram(hc_twentytwo)
plot(dend_twentytwo)
dend_1500 <- color_branches(dend_twentytwo, h = 1500)
plot(dend_1500)

#mean for each category/cluster
mean <- twentytwo_k4_complete %>% 
  group_by(cluster) %>% 
  summarize_all(list(mean))
                           

#kmeans, not working, NAs in data
model_km <- kmeans(twentytwo_numeric, centers = 2)
