library(rgdal)
library(rgeos)
library(tidyverse)
library(tigris)
library(ggplot2)
library(geosphere)

# A tract-by-tract summary leaflet map

# Poverty
# Unemployment
# Health insurance
# Age of buildings (Prior to 1940)
# Labor force in the county
# Average compensation in the county


# 1/2 mile
# 1 mile
# 1.5 miles
# 2 miles

nj_h <- tracts("NJ", county="Hudson", cb=F)


nj_h_centroids <-  SpatialPointsDataFrame(gCentroid(nj_h, byid=TRUE), 
                                          nj_h@data, match.ID=FALSE)

nj_h_centroids <- as.data.frame(nj_h_centroids)
nj_h_centroids <- select(nj_h_centroids, GEOID, x, y)

nj_h_centroids$distance_proj1 <- 0
nj_h_centroids$distance_proj2 <- 0

for (i in 1:nrow(nj_h_centroids)) {
  nj_h_centroids$distance_proj1[i] <- distm (c(40.734330, -74.063644), c(nj_h_centroids$y[i], nj_h_centroids$x[i]), fun = distHaversine)[,1]/ 1609
  nj_h_centroids$distance_proj2[i] <- distm (c(40.72, -74.03577), c(nj_h_centroids$y[i], nj_h_centroids$x[i]), fun = distHaversine)[,1]/ 1609
}

proj1_half <- filter(nj_h_centroids, distance_proj1 <=.5)


###


hudson_unemployment_2013 <- get_acs(geography="tract", endyear=2013, variables= c("B23025_005E", "B23025_002E"), county = "Hudson", state="NJ")
hudson_unemployment_2013$moe <- NULL

hudson_unemployment_2013 <- spread(hudson_unemployment_2013,variable, estimate )
hudson_unemployment_2013$per_un <- round(hudson_unemployment_2013[,4]/hudson_unemployment_2013[,3]*100,2)
hudson_unemployment_2013 <- filter(hudson_unemployment_2013, GEOID!="34017006900" & GEOID!="34017980100")

proj1 <- c("34017001900", "34017004600", "34017005300", "34017006600", "34017006700", "34017007100")

hudson_unemployment_2013_sm <- filter(hudson_unemployment_2013, GEOID %in% proj1)

colnames(hudson_unemployment_2013_sm) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2013_sm$un_rate <- hudson_unemployment_2013_sm$unemp_rate$B23025_005
hudson_unemployment_2013_sm$unemp_rate <- NULL

hudson_unemployment_2013_sm$radius <- "project defined"

proj1_half <- filter(nj_h_centroids, distance_proj1 <=.5)
proj1_half <- proj1_half$GEOID

hudson_unemployment_2013_sm_half <- filter(hudson_unemployment_2013, GEOID %in% proj1_half)
colnames(hudson_unemployment_2013_sm_half) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2013_sm_half$un_rate <- hudson_unemployment_2013_sm_half$unemp_rate$B23025_005
hudson_unemployment_2013_sm_half$unemp_rate <- NULL

hudson_unemployment_2013_sm_half$radius <- "half a mile"

proj1_one <- filter(nj_h_centroids, distance_proj1 <=1)
proj1_one <- proj1_one$GEOID

hudson_unemployment_2013_sm_one <- filter(hudson_unemployment_2013, GEOID %in% proj1_one)
colnames(hudson_unemployment_2013_sm_one) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2013_sm_one$un_rate <- hudson_unemployment_2013_sm_one$unemp_rate$B23025_005
hudson_unemployment_2013_sm_one$unemp_rate <- NULL

hudson_unemployment_2013_sm_one$radius <- "one mile"

proj1_two <- filter(nj_h_centroids, distance_proj1 <=2)
proj1_two <- proj1_two$GEOID

hudson_unemployment_2013_sm_two <- filter(hudson_unemployment_2013, GEOID %in% proj1_two)
colnames(hudson_unemployment_2013_sm_two) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2013_sm_two$un_rate <- hudson_unemployment_2013_sm_two$unemp_rate$B23025_005
hudson_unemployment_2013_sm_two$unemp_rate <- NULL

hudson_unemployment_2013_sm_two$radius <- "two miles"

hudson_unemployment_sm <- rbind(hudson_unemployment_2013_sm, hudson_unemployment_2013_sm_half, hudson_unemployment_2013_sm_one, hudson_unemployment_2013_sm_two)