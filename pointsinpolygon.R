library(readr)
library(lubridate)
library(dplyr)
library(stringr)


library(tidycensus)
library(tidyverse)
source("keys.R")
source("theme_nothing.R")
library(tigris)
#library(sf)
library(viridis)
library(ggmap)
library(leaflet)
library(DT)
police_calls <- read_csv("data/jcpd-all.csv")
shootings <- read_csv("data/jerseycity2015shootings-use.csv")

nj_h <- tracts("NJ", county="Hudson", cb=F)
nj_hf <- fortify(nj_h, region="GEOID")


coords <- police_calls[c("LONGITUDE", "LATITUDE")]
sp <- SpatialPoints(coords)

coords2 <- shootings[c("longitude", "latitude")]
sp2 <- SpatialPoints(coords2)

plot(nj_h)
plot(sp, col="red", add=TRUE)
plot(sp2, col="blue", add=TRUE)

shootings$datetime <- mdy_hm(shootings$datetime)


police_calls$datetime <- mdy_hm(police_calls$`Time Received`)

police_calls$blah <- ifelse(is.na(police_calls$datetime),police_calls$`Time Received`, "" )

police_calls$datetime2 <- mdy_hms(police_calls$blah)

police_calls$datetime3 <- ifelse(is.na(police_calls$datetime),as.character(police_calls$datetime2), as.character(police_calls$datetime))
police_calls$datetime3 <- ymd_hms(police_calls$datetime3)
police_calls$year <- year(police_calls$datetime3)
police_calls_2015 <- subset(police_calls, year==2015)

police_calls_2015 <- police_calls_2015 [!duplicated(police_calls_2015$`Event Number`),]

code2015 <- police_calls_2015 %>% group_by(`Call Code Description`, Priority) %>% summarize(total=n()) %>% arrange(desc(total))
code2015 <- police_calls_2015 %>% group_by(`Call Code Description`) %>% summarize(total=n()) %>% arrange(desc(total))
callcode <- police_calls_2015 %>% group_by(CALLCODE) %>% summarize(total=n()) %>% arrange(desc(total))

write.csv(code2015, "codes2015.csv")
write.csv(callcode, "callcode.csv")


prio1_2015 <- subset(police_calls_2015, Priority==1)
prio1_2015 <- unique(prio1_2015$`Event Number`)

calls <- c("BURG ALARM COMMERCIAL PROP", "ASSAULT NO WEAPON", "USE/SALE OF DRUGS", "THEFT PROP FROM VEHICLE", "MOTOR VEHICLE THEFT", "THEFT FROM PERSON",
           "ROB COMERCIAL", "FOUND CDS/PARAPHERNALIA", "HOMICIDE")

police_calls_2015_sub <- police_calls_2015 %>%
  filter(str_detect(`Call Code Description`, "BURG ALARM COMMERCIAL PROP|ASSAULT NO WEAPON|USE/SALE OF DRUGS|THEFT PROP FROM VEHICLE|MOTOR VEHICLE THEFT|THEFT FROM PERSON|ROB COMERCIAL|FOUND CDS/PARAPHERNALIA|HOMICIDE"))

police_calls_2015_sub$type <- gsub(";.*", "", police_calls_2015_sub$`Call Code Description`)
police_calls_2015_sub <- select(police_calls_2015_sub, type, date=datetime3, lat=LATITUDE, lon=LONGITUDE)
shootings_2015_sub <- shootings
shootings_2015_sub$shooting_type <- paste("Shooting:", shootings_2015_sub$shooting_type)
shootings_2015_sub <- select(shootings, type=shooting_type, date=datetime, lat=latitude, lon=longitude)

sub_2015 <- rbind(police_calls_2015_sub, shootings_2015_sub)

coords <- sub_2015[c("lon", "lat")]

sp <- SpatialPoints(coords)
plot(nj_h)
plot(sp, col="red", add=TRUE)

proj4string(sp) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
proj4string(sp)


by_tract <- over(sp, nj_h)

sub_2015 <- cbind(sub_2015, by_tract)

tract_totals <- sub_2015 %>%
  group_by(GEOID, NAMELSAD, type) %>%
  summarize(total=n())

tract_totals <- tract_totals %>%
  group_by(type) %>%
  mutate(percent=round(total/sum(total, na.rm=T)*100,2))

tract_totals$type <- ifelse(tract_totals$type=="Fatal", "Shooting: Fatal", tract_totals$type)
tract_totals$type <- ifelse(tract_totals$type=="Non-fatal", "Shooting: Non-fatal", tract_totals$type)

tract_totals <- filter(tract_totals, !is.na(GEOID))

proj1 <- c("34017004400", "34017004500", "34017004600", "34017005200", "34017005300", "34017005500", "34017005600", "34017005801", "34017006000", "34017006100", "34017006200", "34017006400", "34017006500", "34017006700", "34017006800", "34017007600")

tract_totals_a  <- filter(tract_totals, GEOID %in% proj1)

shootings_tracts <- filter(tract_totals_a, type=="Shooting: Fatal" | type=="Shooting: Non-fatal")
tract_totals_a <- filter(tract_totals_a, type!="Shooting: Fatal" & type!="Shooting: Non-fatal")

nj_h <- tracts("NJ", county="Hudson", cb=F)
nj_hf <- fortify(nj_h, region="GEOID")


nj_tracts <- left_join(nj_hf, tract_totals_a, by=c("id"="GEOID"))

#nj_tracts <- filter(nj_tracts, !is.na(year))
nj_tracts <- nj_tracts[!is.na(nj_tracts$percent),]

#nj_tracts$year <- gsub("ur_", "", nj_hud$year)

nj_map <- ggplot()
#nj_map <- nj_map + geom_polygon(data=nj_hf, aes(x=long, y=lat, group=group), fill=NA, color="black", size=.1)
nj_map <- nj_map + geom_polygon(data=nj_tracts, aes(x=long, y=lat, group=group, fill=percent), color="black", size=.5)
nj_map <- nj_map + facet_wrap(~type, ncol=2)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_viridis(option = "heat", direction=-1, name = "Percent of crime calls")
nj_map <- nj_map + scale_color_viridis(option = "heat", direction=-1)
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="Percent of crime calls in 2015")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.03577, xend = -74.005, y = 40.72, yend = 40.72, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.03577, y = 40.72, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -73.99217, y = 40.72, label = "65 Bay Street", size=3, colour="gray30") 

print(nj_map)
datatable(tract_totals_a[,2:5])

