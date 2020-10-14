library(RSocrata)
library(sf)
library(dplyr)
library(leaflet)


# open space (parks) -----------
r <- st_read("https://data.cityofnewyork.us/api/geospatial/g84h-jbjm?method=export&format=GeoJSON")  %>% 
  st_simplify(dTolerance = .0001)

r$parknum <- as.character(r$parknum)


# park properties -----------

r1 <- st_read("https://data.cityofnewyork.us/api/geospatial/enfh-gkve?method=export&format=GeoJSON")

# add park properties details to open space file --------

r1$omppropid <- as.character(r1$omppropid)
length(unique(r1$globalid)) == nrow(r1)

r1df<- r1 %>% as.data.frame() %>% select(!geometry)

length(unique(r$source_id)) == nrow(r)
#[1] 12491

rj<- left_join(r,r1df, by = c("parknum"="omppropid")) %>% 
  filter(!duplicated(source_id)) %>% 
  mutate(uid = paste0(source_id, globalid))

# fill in 
rna<-r[which(is.na(rj$park_name)==T),]
# map to see what/where are they
# leaflet() %>%
#   # default settings ---------------
# setView(-73.933560,40.704343, zoom = 10.5) %>%
#   addProviderTiles("CartoDB.Positron") %>% 
#   addPolygons(data = rna, fillOpacity = 0.5, fillColor = "blue",   
#               stroke = T, popup = rna$feat_code)

# get park feature codes -----
# https://github.com/CityOfNewYork/nyc-planimetrics/blob/master/Capture_Rules.md#open-space-attributes

pf <- read.csv("data/park_features.csv", stringsAsFactors = F) %>% 
  janitor::clean_names()
rj$feat_code <- as.integer(as.character(rj$feat_code))
rj <- rj %>% left_join(pf, by = c("feat_code"="feature_code"))

rj[is.na(rj$landuse)==T,]$landuse <- rj[is.na(rj$landuse)==T,]$subtype


# check duplicated ones ------
#rj_dup <- left_join(r,r1df, by = c("parknum"="omppropid")) 
#rj_dup<- rj_dup[duplicated(rj_dup$source_id),]

# list not found in one or the other ------
rdf<- r %>% as.data.frame() %>% select(!geometry)

rj1<- left_join(r1,rdf, by=c("omppropid"="parknum")) %>% 
  filter(!duplicated(globalid)) %>% 
  mutate(uid = paste0(source_id, globalid))

innr <- inner_join(r,r1df, by=c("parknum"="omppropid"))
innr$uid <- paste0(innr$source_id, innr$globalid)
innr<- innr[!duplicated(innr$uid),]

innr1 <- inner_join(r1,rdf, by=c("omppropid"="parknum"))
innr1$uid <- paste0(innr1$source_id, innr1$globalid)
innr1<- innr1[!duplicated(innr1$globalid),]

r_minus_innr <- rj[which(rj$uid%in%innr$uid==F),]
r1_minus_innr1 <- rj1[which(rj1$uid%in%innr1$uid==F),]

# clean
rj$landuse <- gsub("Tracking Only", "Tracking", rj$landuse) 

# waterfront properties ----------

paws <- st_read('https://data.cityofnewyork.us/api/geospatial/6gxz-3w49?method=export&format=GeoJSON')

wpaas <- st_read('https://data.cityofnewyork.us/api/geospatial/6xi4-k8fe?method=export&format=GeoJSON')

wpaas_access <- st_read('https://data.cityofnewyork.us/api/geospatial/jadt-art9?method=export&format=GeoJSON')

wpaas_fp<- st_read("https://data.cityofnewyork.us/api/geospatial/3bx4-hha8?method=export&format=GeoJSON")

public_wf <- st_read('https://data.cityofnewyork.us/api/geospatial/r2c6-8tqb?method=export&format=GeoJSON')

fish <- st_read("https://data.cityofnewyork.us/api/geospatial/dz88-g4k3?method=export&format=GeoJSON")




# park  districts -------

pd <- st_read("https://data.cityofnewyork.us/api/geospatial/mebz-ditc?method=export&format=GeoJSON")

# park sectors

ps <- st_read("https://data.cityofnewyork.us/api/geospatial/t4re-ksn6?method=export&format=GeoJSON")

# park zones

pz <- st_read("https://nycopendata.socrata.com/api/geospatial/4j29-i5ry?method=export&format=GeoJSON")



# park closures during covid


# walk to a park service area ----
wpsa_points <- st_read("https://data.cityofnewyork.us/api/geospatial/5vb5-y6cv?method=export&format=GeoJSON")

wpsa_area<- st_read('https://data.cityofnewyork.us/api/geospatial/rg6q-zak8?method=export&format=GeoJSON')