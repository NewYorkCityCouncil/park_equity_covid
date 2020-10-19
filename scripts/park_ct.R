# Park Equity Tester 

library(leaflet)
library(sf)
library(censusapi)
library(rgeos)
library(ggplot2)
library(dplyr)
library(htmltools)
library(rgdal)
library(raster)
library(stringr)
library(htmlwidgets)

rm(list=ls())

# Access Points

access <- read_sf("data/Walk-to-a-Park Service area/geo_export_077c476d-cadb-41e8-a36d-b5994d952f89.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
shape <- read_sf("data/Walk-to-a-Park Service area/geo_export_f18e18a9-a859-4692-a3e0-48095a41a0d2.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")

# Income Data

ct <- read_sf("data/2010 Census Tracts/geo_export_8d38b305-5fed-49a0-a548-46435c11e818.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")

ct$center <- st_centroid(ct$geometry, of_largest_polygon = TRUE)

income_col <- c("NAME", "GEO_ID", paste0("S1901_C01_0",formatC(1:13, width = 2, flag = "0"), "E")) 

acs5_sub_tract <- getCensus(
  name = "acs/acs5/subject",
  vintage = 2018,
  vars = income_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

pop_col <- c("NAME", "B01003_001E")

acs5_det_tract <- getCensus(
  name = "acs/acs5",
  vintage = 2018,
  vars = pop_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

acs_tract <- merge(acs5_sub_tract, acs5_det_tract[,c("NAME", "B01003_001E")], by="NAME")

acs_tract$boro_code <- 0
for(i in 1:nrow(acs_tract)){ 
if(acs_tract$county[i]=="005") acs_tract$boro_code[i] <- "2"
if(acs_tract$county[i]=="081") acs_tract$boro_code[i] <- "4"
if(acs_tract$county[i]=="061") acs_tract$boro_code[i] <- "1"
if(acs_tract$county[i]=="047") acs_tract$boro_code[i] <- "3"
if(acs_tract$county[i]=="085") acs_tract$boro_code[i] <- "5"
}
acs_tract$boro_ct201 <- paste0(acs_tract$boro_code, acs_tract$tract)

ct_demo <- st_sf(merge(acs_tract, ct, by="boro_ct201"))
ct_demo$S1901_C01_012E <- ifelse(ct_demo$S1901_C01_012E<=0, NA, ct_demo$S1901_C01_012E)
ct_demo$B01003_001E <- ifelse(ct_demo$B01003_001E<=0, NA, ct_demo$B01003_001E)

ct_demo$ins <- ifelse(is.na(over(as_Spatial(ct_demo$center), as_Spatial(shape))$type), 0, 1)


# COVID

### Import and Clean Covid and Crosswalk Data

# Covid data from NYC Health
URL_C19 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/data-by-modzcta.csv"
C19 <- read.csv(URL_C19)
C19$MODZCTA <- as.character(C19$MODIFIED_ZCTA); C19 <- C19[,!(names(C19) %in% "MODIFIED_ZCTA")]

# Crosswalk between census tract and zcta
# https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-zcta-record-layout.html#par_textimage_3
URL_ZCTAtoCT <- "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt?#"
ZCTAtoCT <- read.csv(URL_ZCTAtoCT)
# Only keep NYC
ZNYC <- subset(ZCTAtoCT, STATE=="36" & (COUNTY=="05" | COUNTY=="5" | COUNTY=="47" | COUNTY=="81" | COUNTY=="85" | COUNTY=="61"))

# Crosswalk between zcta and modzcta
URL_MZtoZ <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/ZCTA-to-MODZCTA.csv"
MZtoZ <- read.csv(URL_MZtoZ)
MZtoZ <- MZtoZ %>% rename(ZCTA5 = ZCTA)
MZtoZ[,1:2] <- lapply(MZtoZ[,1:2], as.character)

### Crosswalk between census tract and zcta (ZNYC)

ZNYC <- ZNYC %>% rename(tract = TRACT)
ZNYC$tract <- as.character(as.numeric(ZNYC$tract/100))
ZNYC$boro_code <- str_sub(ZNYC$GEOID,-8,-7) #strip boro code from GEOID


for(i in 1:nrow(ZNYC)){ 
  if(ZNYC$boro_code[i]=="05") ZNYC$boro_code[i] <- "2"
  if(ZNYC$boro_code[i]=="81") ZNYC$boro_code[i] <- "4"
  if(ZNYC$boro_code[i]=="61") ZNYC$boro_code[i] <- "1"
  if(ZNYC$boro_code[i]=="47") ZNYC$boro_code[i] <- "3"
  if(ZNYC$boro_code[i]=="85") ZNYC$boro_code[i] <- "5"
}
ZNYC$boro_ct201 <- paste0(ZNYC$boro_code, substr(ZNYC$GEOID, 6, 11))

########################################################################

# https://github.com/nychealth/coronavirus-data/tree/master/Geography-resources
nyczipjson <- read_sf("data/MODZCTA_2010/MODZCTA_2010.shp") %>% 
  st_transform("+proj=longlat +datum=WGS84")
map_sf_zip <- st_sf(merge(nyczipjson, C19, by = "MODZCTA"))

# Covid
map_sf_zip$numct <- 0
for (i in unique(map_sf_zip$MODZCTA)){
  map_sf_zip[which(map_sf_zip$MODZCTA==i), "numct"] <- length(which(!is.na(over(as_Spatial(ct_demo$center), as_Spatial(subset(map_sf_zip, MODZCTA==i)))$MODZCTA)))
}

########################################################################

# 10-Min Walk buffers

iso <- readOGR("data/isochrones_10min_accesspts.geojson")
sqft_pts <- readOGR("data/sf_access.geojson")

iso@data$squareft <- sqft_pts@data$squareft
iso@data$park_name <- sqft_pts@data$park_name
iso@data$parknum <- sqft_pts@data$parknum
iso@data$parkid <- ifelse(!is.na(iso@data$park_name), as.character(iso@data$park_name), 
                     ifelse(!is.na(iso@data$parknum), as.character(iso@data$parknum), 
                            as.character(iso@data$parkname)))
iso@data <- subset(iso@data, !is.na(squareft))

#drop squareft NA

# map_iso <- leaflet() %>%
#   setView(-73.935242,40.730610,10) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data=iso, weight=1) %>%
#   addCircles(data=ct_demo$center, 
#              group= "Center") %>%
#   addCircles(data=access, 
#              group= "Access", 
#              color="red")
# map_iso

# test <- subset(iso, is.na(parkname))
# 
# map_test <- leaflet() %>%
#   setView(-73.935242,40.730610,10) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data=test, weight=0.1) %>%
#   addCircles(data=ct_demo$center,
#              group= "Center",
#              popup = lapply(labels,HTML)) %>%
#   addCircles(data=subset(access, is.na(parkname) & is.na(gispropnum)),
#              group= "Access",
#              color="red")
# map_test
# sort(ct_walk[which(ct_walk$`East River Park`==1),]$tract)



ct_walk <- ct_demo

parknames <- unique(as.character(subset(iso, !is.na(iso@data$parkid))$parkid))

# create column for each park
ct_walk[, parknames] <- 0 

for (i in parknames){
  temp_cens <- ct_demo[0,]
  # collect the access points for each park
  for (j in rownames(iso@data[which(iso@data$parkid==i),])){
    # rows off by 1 for some reason
    temp_sp <- SpatialPolygons(list(iso@polygons[[as.numeric(j)+1]])) 
    # make same crs
    crs(temp_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
    # create list of all census tracts within iso of access points to parkname (i)
    temp_cens <- rbind(temp_cens, ct_demo[!is.na(over(as_Spatial(ct_demo$center), temp_sp)),])
  }
  # each column says whether or not the census tract has access to the column name park
  nam <- i
  ct_walk[, nam] <- ifelse(ct_walk$NAME %in% temp_cens$NAME, unique(subset(iso@data, parkid==nam)$squareft), 0)
}

# do squareft / park
ct_walk$parktot <- round(rowSums(st_drop_geometry(ct_walk[,35:1254]), na.rm=TRUE), 2)
ct_walk$parktotpc <- round(ct_walk$parktot / ct_walk$B01003_001E, 2)

# st_write(ct_walk[, c(1:6,18,20,23,28,29,30,31,1262,1263)], 
#          "data/ct_walk.geojson",
#          driver='GeoJSON', delete_dsn=TRUE)
#
# writeOGR(as(ct_walk[, c(1:6,18,20,23,28,29,30,31,1262,1263)], 'Spatial'), dsn="data/ct_walk",
#          "ct_walk", driver="ESRI Shapefile", overwrite_layer =TRUE)
# 
# zip(zipfile = 'data/ct_walk', files = dir('data/ct_walk/', full.names = TRUE))

labels_sqft <- paste("<h3>","Name: ",ct_walk$NAME, "</h3>",
                "<p>",paste0("Tract: ",ct_walk$tract),"</p>", 
                "<p>",paste0("Median Income: ",ct_walk$S1901_C01_012E),"</p>",  
                "<p>",paste0("Population: ",ct_walk$B01003_001E),"</p>", 
                "<p>","COVID19 Case Rate: ",map_sf_zip$COVID_CASE_RATE,"</p>", 
                "<p>","Neighborhood: ",map_sf_zip$NEIGHBORHOOD_NAME,"</p>",
                "<p>","sqft: ",round(ct_walk$parktot, 0),"</p>", 
                "<p>","sqft per capita: ",round(ct_walk$parktotpc, 0),"</p>")

map_sqft <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = ct_walk$parktot, n=9)(ct_walk$parktot),
              fillOpacity = 0.5, 
              group = "Square Footage", 
              popup = lapply(labels_sqft,HTML)) %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = ct_walk$parktotpc, n=9)(ct_walk$parktotpc),
              fillOpacity = 0.5,
              group = "Square Footage Per Capita", 
              popup = lapply(labels_sqft,HTML)) %>%
  addLayersControl(
    overlayGroups = c("Square Footage", "Square Footage Per Capita"),
    options = layersControlOptions(collapsed = FALSE))%>% 
  hideGroup("Square Footage")

map_sqft

saveWidget(map_sqft, file = "map_sqft.html")

########################################################################

# MODZCTA aggregation

# Aggregate census sqft up to MODZCTA
Z_sqft <- merge(ZNYC, ct_walk[,c("boro_ct201", "parktot", "parktotpc")], by="boro_ct201")

# zcta sqft is weighted average of sqft in each nested tract 
for (i in unique(Z_sqft$ZCTA5)){
  Z_sqft[Z_sqft$ZCTA5==i,"Z_sqft"] <- sum(Z_sqft[Z_sqft$ZCTA5==i,"parktot"] * 
                                     Z_sqft[Z_sqft$ZCTA5==i,"ZPOPPCT"]/
                                     (sum(Z_sqft[which(!is.na(Z_sqft$parktot) & Z_sqft$ZCTA5==i),"ZPOPPCT"]))
                                   , na.rm=TRUE)
  Z_sqft[Z_sqft$ZCTA5==i,"Z_sqft"] <- ifelse(Z_sqft[Z_sqft$ZCTA5==i,"Z_sqft"]==0, 
                                             Z_sqft[Z_sqft$ZCTA5==i,"parktot"], 
                                             Z_sqft[Z_sqft$ZCTA5==i,"Z_sqft"])
  Z_sqft[Z_sqft$ZCTA5==i,"Pop_Add"] <- sum(Z_sqft[Z_sqft$ZCTA5==i,"POPPT"])
}

Pop_MZtoZ <- unique(merge(MZtoZ, Z_sqft[,c("ZCTA5", "Pop_Add", "Z_sqft")], by="ZCTA5"))

for (j in Pop_MZtoZ$MODZCTA){
  Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"sqft"] <- sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Z_sqft"] * 
                                        Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"]/
                                        (sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"]))
                                      , na.rm=TRUE)
  Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add_MODZCTA"] <- sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"])
}

sqft_mzcta <- unique(st_sf(merge(map_sf_zip, Pop_MZtoZ, by = "MODZCTA")))
sqft_mzcta$sqftpc <- ifelse(sqft_mzcta$Pop_Add_MODZCTA!=0, 
                            sqft_mzcta$sqft / sqft_mzcta$Pop_Add_MODZCTA, 
                            NA)
sqft_mzcta<- st_sf(sqft_mzcta)

########################################################################

# MAP

# Demographics and Within 10-min Walk Overlay

labels_census <- paste("<h3>","Name: ",ct_walk$NAME, "</h3>",
                "<p>",paste0("Tract: ",ct_walk$tract),"</p>", 
                "<p>",paste0("Median Income: ",ct_walk$S1901_C01_012E),"</p>",  
                "<p>",paste0("Population: ",ct_walk$B01003_001E),"</p>", 
                "<p>","COVID19 Case Rate: ",map_sf_zip$COVID_CASE_RATE,"</p>", 
                "<p>","MODZCTA: ",map_sf_zip$MODZCTA,"</p>", 
                "<p>","Neighborhood: ",map_sf_zip$NEIGHBORHOOD_NAME,"</p>", 
                "<p>","Square feet (tract): ",round(ct_walk$parktot, 0),"</p>", 
                "<p>","Square feet per capita (tract): ",round(ct_walk$parktotpc, 0),"</p>",
                "<p>","Square feet (MODZCTA): ",round(sqft_mzcta$sqft, 0),"</p>", 
                "<p>","Square feet per capita (MODZCTA): ",round(sqft_mzcta$sqftpc, 0),"</p>")

labels_modzcta <- paste("<h3>","MODZCTA: ",map_sf_zip$MODZCTA,"</h3>", 
                       "<p>",paste0("Population: ",sqft_mzcta$Pop_Add_MODZCTA),"</p>", 
                       "<p>","COVID19 Case Rate: ",map_sf_zip$COVID_CASE_RATE,"</p>", 
                       "<p>","Neighborhood: ",map_sf_zip$NEIGHBORHOOD_NAME,"</p>",
                       "<p>","# Census Tracts in MODZCTA: ",map_sf_zip$numct,"</p>", 
                       "<p>","Square feet (MODZCTA): ",round(sqft_mzcta$sqft, 0),"</p>", 
                       "<p>","Square feet per capita (MODZCTA): ",round(sqft_mzcta$sqftpc, 0),"</p>")

map <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = ct_walk$S1901_C01_012E)(ct_walk$S1901_C01_012E),
              fillOpacity = 0.5,
              group = "Tract Income", 
              popup = lapply(labels_census,HTML)) %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = ct_walk$B01003_001E)(ct_walk$B01003_001E),
              fillOpacity = 0.5,
              group = "Tract Population", 
              popup = lapply(labels_census,HTML)) %>%
  addPolygons(data=map_sf_zip,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = map_sf_zip$COVID_CASE_RATE)(map_sf_zip$COVID_CASE_RATE),
              fillOpacity = 0.5,
              group = "COVID Case Rate", 
              popup = lapply(labels_modzcta,HTML)) %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = ct_walk$parktot)(ct_walk$parktot),
              fillOpacity = 0.5,
              group = "Tract sqft",
              popup = lapply(labels_census,HTML)) %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = ct_walk$parktotpc)(ct_walk$parktotpc),
              fillOpacity = 0.5,
              group = "Tract sqft per capita",
              popup = lapply(labels_census,HTML)) %>%
  addPolygons(data=sqft_mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = sqft_mzcta$sqft)(sqft_mzcta$sqft),
              fillOpacity = 0.5,
              group = "MODZCTA sqft", 
              popup = lapply(labels_modzcta,HTML)) %>%
  addPolygons(data=sqft_mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = sqft_mzcta$sqftpc)(sqft_mzcta$sqftpc),
              fillOpacity = 0.5,
              group = "MODZCTA sqft per capita", 
              popup = lapply(labels_modzcta,HTML)) %>%
  addPolygons(data=shape, 
              weight=1, 
              group= "Walk") %>%
  addCircles(data=ct_demo$center, 
             group= "Center") %>%
  addCircles(data=access, 
             group= "Access", 
             color="red") %>%
  addCircles(data=subset(ct_demo, ins==0)$center, 
             group= "Not Walkable", 
             color="black") %>%
  addLayersControl(
    overlayGroups = c("Walk", "Tract Income", "Tract Population", "Access", "Center", "Not Walkable", "COVID Case Rate", 
                      "Tract sqft", "Tract sqft per capita", "MODZCTA sqft", "MODZCTA sqft per capita"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Walk") %>% 
  hideGroup("Tract Income") %>% 
  hideGroup("Tract Population") %>% 
  hideGroup("Access") %>% 
  hideGroup("Center") %>% 
  hideGroup("Not Walkable") %>% 
  hideGroup("COVID Case Rate") %>% 
  hideGroup("Tract sqft") %>% 
  hideGroup("MODZCTA sqft") %>% 
  hideGroup("MODZCTA sqft per capita") 
map

saveWidget(map, file = "map.html")

########################################################################

ggplot(ct_demo,aes(x=S1901_C01_012E)) + 
  geom_histogram(data=subset(ct_demo,ins == 0),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(ct_demo,ins == 1),fill = "blue", alpha = 0.2) +
  theme_minimal() + 
  labs(
    x= "Median Income", 
    y= "Number of Census Tracts",
    title = "Income Distribution by Walkable to Park"
  ) + 
  facet_wrap(~boro_name)

ggplot(ct_demo,aes(x=B01003_001E)) + 
  geom_histogram(data=subset(ct_demo,ins == 0),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(ct_demo,ins == 1),fill = "blue", alpha = 0.2) +
  theme_minimal() + 
  labs(
    x= "Population", 
    y= "Number of Census Tracts",
    title = "Population Distribution by Walkable to Park"
  ) + 
  facet_wrap(~boro_name)


# Census tract summary data
ct_boro_total <- ct_demo %>%
  st_drop_geometry() %>%
  group_by(boro_name) %>%
  summarise(total=n())

ct_boro_ins <- subset(ct_demo, ins==0) %>%
  st_drop_geometry() %>%
  group_by(boro_name) %>%
  summarise(outside=n())

ct_boro <- merge(ct_boro_total, ct_boro_ins, by="boro_name")
ct_boro$perc_ct <- ct_boro$outside/ct_boro$total
ct_boro

# Population summary data
pop_boro_total <- ct_demo %>%
  st_drop_geometry() %>%
  group_by(boro_name) %>%
  summarise(total=sum(B01003_001E, na.rm=TRUE))

pop_boro_ins <- subset(ct_demo, ins==0) %>%
  st_drop_geometry() %>%
  group_by(boro_name) %>%
  summarise(outside=sum(B01003_001E, na.rm=TRUE))

pop_boro <- merge(pop_boro_total, pop_boro_ins, by="boro_name")
pop_boro$perc_pop <- pop_boro$outside/pop_boro$total
pop_boro


########################################################################

# take highest covid level areas and look at their relative access to parks
# side by side map and plot of covid and sqftpc


mzcta <- merge(map_sf_zip, st_drop_geometry(unique(sqft_mzcta[,c("MODZCTA", "sqft", "sqftpc")])), by="MODZCTA")

#write.csv(mzcta, "data/mzcta")

ggplot(mzcta, aes(x=log(sqftpc), y=COVID_CASE_RATE, color=BOROUGH_GROUP)) + geom_point() 

ggplot(mzcta, aes(x=log(sqftpc), y=COVID_CASE_RATE, color=BOROUGH_GROUP)) + geom_point() + facet_wrap(~BOROUGH_GROUP)
cor(log(mzcta$sqftpc), mzcta$COVID_CASE_RATE)

ggplot(mzcta, aes(x=rank(sqftpc), y=COVID_CASE_RATE, color=BOROUGH_GROUP)) + geom_point() + facet_wrap(~BOROUGH_GROUP)
cor(rank(mzcta$sqftpc), mzcta$COVID_CASE_RATE)

ggplot(mzcta, aes(x=log(sqftpc), y=COVID_DEATH_RATE, color=BOROUGH_GROUP)) + geom_point() + facet_wrap(~BOROUGH_GROUP)
cor(log(mzcta$sqftpc), mzcta$COVID_DEATH_RATE)

ggplot(mzcta, aes(x=rank(sqftpc), y=COVID_DEATH_RATE, color=BOROUGH_GROUP)) + geom_point() + facet_wrap(~BOROUGH_GROUP)
cor(rank(mzcta$sqftpc), mzcta$COVID_DEATH_RATE)
