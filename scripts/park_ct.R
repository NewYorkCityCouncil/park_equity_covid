# Park Equity

########################################################################

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
library(RSocrata)
library(ggpubr)

########################################################################

############ Import and Clean Data

rm(list=ls())

# Walk-to-a-Park Access Points
# https://data.cityofnewyork.us/Recreation/Walk-to-a-Park-Service-area/5vb5-y6cv
access <- read_sf("data/Walk-to-a-Park Service area/geo_export_077c476d-cadb-41e8-a36d-b5994d952f89.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
# Walk-to-a-Park "walkable" shapefile 
shape <- read_sf("data/Walk-to-a-Park Service area/geo_export_f18e18a9-a859-4692-a3e0-48095a41a0d2.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")

# Census tracts shapefile 
ct <- read_sf("data/2010 Census Tracts/geo_export_8d38b305-5fed-49a0-a548-46435c11e818.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")

# Create center point of census tract
ct$center <- st_centroid(ct$geometry, of_largest_polygon = TRUE)

########################################################################

# ACS Income and Population Data

# Income data to pull from ACS
income_col <- c("NAME", "GEO_ID", paste0("S1901_C01_0",formatC(1:13, width = 2, flag = "0"), "E")) 

# Note: you must get a census API key to use these functions
# Income data at census tract level
acs5_sub_tract <- getCensus(
  name = "acs/acs5/subject",
  vintage = 2018,
  vars = income_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

# Income data at zip code (zcta5) level
acs5_sub_zip <- getCensus(
  name = "acs/acs5/subject",
  vintage = 2018,
  vars = income_col, 
  region = "zip code tabulation area:*")
acs5_sub_zip$ZCTA5 <- acs5_sub_zip$zip_code_tabulation_area

# Population data to pull from ACS
pop_col <- c("NAME", "B01003_001E")

acs5_det_tract <- getCensus(
  name = "acs/acs5",
  vintage = 2018,
  vars = pop_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

# Combine income and population data for census tract
acs_tract <- merge(acs5_sub_tract, acs5_det_tract[,c("NAME", "B01003_001E")], by="NAME")

# Make consistent boro code designations
acs_tract$boro_code <- 0
for(i in 1:nrow(acs_tract)){ 
if(acs_tract$county[i]=="005") acs_tract$boro_code[i] <- "2"
if(acs_tract$county[i]=="081") acs_tract$boro_code[i] <- "4"
if(acs_tract$county[i]=="061") acs_tract$boro_code[i] <- "1"
if(acs_tract$county[i]=="047") acs_tract$boro_code[i] <- "3"
if(acs_tract$county[i]=="085") acs_tract$boro_code[i] <- "5"
}
acs_tract$boro_ct201 <- paste0(acs_tract$boro_code, acs_tract$tract)

# Merge shapefile and income / population data
ct_demo <- st_sf(merge(acs_tract, ct, by="boro_ct201"))
ct_demo$S1901_C01_012E <- ifelse(ct_demo$S1901_C01_012E<=0, NA, ct_demo$S1901_C01_012E)
ct_demo$B01003_001E <- ifelse(ct_demo$B01003_001E<=0, NA, ct_demo$B01003_001E)

########################################################################

# COVID data

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

# Make consistent boro code designations
for(i in 1:nrow(ZNYC)){ 
  if(ZNYC$boro_code[i]=="05") ZNYC$boro_code[i] <- "2"
  if(ZNYC$boro_code[i]=="81") ZNYC$boro_code[i] <- "4"
  if(ZNYC$boro_code[i]=="61") ZNYC$boro_code[i] <- "1"
  if(ZNYC$boro_code[i]=="47") ZNYC$boro_code[i] <- "3"
  if(ZNYC$boro_code[i]=="85") ZNYC$boro_code[i] <- "5"
}
ZNYC$boro_ct201 <- paste0(ZNYC$boro_code, substr(ZNYC$GEOID, 6, 11))

# MODZCTA Shapefile
# https://github.com/nychealth/coronavirus-data/tree/master/Geography-resources
nyczipjson <- read_sf("data/MODZCTA_2010/MODZCTA_2010.shp") %>% 
  st_transform("+proj=longlat +datum=WGS84")
map_sf_zip <- st_sf(merge(nyczipjson, C19, by = "MODZCTA"))

# Count number of census tracts in each MODZCTA
map_sf_zip$numct <- 0
for (i in unique(map_sf_zip$MODZCTA)){
  map_sf_zip[which(map_sf_zip$MODZCTA==i), "numct"] <- length(which(!is.na(over(as_Spatial(ct_demo$center), as_Spatial(subset(map_sf_zip, MODZCTA==i)))$MODZCTA)))
}

########################################################################

# Check which census tracts are inside the "walkable" area shapefile
ct_demo$ins <- ifelse(is.na(over(as_Spatial(ct_demo$center), as_Spatial(shape))$type), 0, 1)

# Import 10-Min Walk buffers made by isochrones.R
iso <- readOGR("data/isochrones_10min_accesspts.geojson")
# Read shapefile from park_universe.R
sqft_pts <- readOGR("data/sf_access.geojson")

# Link iso dataset to park_universe variables
iso@data$squareft <- sqft_pts@data$squareft
iso@data$park_name <- sqft_pts@data$park_name
iso@data$parknum <- sqft_pts@data$parknum
# Try park_name first, then parknum, then parkname
iso@data$parkid <- ifelse(!is.na(iso@data$park_name), as.character(iso@data$park_name), 
                     ifelse(!is.na(iso@data$parknum), as.character(iso@data$parknum), 
                            as.character(iso@data$parkname)))
# Drop rows with squareft NA's 
# iso <- subset(iso, !is.na(squareft))

########################################################################

# Create ct_walk from ct_demo (income and population data at census tract)
ct_walk <- ct_demo

# Create list of non-NA park ID's
parknames <- unique(as.character(subset(iso, !is.na(squareft))$parkid))

# Create column for each park 
ct_walk[, parknames] <- 0 

# For each park ID
for (i in parknames){
  temp_cens <- ct_demo[0,]
  # Collect the access points for each park
  for (j in rownames(iso@data[which(iso@data$parkid==i),])){
    # Note: Slot "ID" diff by 1
    temp_sp <- SpatialPolygons(list(iso@polygons[[as.numeric(j)+1]])) 
    # Make same crs
    crs(temp_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
    # Create list of all census tracts within iso of access points to parkname (i)
    temp_cens <- rbind(temp_cens, ct_demo[!is.na(over(as_Spatial(ct_demo$center), temp_sp)),])
  }
  # each column says whether or not the census tract has access to the column name park and the square footage of that park if it has access
  nam <- i
  ct_walk[, nam] <- ifelse(ct_walk$NAME %in% temp_cens$NAME, unique(subset(iso@data, parkid==nam)$squareft), 0)
}

# Find total park square footage accessible by each census tract
ct_walk$parktot <- round(rowSums(st_drop_geometry(ct_walk[,35:1254]), na.rm=TRUE), 2)
# Find per capita park square footage accessible by each census tract by dividing by ACS population data
ct_walk$parktotpc <- round(ct_walk$parktot / ct_walk$B01003_001E, 2)

# Save ct_walk files and zip ct_walk
# st_write(ct_walk[, c(1:6,18,20,23,28,29,30,31,1262,1263)], 
#          "data/ct_walk.geojson",
#          driver='GeoJSON', delete_dsn=TRUE)
#
# writeOGR(as(ct_walk[, c(1:6,18,20,23,28,29,30,31,1262,1263)], 'Spatial'), dsn="data/ct_walk",
#          "ct_walk", driver="ESRI Shapefile", overwrite_layer =TRUE)
# 
# zip(zipfile = 'data/ct_walk', files = dir('data/ct_walk/', full.names = TRUE))

########################################################################

# Aggregate census tract data to MODZCTA for comparisons with COVID19 data

# Merge census tract to ZCTA crosswalk with ct_walk data
Z_sqft <- merge(ZNYC, ct_walk[,c("boro_ct201", "parktot", "parktotpc")], by="boro_ct201")

# ZCTA sqft is weighted average of sqft in each nested census tract 
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

# Merge ZCTA to MODZCTA crosswalk with Z_sqft data
Pop_MZtoZ_sqft <- unique(merge(MZtoZ, Z_sqft[,c("ZCTA5", "Pop_Add", "Z_sqft")], by="ZCTA5"))
Pop_MZtoZ <- unique(merge(Pop_MZtoZ_sqft, acs5_sub_zip[,c("ZCTA5", "S1901_C01_012E")], by="ZCTA5"))
Pop_MZtoZ$S1901_C01_012E <- ifelse(Pop_MZtoZ$S1901_C01_012E<=0, NA, Pop_MZtoZ$S1901_C01_012E)


# MODZCTA sqft is weighted average of sqft in each nested ZCTA
for (j in Pop_MZtoZ$MODZCTA){
  Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"sqft"] <- sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Z_sqft"] * 
                                        Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"]/
                                        (sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"]))
                                      , na.rm=TRUE)
  Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add_MODZCTA"] <- sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"])
  Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"MedInc"] <- sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"S1901_C01_012E"] * 
                                              Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"]/
                                              (sum(Pop_MZtoZ[Pop_MZtoZ$MODZCTA==j,"Pop_Add"]))
                                            , na.rm=TRUE)
}

# Merge MODZCTA sqft data with MODZCTA shapefile
sqft_mzcta <- unique(st_sf(merge(map_sf_zip, Pop_MZtoZ, by = "MODZCTA")))
# Calculate square feet per capita for each MODZCTA
sqft_mzcta$sqftpc <- ifelse(sqft_mzcta$Pop_Add_MODZCTA!=0, 
                            sqft_mzcta$sqft / sqft_mzcta$Pop_Add_MODZCTA, 
                            NA)
sqft_mzcta<- st_sf(sqft_mzcta)

# Keep unique MODZCTA and merge shapefile with square feet, square feet per capita, median income, population
mzcta <- merge(map_sf_zip, st_drop_geometry(unique(sqft_mzcta[,c("MODZCTA", "MedInc", "sqft", "sqftpc", "Pop_Add_MODZCTA")])), by="MODZCTA")

# Rank each MODZCTA across COVID case rate, square feet per capita, and median income
mzcta$rankccr <- rank(-mzcta$COVID_CASE_RATE)
mzcta$ranksqft <- rank(mzcta$sqftpc)
mzcta$rankinc <- rank(mzcta$MedInc)
mzcta$ccrsqft <- mzcta$rankccr + mzcta$ranksqft

#write.csv(st_drop_geometry(mzcta), "data/mzcta.csv")


########################################################################

############ Plots 

# Inside vs. Outside "Walkable" plots 

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

# MODZCTA square feet per capita and COVID case rate plot
ggplot(mzcta, aes(x=rank(sqftpc), y=COVID_CASE_RATE, color=BOROUGH_GROUP)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Zip Code Open Space Access and COVID19 Case Rate By Borough",
    x = "Least to Most Square Feet Per Capita (Rank)",
    y = "COVID19 Case Rate (Per 100,000)", 
    color = "Borough", 
    caption = expression(paste(italic("Source: NYC Health; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP, nrow=1) + 
  theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# MODZCTA square feet per capita and median income plot
ggplot(mzcta, aes(x=rank(sqftpc), y=MedInc, color=BOROUGH_GROUP)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Zip Code Open Space Access and Median Income By Borough",
    x = "Least to Most Square Feet Per Capita (Rank)",
    y = "Median Income ($)", 
    color = "Borough", 
    caption = expression(paste(italic("Source: 2018 ACS 5-Year Estimates; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP, nrow=1) + 
  theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

########################################################################

############ Maps

# Map of all variables at census tract and MODZCTA level

labels_census <- paste("<h3>","Name: ",ct_walk$NAME, "</h3>",
                       "<p>",paste0("Tract: ",ct_walk$tract),"</p>", 
                       "<p>",paste0("Median Income: ",ct_walk$S1901_C01_012E),"</p>",  
                       "<p>",paste0("Population: ",ct_walk$B01003_001E),"</p>",
                       "<p>","Square feet (tract): ",round(ct_walk$parktot, 0),"</p>", 
                       "<p>","Square feet per capita (tract): ",round(ct_walk$parktotpc, 0),"</p>")

labels_modzcta <- paste("<h3>","MODZCTA: ",mzcta$MODZCTA,"</h3>", 
                        "<p>",paste0("Population: ",mzcta$Pop_Add_MODZCTA),"</p>", 
                        "<p>","COVID19 Case Rate: ",mzcta$COVID_CASE_RATE,"</p>", 
                        "<p>","Neighborhood: ",mzcta$NEIGHBORHOOD_NAME,"</p>",
                        "<p>","Median Income: ",mzcta$MedInc,"</p>", 
                        "<p>","# Census Tracts in MODZCTA: ",mzcta$numct,"</p>",
                        "<p>","Square feet (MODZCTA): ",round(mzcta$sqft, 0),"</p>", 
                        "<p>","Square feet per capita (MODZCTA): ",round(mzcta$sqftpc, 0),"</p>")

map <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = ct_walk$S1901_C01_012E)(ct_walk$S1901_C01_012E),
              fillOpacity = 0.5,
              group = "Tract Median Income", 
              popup = lapply(labels_census,HTML)) %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = ct_walk$B01003_001E)(ct_walk$B01003_001E),
              fillOpacity = 0.5,
              group = "Tract Population", 
              popup = lapply(labels_census,HTML)) %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = mzcta$COVID_CASE_RATE)(mzcta$COVID_CASE_RATE),
              fillOpacity = 0.5,
              group = "COVID Case Rate", 
              popup = lapply(labels_modzcta,HTML)) %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = mzcta$MedInc)(mzcta$MedInc),
              fillOpacity = 0.5,
              group = "MODZCTA Median Income", 
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
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = mzcta$sqft)(mzcta$sqft),
              fillOpacity = 0.5,
              group = "MODZCTA sqft", 
              popup = lapply(labels_modzcta,HTML)) %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~colorQuantile("YlOrRd", domain = mzcta$sqftpc)(mzcta$sqftpc),
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
    overlayGroups = c("Walk", "Tract Median Income", "Tract Population", "Access", "Center", "Not Walkable", "COVID Case Rate", 
                      "MODZCTA Median Income", "Tract sqft", "Tract sqft per capita", "MODZCTA sqft", "MODZCTA sqft per capita"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Walk") %>% 
  hideGroup("Tract Median Income") %>% 
  hideGroup("Tract Population") %>% 
  hideGroup("Access") %>% 
  hideGroup("Center") %>% 
  hideGroup("Not Walkable") %>% 
  hideGroup("COVID Case Rate") %>% 
  hideGroup("MODZCTA Median Income") %>% 
  hideGroup("Tract sqft") %>% 
  hideGroup("MODZCTA sqft") %>% 
  hideGroup("MODZCTA sqft per capita") 
map

#saveWidget(map, file = "map.html")

########################################################################

# Map square footage and square footage per capita data at census tract level with PUMA overlay

# Import open space shapfile from park_universe.R
openspace <- readOGR("data/parks_with_sf_matched.geojson")
# Import PUMA shapefile
puma <- readOGR("data/Public Use Microdata Areas (PUMA).geojson")

labels_sqft <- paste("<h3>","Name: ",ct_walk$NAME, "</h3>",
                     "<p>",paste0("Tract: ",ct_walk$tract),"</p>", 
                     "<p>",paste0("Median Income: ",ct_walk$S1901_C01_012E),"</p>",  
                     "<p>",paste0("Population: ",ct_walk$B01003_001E),"</p>",
                     "<p>","sqft: ",round(ct_walk$parktot, 0),"</p>", 
                     "<p>","sqft per capita: ",round(ct_walk$parktotpc, 0),"</p>")

pal_sqft <- colorBin(
  palette = "YlGn",
  domain = ct_walk$parktot,
  bins = ceiling(quantile(ct_walk$parktot, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE))
)
pal_sqftpc <- colorBin(
  palette = "YlGn",
  domain = ct_walk$parktotpc, 
  bins = ceiling(quantile(ct_walk$parktotpc, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE))
)

map_sqft <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # addPolygons(data=ct_walk,
  #             weight = 1,
  #             color = "grey",
  #             fillColor = ~pal_sqft(ct_walk$parktot),
  #             fillOpacity = 0.5, 
  #             group = "Square Footage", 
  #             popup = lapply(labels_sqft,HTML)) %>%
  addPolygons(data=ct_walk,
              weight = 1,
              color = "grey",
              fillColor = ~pal_sqftpc(ct_walk$parktotpc),
              fillOpacity = 0.5,
              group = "Square Footage Per Capita", 
              popup = lapply(labels_sqft,HTML)) %>%
  addPolylines(data=puma, weight=0.5, color="black") %>%
  addPolygons(data=openspace, 
              color="green",
              group = "Open Space") %>%
  # addLegend(pal = pal_sqft, values = ct_walk$parktot,
  #           group = "Square Footage", 
  #           position = "bottomright") %>%
  addLegend(pal = pal_sqftpc, values = ct_walk$parktotpc,
            group = "Square Footage Per Capita",
            position = "bottomright") %>%
  addLayersControl(
    overlayGroups = c("Square Footage Per Capita", "Open Space"),
    options = layersControlOptions(collapsed = FALSE))%>% 
  #  hideGroup("Square Footage") %>% 
  hideGroup("Open Space")

map_sqft

#saveWidget(map_sqft, file = "map_sqft.html")

########################################################################

# Map square feet per captita, COVID case rate, and median income at MODZCTA level

labels_compare <- paste("<h3>","MODZCTA: ",mzcta$MODZCTA,"</h3>", 
                        "<p>",paste0("Population: ",mzcta$Pop_Add_MODZCTA),"</p>", 
                        "<p>","Neighborhood: ",mzcta$NEIGHBORHOOD_NAME,"</p>",
                        "<p>","COVID19 Case Rate: ",round(mzcta$COVID_CASE_RATE, 0),"</p>", 
                        "<p>","Median Income: ",round(mzcta$MedInc, 0),"</p>",
                        "<p>","Square feet per capita: ",round(mzcta$sqftpc, 0),"</p>")

pal_sqftpc_m <- colorBin(
  palette = "YlGn",
  domain = mzcta$sqftpc, 
  bins = c(floor(quantile(mzcta$sqftpc, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE)[1]) ,ceiling(quantile(mzcta$sqftpc, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE))[2:6])
)

pal_covid_m <- colorBin(
  palette = "YlGn",
  domain = mzcta$COVID_CASE_RATE, 
  bins = c(floor(quantile(mzcta$COVID_CASE_RATE, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE)[1]) ,ceiling(quantile(mzcta$COVID_CASE_RATE, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE))[2:6])
)

pal_inc_m <- colorBin(
  palette = "YlGn",
  domain = mzcta$MedInc, 
  bins = c(floor(quantile(mzcta$MedInc, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE)[1]) ,ceiling(quantile(mzcta$MedInc, na.rm=TRUE, probs=seq(0,1,0.2), names=FALSE))[2:6])
)


map_compare <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~pal_covid_m(mzcta$COVID_CASE_RATE),
              fillOpacity = 0.5,
              group = "COVID Case Rate", 
              popup = lapply(labels_compare,HTML)) %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~pal_inc_m(mzcta$MedInc),
              fillOpacity = 0.5,
              group = "Median Income", 
              popup = lapply(labels_compare,HTML)) %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~pal_sqftpc_m(mzcta$sqftpc),
              fillOpacity = 0.5,
              group = "Square feet per capita", 
              popup = lapply(labels_compare,HTML)) %>%
  addLayersControl(
    overlayGroups = c("COVID Case Rate", "Median Income", "Square feet per capita"),
    options = layersControlOptions(collapsed = FALSE))  %>%
  addLegend(pal = pal_covid_m, values = mzcta$COVID_CASE_RATE,
           group = "COVID Case Rate", 
           position = "bottomright") %>%
  addLegend(pal = pal_inc_m, values = mzcta$MedInc,
          group = "Median Income",
          position = "bottomright") %>%
  addLegend(pal = pal_sqftpc_m, values = mzcta$sqftpc,
            group = "Square feet per capita",
            position = "bottomright") %>%
  hideGroup("COVID Case Rate") %>% 
  hideGroup("Median Income") %>% 
  hideGroup("Square feet per capita") 
map_compare

#saveWidget(map_compare, file = "map_compare.html")

map_covid <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~pal_covid_m(mzcta$COVID_CASE_RATE),
              fillOpacity = 0.5,
              group = "COVID Case Rate", 
              popup = lapply(labels_compare,HTML)) %>%
  addLegend(pal = pal_covid_m, values = mzcta$COVID_CASE_RATE,
            group = "COVID Case Rate", 
            position = "bottomright", 
            title = "COVID19 Case Rate Per 100,000<br> by Zip Code")
map_covid

map_income <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~pal_inc_m(mzcta$MedInc),
              fillOpacity = 0.5,
              group = "Median Income", 
              popup = lapply(labels_compare,HTML)) %>%
  addLegend(pal = pal_inc_m, values = mzcta$MedInc,
            group = "Median Income",
            position = "bottomright", 
            title = "Median Income<br> by Zip Code")
map_income


source_sqft <- paste("<h10>","Source: ACS Population Estimates; NYC Parks: Walk-to-a-Park Service Area","</h10>")

map_sqft <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=mzcta,
              weight = 1,
              color = "grey",
              fillColor = ~pal_sqftpc_m(mzcta$sqftpc),
              fillOpacity = 0.5,
              group = "Square feet per capita", 
              popup = lapply(labels_compare,HTML)) %>%
  addLegend(pal = pal_sqftpc_m, values = mzcta$sqftpc,
            group = "Square feet per capita",
            position = "bottomright", 
            title = "Open Space Access<br>(Sq. Ft. Per Capita)<br> by Zip Code") 
map_sqft

### Walking Distance Example

map_iso <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=subset(iso, parkname=="Silver Lake Park")[1,], weight=0.1) %>%
  addCircles(data=subset(ct_demo, boro_ct201=="5007500")$center,
             group= "Center") %>%
  addCircles(data=subset(access, parkname=="Silver Lake Park")[1,],
             group= "Access",
             color="red")
map_iso
