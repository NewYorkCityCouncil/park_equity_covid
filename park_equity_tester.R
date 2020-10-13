# Park Equity Tester 

library(leaflet)
library(sf)
library(censusapi)
library(rgeos)

rm(list=ls())

# Access Points

access <- read_sf("Walk-to-a-Park Service area/geo_export_077c476d-cadb-41e8-a36d-b5994d952f89.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
shape <- read_sf("Walk-to-a-Park Service area/geo_export_f18e18a9-a859-4692-a3e0-48095a41a0d2.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")

map_access <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(data=access)
map_access

map_walk <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=shape, weight=1)
map_walk

# Income Data

ct <- read_sf("2010 Census Tracts/geo_export_8d38b305-5fed-49a0-a548-46435c11e818.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")

ct$center <- st_centroid(ct$geometry, of_largest_polygon = TRUE)

income_col <- c("NAME", "GEO_ID", paste0("S1901_C01_0",formatC(1:13, width = 2, flag = "0"), "E")) 

acs5_sub_tract <- getCensus(
  name = "acs/acs5/subject",
  vintage = 2018,
  vars = income_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

acs5_sub_tract$boro_code <- 0
for(i in 1:nrow(acs5_sub_tract)){ 
if(acs5_sub_tract$county[i]=="005") acs5_sub_tract$boro_code[i] <- "2"
if(acs5_sub_tract$county[i]=="081") acs5_sub_tract$boro_code[i] <- "4"
if(acs5_sub_tract$county[i]=="061") acs5_sub_tract$boro_code[i] <- "1"
if(acs5_sub_tract$county[i]=="047") acs5_sub_tract$boro_code[i] <- "3"
if(acs5_sub_tract$county[i]=="085") acs5_sub_tract$boro_code[i] <- "5"
}
acs5_sub_tract$boro_ct201 <- paste0(acs5_sub_tract$boro_code, acs5_sub_tract$tract)

ct_income <- st_sf(merge(acs5_sub_tract, ct, by="boro_ct201"))
ct_income$S1901_C01_012E <- ifelse(ct_income$S1901_C01_012E<=0, NA, ct_income$S1901_C01_012E)

# Income and Within 10-min Walk Overlay
map_income <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_income,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = ct_income$S1901_C01_012E)(ct_income$S1901_C01_012E),
              fillOpacity = 0.5,
              group = "Income") %>%
  addPolygons(data=shape, weight=1, group= "Walk") %>%
#  addCircles(data=ct_income$center, group= "Center") %>%
  addCircles(data=access, group= "Access", color="red") %>%
  addLayersControl(
    overlayGroups = c("Walk", "Income", "Access"),
    options = layersControlOptions(collapsed = FALSE))
map_income


