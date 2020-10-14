# Park Equity Tester 

library(leaflet)
library(sf)
library(censusapi)
library(rgeos)
library(ggplot2)
library(dplyr)
library(htmltools)

rm(list=ls())

# Access Points

access <- read_sf("data/Walk-to-a-Park Service area/geo_export_077c476d-cadb-41e8-a36d-b5994d952f89.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
shape <- read_sf("data/Walk-to-a-Park Service area/geo_export_f18e18a9-a859-4692-a3e0-48095a41a0d2.shp") %>%
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

# Income and Within 10-min Walk Overlay

labels <- paste("<h3>","Name: ",ct_demo$NAME, "</h3>",
                "<p>",paste0("Tract: ",ct_demo$tract),"</p>", 
                "<p>",paste0("Median Income: ",ct_demo$S1901_C01_012E),"</p>",  
                "<p>",paste0("Population: ",ct_demo$B01003_001E),"</p>")

map <- leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_demo,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = ct_demo$S1901_C01_012E)(ct_demo$S1901_C01_012E),
              fillOpacity = 0.5,
              group = "Income", 
              popup = lapply(labels,HTML)) %>%
  addPolygons(data=ct_demo,
              weight = 1,
              color = "grey",
              fillColor = ~colorBin("YlOrRd", domain = ct_demo$B01003_001E)(ct_demo$B01003_001E),
              fillOpacity = 0.5,
              group = "Pop", 
              popup = lapply(labels,HTML)) %>%
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
    overlayGroups = c("Walk", "Income", "Pop", "Access", "Center", "Not Walkable"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Income") %>% 
  hideGroup("Pop") %>% 
  hideGroup("Access") %>% 
  hideGroup("Center") 
map

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



# IBO map
ps_ct <- st_read("data/ParkSpaceCT/PPC4.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
pncf <- st_read("data/parksNeighborCommFlag/parksfixed.shp") 

map_ibo <- leaflet(pncf) %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight=1) %>%
  addPolygons(data=ps_ct,
              weight = 1,
              color = "grey",
              fillColor = ~colorNumeric("YlOrRd", domain = ps_ct$Sqftper_Ca)(ps_ct$Sqftper_Ca),
              fillOpacity = 0.5)
map_ibo
