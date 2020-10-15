# IBO map

library(leaflet)
library(sf)
library(dplyr)
library(htmltools)

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