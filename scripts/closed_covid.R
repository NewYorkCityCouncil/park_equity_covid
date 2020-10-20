library(RSocrata)
library(sf)
library(dplyr)
library(leaflet)

plygrd<- read_sf("https://data.cityofnewyork.us/api/geospatial/a4qt-mpr5?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84")

athe<- read_sf("https://data.cityofnewyork.us/api/geospatial/g3xg-qtbc?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84")

skate <- read_sf("https://data.cityofnewyork.us/api/geospatial/pvvr-75zk?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84")

dogrun <- read_sf("https://data.cityofnewyork.us/api/geospatial/wswf-9pts?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84")

adultequip <- read_sf("https://data.cityofnewyork.us/api/geospatial/tkzt-zfpz?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84")



sd_pa <- st_read("https://data.cityofnewyork.us/api/geospatial/4iha-m5jk?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84")

# make master file of park facilities closed

plygrd <- plygrd %>%  
  select(name, location, system, gispropnum,  approx_date_closed, 
         approx_date_reopened, editdate, status, district, 
         closuretype, geometry) %>% 
  mutate(fac_type = rep("Playground", nrow(plygrd)))
#
athe <- athe %>%  
  select(name, propertyname, system, gispropnum,  approx_date_closed, 
         approx_date_reopened, editdate, status, parkdistrict, closuretype, 
         geometry) %>% 
  mutate(fac_type = rep("Athletic ", nrow(athe)))
names(athe) <- names(plygrd)
#
dogrun <- dogrun %>%  
  select(name, propertyname, system, gispropnum,  approx_date_closed, 
         approx_date_reopened, editdate, status, parkdistrict, closuretype, 
         geometry) %>% 
  mutate(fac_type = rep("Dog Runs", nrow(dogrun)))
names(dogrun) <- names(plygrd)
# 
skate <- skate %>%  
  select(name, propertyname, system, gispropnum,  approx_date_closed, 
         approx_date_reopened, editdate, status, parkdistrict, closuretype, 
         geometry) %>% 
  mutate(fac_type = rep("Skate Park", nrow(skate)))
names(skate) <- names(plygrd)
#
adultequip <- adultequip %>%  
  select(propname, sitename, propid, propnum,  approx_date_closed, 
         approx_date_reopened, editdate, status, borough, closuretype, 
         geometry) %>% 
  mutate(fac_type = rep("Adult Equipment", nrow(adultequip)))
names(adultequip) <- names(plygrd)

# into 1 file
pc <- rbind(plygrd, athe, adultequip, dogrun, skate) %>% 
  # remove closure dates not related to covid
  filter(status == "Reopened" | status == "COVID-19 Closure" | 
           status == "Active") %>% 
  filter(approx_date_closed > '2020-03-22') %>% 
 # add duration column 
  mutate(approx_date_reopened = as.Date(approx_date_reopened),
         approx_date_closed = as.Date(approx_date_closed),
         editdate = as.Date(editdate))

row.names(pc)<-NULL

# fix wrong dates
pc[which(pc$approx_date_reopened=="2002-07-09"),]$approx_date_reopened <- rep(as.Date("2020-07-09"), 2)

pc <- pc %>% mutate(duration_closed = difftime(approx_date_reopened,
                                           approx_date_closed, 
                                           units = "weeks"))

# save table
write.csv(pc, "data/parksclosed_duetocovid.csv", row.names = F)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=sd_pa, weight = 1)