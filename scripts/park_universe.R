library(RSocrata)
library(sf)
library(dplyr)
library(leaflet)
library(nngeo)



# open space (parks) -----------
r <- st_read("https://data.cityofnewyork.us/api/geospatial/g84h-jbjm?method=export&format=GeoJSON") 
  
r_shp <- st_read('data/OpenSpace/geo_export_731ca47d-b06c-49f3-811d-c1d13862f1b9.shp') 
  


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
rna<-r[which(is.na(rj$parknum==T)),]
# map to see what/where are they
# leaflet() %>%
#   # default settings ---------------
# setView(-73.933560,40.704343, zoom = 10.5) %>%
#   addProviderTiles("CartoDB.Positron") %>% 
#   addPolygons(data = rna, fillOpacity = 0.5, fillColor = "blue",   
#               stroke = T, popup = rna$feat_code)

w1 <- which(is.na(rj$system)==T)


# get park feature codes -----
# https://github.com/CityOfNewYork/nyc-planimetrics/blob/master/Capture_Rules.md#open-space-attributes

pf <- read.csv("data/park_features.csv", stringsAsFactors = F) %>% 
  janitor::clean_names()
rj$feat_code <- as.integer(as.character(rj$feat_code))
rj <- rj %>% left_join(pf, by = c("feat_code"="feature_code"))

rj$landuse <- as.character(rj$landuse)
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



# walk to a park service area ----
wpsa_points <- st_read("https://data.cityofnewyork.us/api/geospatial/5vb5-y6cv?method=export&format=GeoJSON")  %>% 
  filter(!type=="1/4 MILE AND 1/2 MILE SERVED AREA")

wpsa_area<- st_read('https://data.cityofnewyork.us/api/geospatial/rg6q-zak8?method=export&format=GeoJSON')


# there are rows for subfeatures within a main feature, using parknum and the largest shape area to select the main features 
length(which(is.na(rj$parknum)==T))
ff <- rj %>% 
  group_by(parknum) %>%
  top_n(1, abs(as.integer(as.character(shape_area))))

# test
 tt <- rj %>% filter(parknum=="BS29")

# join open space features to wpsa points ----
# map test
leaflet() %>%
  # default settings ---------------
setView(-73.933560,40.704343, zoom = 10.5) %>%
  addProviderTiles("CartoDB.Positron") %>%
   addCircleMarkers(data = access[1514,], color = 'blue', 
                    radius = 2, weight = 1,
                    popup = paste(access[1514,]$parkname, access[1514,]$gispropnum)) %>% 
   
   addPolygons(data = rj_dissolve, weight = 6, color="green", 
               popup = paste(rj_dissolve$park_name, rj_dissolve$parknum)) %>% 
  addPolygons(data = tp, weight = 5, popup = tp$park_name) %>% 
 #  addPolygons(data = shape, color = "red", weight = 1)

buff <- st_read("data/halfmile_buffer_pts.geojson")
tp <- rj_dissolve %>% filter(squareft<1) 
 
# join dissolve parks
 rj_dissolve <- rj %>% 
   select(shape_area, parknum, park_name,landuse, jurisdiction, location, 
          typecategory, name311, subcategory, acquisitiondate, subtype) %>% 
   group_by(parknum) %>% 
   summarize(park_name = first(park_name),
             shape_area = max(as.numeric(shape_area)),
             landuse = paste(unique(landuse), collapse = ", "),
             jurisdiction = paste(unique(jurisdiction), collapse = ", "),
             location = paste(unique(location), collapse = ", "),
             typecategory = paste(unique(typecategory), collapse = ", "),
             name311 = paste(unique(name311), collapse = ", "),
             subcategory = paste(unique(subcategory), collapse = ", "),
             acquisitiondate = paste(unique(acquisitiondate), collapse = ", "),
             subtype = paste(unique(subtype), collapse = ", ")) 
   
 # to square foot
 
 #st_area(rj_dissolve)[1]
 #1971.664 [m^2]
 # https://rpubs.com/oaxacamatt/sqm_2_sqft
 convert_sqm_2_sqft <- function(sq_meters){
   sqin_per_sqm = 39.3 * 39.3
   sqft_per_sqm <- sqin_per_sqm / 144
   return(sq_meters * sqft_per_sqm)
 }
 
   rj_dissolve$squareft = convert_sqm_2_sqft(as.numeric(st_area(rj_dissolve)))
   rj_dissolve$test = as.numeric(st_area(rj_dissolve))

   row.names(rj_dissolve)<-NULL
   
   st_write(rj_dissolve, "data/openspace_parks_dissolve.geojson",
            driver='GeoJSON', delete_dsn=TRUE)

# add square footage to access pts ----
# 50 ft - 15.24m
sf_access <-  st_join(access, rj_dissolve, join = st_nn, maxdist = 15.24)

st_write(sf_access, "data/sf_access.geojson",
         driver='GeoJSON', delete_dsn=TRUE) 


# map test
leaflet() %>%
  # default settings ---------------
setView(-73.933560,40.704343, zoom = 10.5) %>%
  addProviderTiles("CartoDB.Positron") %>%
   # addCircleMarkers(data = wpsa_points[wpsa_points$parkname=="Hunter's Point",], color = 'green', 
   #                  radius = 4, weight = 1) %>% 
  # addPolygons(data = buff, weight = 1, color="green") %>% 
  addPolygons(data = df[453,][1], weight = 1, fillOpacity = 0.5, stroke = F) %>% 
  addPolygons(color= 'green', data = buff[buff$parknum=="Q306" | buff$parknum=="Q309" | buff$parknum=="B126" | buff$parknum=="X039" | buff$parknum=="Q020" | buff$parknum=="R142" | buff$parknum=="X118",], weight = 3, popup = buff[buff$parknum=="Q306" | buff$parknum=="Q309" | buff$parknum=="B126" | buff$parknum=="X039" | buff$parknum=="Q020" | buff$parknum=="R142" | buff$parknum=="X118",]$parknum) %>% 
  addCircleMarkers(data = wpsa_points[wpsa_points$gispropnum=="X118",], color = 'orange', radius = 4, weight = 1, popup = paste(wpsa_points[wpsa_points$gispropnum=="X118",]$parkname))  %>% 

  #  addPolygons(data = shape, color = "red", weight = 1)
 
  ct_walk <-   st_read("data/ct_walk.geojson")
   
# pluto res ------
#pluto <- st_read("data/pluto_res_columns.geojson") %>% 
#  filter(unitsres>0)

#st_write(pluto, "data/pluto_res_only.geojson")


buff <- st_read('data/openspace_parks_dissolve.geojson')

# check distribution - extremely skewed
plot(density(buff$squareft))
abline(v=median(buff$squareft))
abline(v=mean(buff$squareft), col='blue')


# number of outliers
length(boxplot(buff$squareft, plot=FALSE)$out)
# last outlier
sort(boxplot(buff$squareft, plot=FALSE)$out, decreasing = T)[1]
# first outlier
first_out <- sort(boxplot(buff$squareft, plot=FALSE)$out) [1]