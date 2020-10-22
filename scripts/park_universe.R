library(RSocrata)
library(sf)
library(dplyr)
library(leaflet)
library(nngeo)

options(scipen = 999)

# open space (parks) -----------
openspace <- st_read('data/OpenSpace/geo_export_731ca47d-b06c-49f3-811d-c1d13862f1b9.shp', stringsAsFactors = FALSE) %>% 
  st_transform("+proj=longlat +datum=WGS84")

# park properties -----------

parksprop <- st_read('data/Parks_Properties/geo_export_632341e5-8301-4355-a485-4185d449492d.shp', stringsAsFactors = FALSE) %>%
  st_transform("+proj=longlat +datum=WGS84")

# add park properties details to open space file  --------
# check - sort(table(parksprop$gispropnum),decreasing = T)[1:10]
pp<- parksprop %>% as.data.frame() %>% select(!geometry)

park_universe<- left_join(openspace, pp, 
                          by = c("parknum"="omppropid"), 
                          keep=T) %>% 
  filter(!duplicated(source_id)) %>% 
  mutate(uid = paste0(source_id, globalid))


# get park feature codes for open space -----
# https://github.com/CityOfNewYork/nyc-planimetrics/blob/master/Capture_Rules.md#open-space-attributes

pf <- read.csv("data/park_features.csv", stringsAsFactors = F) %>% 
  janitor::clean_names()

park_universe <- park_universe %>% 
  left_join(pf, by = c("feat_code"="feature_code"))

# fill in missing land use with subtype
park_universe[is.na(park_universe$landuse)==T,]$landuse <- park_universe[is.na(park_universe$landuse)==T,]$subtype


# add park features not found in open space --------
rdf<- openspace %>% as.data.frame() 

rj1<- left_join(parksprop,rdf, by=c("gispropnum"="parknum"), keep=T) %>% 
  filter(!duplicated(globalid)) %>% 
  mutate(uid = paste0(source_id, globalid))

innr1 <- inner_join(parksprop,rdf, by=c("gispropnum"="parknum")) %>% 
  filter(!duplicated(globalid)) %>% 
  mutate(uid = paste0(source_id, globalid))

pp_minus_innr1 <- rj1[which(rj1$uid%in%innr1$uid==F),]

## reorder columns to rbind
pp_minus_innr1$subtype <- rep(NA, nrow(pp_minus_innr1))

pp_minus_innr1 <- pp_minus_innr1[,c(36:45,1:35,49)]

pp_minus_innr1<- pp_minus_innr1 %>% select(!uid)
 st_geometry(pp_minus_innr1) <- "geometry"

#test<-cbind(names(park_universe), names(pp_minus_innr1))

park_universe <- rbind(park_universe, pp_minus_innr1)

# clean
park_universe$landuse <- gsub("Tracking Only", "Tracking", 
                              park_universe$landuse) 


# there are rows for subfeatures within a main feature, dissolve
  # length(which(is.na(park_universe$parknum)==T)) --------

# fill-in missing parknum
park_universe[is.na(park_universe$parknum)==T,]$parknum <- 
  park_universe[is.na(park_universe$parknum)==T,]$gispropnum 

 park_universe_final <- park_universe %>% 
   select(shape_area, parknum, park_name,landuse, jurisdicti, location, 
          typecatego, name311, subcategor, acquisitio, subtype) %>% 
   group_by(parknum) %>% 
   summarize(park_name = first(park_name),
             shape_area = max(as.numeric(shape_area)),
             landuse = paste(unique(landuse), collapse = ", "),
             jurisdicti = paste(unique(jurisdicti), collapse = ", "),
             location = paste(unique(location), collapse = ", "),
             typecatego = paste(unique(typecatego), collapse = ", "),
             name311 = paste(unique(name311), collapse = ", "),
             subcategor = paste(unique(subcategor), collapse = ", "),
             acquisitio = paste(unique(acquisitio), collapse = ", "),
             subtype = paste(unique(subtype), collapse = ", ")) 
   
 # get square footage ---------
 
 #st_area(park_universe_final)[1]
 #1971.664 [m^2]
 # https://rpubs.com/oaxacamatt/sqm_2_sqft
 convert_sqm_2_sqft <- function(sq_meters){
   sqin_per_sqm = 39.3 * 39.3
   sqft_per_sqm <- sqin_per_sqm / 144
   return(sq_meters * sqft_per_sqm)
 }
 
park_universe_final$squareft <- 
  convert_sqm_2_sqft(as.numeric(st_area(park_universe_final)))
#park_universe_final$test = as.numeric(st_area(park_universe_final))

row.names(park_universe_final)<-NULL

st_write(park_universe_final, "data/openspace_parks_dissolve.geojson",
            driver='GeoJSON', delete_dsn=TRUE)


# add square footage to access pts ----
# 50 ft - 15.24m

# walk to a park service area ----
access <- st_read("data/Walk-to-a-Park Service area/geo_export_077c476d-cadb-41e8-a36d-b5994d952f89.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")

# waterfront properties ---------- has square footage already
wf <- st_read("data/Waterfront/geo_export_3a9b8ad8-00be-4d92-ad09-16ab27adcb34.shp") %>%
  st_transform("+proj=longlat +datum=WGS84") %>% 
  filter(status=="Open") %>% 
  select(name, wpaa_area, wpaa_id, geometry)

pow <- st_read("data/publiclyownedwaterfront/geo_export_accfbd3e-51d3-4589-807c-67f728e5026f.shp") %>%
  st_transform("+proj=longlat +datum=WGS84") %>% 
  mutate(uid = paste(name, shape_star ,sep = " "))

# to aviod running the whole st_join which takes awhile, ran the ones that didnt match with the new features added
tt1<- st_read("data/sf_access.geojson")

# tt1 <- tt1[tt1$typecategory == "Waterfront",]
# 
# tt1 <- tt1 %>% 
filter(!is.na(squareft)) %>% 
mutate(uid = paste(park_name, squareft ,sep = " ")) %>% 
filter(!duplicated(uid))

wft <- inner_join(pow, tt1 %>% as.data.frame(), by = c("uid"="uid"))

wft <- wft[,10:22]
park_universe_final <- park_universe_final %>% select(1:11, 13,12)
names(wft)<- names(park_universe_final)
st_geometry(wft) <- "geometry"
parks_universe_final_wf <- rbind(park_universe_final, wft)

st_write(parks_universe_final_wf,"data/parks_with_sf_matched.geojson",
        driver='GeoJSON', delete_dsn=TRUE)
     

############
sf2<- st_join(access[which(is.na(tt1$squareft)==T),], 
              park_universe_final, join = st_nn, maxdist = 15.24)
names(sf2) <- names(tt1)
tt1[which(is.na(tt1$squareft)==T),] <-sf2

sf3<- st_join(access[which(is.na(tt1$squareft)==T),], 
              pow, join = st_nn, maxdist = 15.24)
names(sf3)[names(sf3)=="agency"] <- "jurisdiction"
names(sf3)[names(sf3)=="name"] <- "park_name"
names(sf3)[names(sf3)=="shape_star"] <- "squareft"
sf3$parknum <- rep(NA, nrow(sf3))
sf3$shape_area <- rep(NA, nrow(sf3))
sf3$landuse <- rep(NA, nrow(sf3))
sf3$location <- rep(NA, nrow(sf3))
sf3$typecategory <- rep("Waterfront", nrow(sf3))
sf3$name311 <- sf3$park_name
sf3$subcategory <- rep("Waterfront", nrow(sf3))
sf3$acquisitiondate <- rep(NA, nrow(sf3))
sf3$subtype <- rep("Waterfront", nrow(sf3))
sf3<- sf3 %>% select(!link)
sf3<- sf3 %>% select(!shape_stle)
sf3<- sf3 %>% select(gispropnum, parkname, type, parknum, park_name, shape_area, landuse, jurisdiction, location, typecategory, name311, subcategory, acquisitiondate, subtype, squareft, geometry)

tt1[which(is.na(tt1$squareft)==T),] <-sf3


# sf_access <-  st_join(access, park_universe_final, join = st_nn, maxdist = 15.24)

st_write(tt1, "data/sf_access.geojson",
         driver='GeoJSON', delete_dsn=TRUE) 
   
# pluto res ------ future work, time from every residence to nearest park
#pluto <- st_read("data/pluto_res_columns.geojson") %>% 
#  filter(unitsres>0)

#st_write(pluto, "data/pluto_res_only.geojson")

# checks
tt1$uid<- paste(tt1$gispropnum, tt1$parkname, tt1$parknum, tt1$park_name, sep = " ")

tt2 <- tt1[!duplicated(tt1$uid),]

tt3 <- tt2 %>% filter(typecategory=="Community Park" | typecategory=="	Neighborhood Park")
tt4 <- tt2 %>% filter(typecategory=="NA" | typecategory=="Triangle/Plaza")

# check distribution - extremely skewed
plot(density(log10(tt2$squareft),na.rm = T))
abline(v=median(log10(tt2$squareft), na.rm=T))
abline(v=mean(log10(tt2$squareft),  na.rm = T), col='blue')

qqnorm(log10(tt2$squareft), pch = 1, frame = FALSE)
qqline(log10(tt2$squareft), pch = 1, frame = FALSE)
library(ggpubr)
tt2$log<- log10(tt2$squareft)
p <- ggqqplot(tt2, x = "log") +
  scale_x_continuous(breaks = seq(-3,3,.5),
                     labels = seq(-3,3,.5)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     labels = seq(0,8,1)) +
  theme(panel.grid.major.x = element_line(),
        panel.grid.major.y = element_line())

quantile(tt2$log, na.rm = T, seq(0, 1, by=.1))


# number of outliers
length(boxplot(tt$squareft, plot=FALSE)$out)
# last outlier
sort(boxplot(tt$squareft, plot=FALSE)$out, decreasing = T)[1]
# first outlier
first_out <- sort(boxplot(buff$squareft, plot=FALSE)$out) [1]










