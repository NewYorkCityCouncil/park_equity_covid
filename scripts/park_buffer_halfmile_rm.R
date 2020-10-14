library(RSocrata)
library(sf)
library(dplyr)
library(leaflet)
library(sp)


# walk to a park service area ----
wpsa_points <- st_read("https://data.cityofnewyork.us/api/geospatial/5vb5-y6cv?method=export&format=GeoJSON") 

wpsa_points$type <- as.character(wpsa_points$type)


wpsa_points <- wpsa_points[wpsa_points$type!="1/4 MILE AND 1/2 MILE SERVED AREA",]

wpsa_points <- wpsa_points[ ! st_is_empty( wpsa_points ) , ]
sp_wpsa_pts<- as_Spatial(wpsa_points)

wpsa_area<- st_read('https://data.cityofnewyork.us/api/geospatial/rg6q-zak8?method=export&format=GeoJSON')

#
aeqd.buffer <- function(p, r)
{
  stopifnot(length(p) == 1)
  aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
                  p@coords[[2]], p@coords[[1]])
  projected <- spTransform(p, CRS(aeqd))
  buffered <- gBuffer(projected, width=r, byid=TRUE)
  spTransform(buffered, p@proj4string)
}

iterate.spdf = function(x){
  p = list()
  for(i in 1:nrow(x)){
    p[[i]] = x[i,]
  }
  return(do.call(rbind,p))
}

correct_buff<- iterate.spdf(aeqd.buffer(sp_wpsa_pts[i,], 804.672))

p = list()
correct_buff = c()
for(i in 1:nrow(sp_wpsa_pts)){
  p[[i]] = aeqd.buffer(sp_wpsa_pts[i,], 804.672)
  correct_buff= do.call(rbind,p)
  print(i)
}



# half mile buffer
# 0.0145 degrees of arc per mile
# # http://mathcentral.uregina.ca/QQ/database/QQ.09.97/dyck1.html#:~:text=The%20length%20of%20an%20arc,360%3D69%20miles%20per%20degree. 

# buffer of all wpsa points at half a mile
buff_half<- st_buffer(wpsa_points, 0.00725)

# buffer of wpsa points at fourth and a half a mile
buff_

leaflet() %>% 
  # default settings ---------------
setView(-73.933560,40.704343, zoom = 10.5) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  # shapefiles -----------
addPolygons(data = correct_buff, weight = 1) 