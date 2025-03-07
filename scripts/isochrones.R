library(mapboxapi)
library(plyr)
library(dplyr)

#token for api is needed

library(mapboxapi)
mb_access_token(my_token, install = TRUE)

access <- st_read("data/Walk-to-a-Park Service area/geo_export_077c476d-cadb-41e8-a36d-b5994d952f89.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
    
isos=list()
system.time(
for(i in 1:nrow(access)){
  Sys.sleep(0.5)
  isos[[i]] <- mb_isochrone(location = c(st_coordinates(access[i,])[1], 
                                       st_coordinates(access[i,])[2]),
                            profile = "walking",
                            time = 10)
  print(i)
}
)

# not needed anymore - only when figuring out sys.sleep 
# ml1 <- append(isos,isos1[301:1628])
# ml2 <- append(ml1,isos2[1629:1933])
# ml3 <- append(ml2, isos4[1934:1943])
# ml <- append(ml3, isos3[1944:5241])



#df <- rbind.fill(ml)
df <- rbind.fill(isos)
df <- cbind(df,access %>% st_set_geometry(NULL)) %>%  st_as_sf()

st_write(df, "data/isochrones_10min_accesspts.geojson",  
         driver='GeoJSON', delete_dsn=TRUE)

