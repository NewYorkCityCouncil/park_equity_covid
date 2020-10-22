library(RSocrata)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tmap)

library(hrbrthemes)
library(councildown)
library(ggrepel)
options(scipen = 999)

# parks areas closed during covid ----------
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

# make master file of park facilities closed --------------

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

# charts ----------
pc_fac <- pc %>% as.data.frame() %>% 
  group_by(fac_type) %>% 
  count(fac_type, name = "count") %>% 
  arrange(count)

# to get it in descending order in ggplot
pc_fac$fac_type <- factor(pc_fac$fac_type, 
                          levels = unique(pc_fac$fac_type))

# Barplot
ggplot(pc_fac, aes(fill = fac_type, y = fac_type, 
                   x = count, label = fac_type)) + 
  geom_bar(stat = "identity",  width = 0.5, 
           alpha=0.9) +
  scale_fill_nycc(palette = "mixed", discrete = TRUE) +
  scale_x_continuous(breaks = seq(0, max(pc_fac$count), 500),
                     labels = scales::comma(
                       seq(0, max(pc_fac$count), 500))) +
  geom_text_repel(data = pc_fac,
                  label = scales::comma(pc_fac$count, accuracy = 1),
                  size = 3, direction = 'x', force = 1, hjust = 1) +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11,
              axis_text_size = 9) +
  xlab("Number Closed") + ylab('') +
  ggtitle("Park Facilities Closed During Covid", 
  "Between 3/22/20 to 5/21/20, 5914 park facilities were closed. Majority were athletic. As of 10/20/20, 232 do not have a reopened date, but only 22 have 'Covid-19 Closure' as their status.") +
  labs(caption = 'NYC Parks: Parks Closure Status Due to COVID-19') +
  theme(legend.position="none", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        text = element_text(family = "Open Sans"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = "Georgia", size = 14),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 10,
             margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(
          margin = margin(t = 10, r = 0, b = 0, l = 0)))
  
w1 = pc[which(is.na(pc$approx_date_reopened)==T),8]

pc_covid <- pc %>% filter(status == "COVID-19 Closure")
st_geometry(pc_covid) <-'geometry'

#####

pp <- st_read("data/Parks_Properties/geo_export_632341e5-8301-4355-a485-4185d449492d.shp")  %>% 
  st_transform("+proj=longlat +datum=WGS84")

park <- right_join(pp, pc_covid %>% as.data.frame(), by=c("gispropnum" = "gispropnum")) %>% filter(!duplicated(gispropnum))

# map -------
leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=pc_covid[pc_covid$fac_type=="Athletic ",],
              weight = 1,
              color = "green",
              fillColor = "green",
              fillOpacity = 0.5) 