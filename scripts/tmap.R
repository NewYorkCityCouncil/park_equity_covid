library(dplyr)
library(tmap)
library(sf)

options(scipen = 999)

# files --------
openspace <- st_read("data/parks_with_sf_matched.geojson", stringsAsFactors = FALSE)

ct_walk <- st_read("data/ct_walk/ct_walk.shp", stringsAsFactors = FALSE) %>% 
  filter(ntaname!="Airport" & ntaname!="Rikers Island" & 
           !grepl('park-c*', ntaname)) %>% 
  filter(!is.na(B01003_))

ct_removed <- st_read("data/ct_walk/ct_walk.shp")  %>% 
  # remove park areas & etc, airports, rikers island
  filter( grepl('park-c*', ntaname))

puma <- st_read("data/Public Use Microdata Areas (PUMA).geojson")


##### map ----
#70b18a
tmap_mode("plot")
map<- # 
  tm_shape(ct_walk)+ 
  tm_polygons(col = "prkttpc", 
              border.col = NULL, 
              palette="Greys", n = 7, 
              contrast = c(0.1, 0.7), lwd=.9,
              style="fixed",
              breaks = c(0, 136.3155, 321.062, 889.08,
                         4228.69, 7750.2450,368532.880),
              title="Square Footage Per Capita \nby Census Tract",
              legend.format=list(
                fun=function(x) prettyNum(x, digits=2, 
                                        big.mark = ",", format="d"))) +
  tm_shape(ct_removed) + 
      tm_fill(col = "#b2c6bb", alpha = 0.8) +
      tm_borders(col = "#9c9c9c", lwd = .7, lty = "dotted") +
  tm_add_legend(type = "line", labels = "Cemeteries were excluded",
                col = "#9c9c9c",lwd = 1,lty = "dotted") +
  tm_shape(openspace) + tm_fill(col = "#b2c6bb", alpha = 0.8) +
  tm_shape(puma) +
  tm_borders(col = "#9c9c9c", lwd = .4) +
  tm_layout(asp=0,
            legend.title.size= 1, 
            legend.title.fontface = "bold",
            legend.text.size=.9, legend.width = 1,
            legend.text.fontfamily="Open Sans",
            legend.title.fontfamily="Georgia", 
            frame = F, 
            legend.position = c("0.05", "0.65"),
            outer.margins = c(0,0,0,0)) +   
  tm_credits("\n \n Source: US Census: 2018 ACS 5-Year Survey Estimates, NYC Parks: Walk-to-a-Park Service Area",
             size = 0.7, 
             fontfamily = "Open Sans", 
             position = c("right", "bottom"), 
             fontface = "italic", 
             align="right")

tmap_save(map, paste0("visuals/htc_map_", format(Sys.Date()-1, "%m%d"), ".png"), width = 2000, height = 2200, dpi = 300)


##### map ----
#70b18a
ct_walk <- st_read("data/ct_walk/ct_walk.shp", stringsAsFactors = FALSE) %>% 
  filter(ntaname!="Airport" & ntaname!="Rikers Island" & 
           !grepl('park-c*', ntaname)) %>% 
  filter(!is.na(S1901_C))
 ct_walk$sfinc <- ct_walk$parktot/ct_walk$S1901_C
tmap_mode("plot")
map<- 
  tm_shape(ct_walk)+ 
  tm_polygons(col = "sfinc", 
              border.col = NULL, 
              palette="Greys", n = 7, 
              contrast = c(0.1, 0.7), lwd=.9,
              style="fixed",
              breaks = c(0, 8.142249, 22.915140, 60.9003962,
                         254.4795637, 405.2194692, 2004.8200582),
              title="Square Footage Per Income \nby Census Tract",
              legend.format=list(
                fun=function(x) prettyNum(x, digits=2, 
                                          big.mark = ",", format="d"))) +
  tm_layout(asp=0,
            legend.title.size= 1, 
            legend.title.fontface = "bold",
            legend.text.size=.75, legend.width = 1,
            legend.text.fontfamily="Open Sans",
            legend.title.fontfamily="Georgia", 
            frame = F, 
            legend.position = c("0.05", "0.70"),
            outer.margins = c(0,0,0,0)) +
  tm_shape(ct_removed) + 
  tm_fill(col = "#b2c6bb", alpha = 0.8) +
  tm_borders(col = "#9c9c9c", lwd = .7, lty = "dotted") +
  tm_add_legend(type = "line", 
                labels = "Cemeteries excluded in analysis",
                col = "#9c9c9c",lwd = 1,lty = "dotted") +
  tm_shape(openspace) + tm_fill(col = "#b2c6bb", alpha = 0.8) +
  tm_shape(puma) +
  tm_borders(col = "#9c9c9c", lwd = .4) +
  tm_credits("\n \n Source: US Census: 2018 ACS 5-Year Survey Estimates, NYC Parks: Walk-to-a-Park Service Area",
             size = 0.7, 
             fontfamily = "Open Sans", 
             position = c("right", "bottom"), 
             fontface = "italic", 
             align="right")

tmap_save(map, "visuals/sfpercapita.png", width = 2000, 
          height = 2200, dpi = 300)


# sf per income bins
plot(density(ct_walk$parktot/ct_walk$S1901_C, na.rm = T))
brks <- classIntervals(ct_walk$parktot/ct_walk$S1901_C, n=5, style = "hclust")

quantile(ct_walk$parktot/ct_walk$S1901_C, na.rm=TRUE, probs=seq(0,1,0.05))

pal_sqftinc <- colorBin(
  palette = "YlGn",
  domain = ct_walk$parktot, 
  bins = ceiling(brks$brks))


# sf per capita bins

plot(density(ct_walk$prkttpc, na.rm = T))
brks_ht <- classIntervals(ct_walk$prkttpc,  
                          style = "fixed", n=7,
                          fixedBreaks = c(0, 136.3155, 321.062, 889.08,
                                          4228.69, 7750.2450,368532.880))

quantile(ct_walk$prkttpc, na.rm=TRUE, probs=seq(0,1,0.05), names=FALSE)

classIntervals(jenks71$jenks71, n=5, style="fixed",
               fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30)

pal_sqftpc <- colorBin(
  palette = colorRamp(c("#7fbf7f", "#198c19"), interpolate = "spline"),
  domain = ct_walk$prkttpc, 
  bins = ceiling(brks_ht$brks))


