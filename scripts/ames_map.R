# --------------------------------libraries-------------------------------------
library(tidyverse)
library(osmdata)
library(modeldata)
data("ames")

# ---------------------------------setup----------------------------------------

extrafont::loadfonts(device = "win")

# sf for plotting map of Ames
big_streets <- 
  getbb("Ames Iowa") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <-
  getbb("Ames Iowa") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <-
  getbb("Ames Iowa") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

rivers <-
  getbb("Ames Iowa") %>%
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = "river") %>%
  osmdata_sf() 

railway <-
  getbb("Ames Iowa") %>%
  opq() %>%
  add_osm_feature(key = "railway",
                  value = "rail") %>%
  osmdata_sf() 

# ---------------------------------plot-----------------------------------------
ames %>%
  ggplot() +
  geom_sf(data = rivers$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-93.70, -93.57),
           ylim = c(41.98, 42.07)) +
  geom_point(aes(x = Longitude, 
                 y = Latitude,
                 color = sqrt(Sale_Price)),
             size = 2.5,
             alpha = 0.35,
             position = position_jitter(width = 0.0005, height = 0.0005)) +
  shadowtext::geom_shadowtext(x = -93.635,
                              y = 42.025,
                              label = "Ames, Iowa",
                              size = 20,
                              family = "Source Sans Pro Light",
                              color = "#595959",
                              bg.color = "#F9F9FC") +
  MetBrewer::scale_color_met_c("Hiroshige", -1) +
  theme_void() +
  theme(legend.position = "none")

riekelib::ggquicksave("plots/ames_map.png")

