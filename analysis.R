## Libraries
library(sf)
library(tidycensus)
library(tidyverse)
library(ggmap)
library(leaflet)
# find variable
v18 <- load_variables(2018, "acs5", cache = TRUE)

# -> total population variable is B01003_001

#get bounding boxes for counties
counties_bb <- read_csv("https://github.com/stucka/us-county-bounding-boxes/raw/master/bounding.csv")

#Get census data
pop <- get_acs(geography = "block group",
        state = "WI",
        county = "Dane",
        year = 2018,
        variables = "B01003_001",
        geometry = TRUE),
        keep_geo_vars = TRUE)
pop2 <- pop %>% 
  mutate(pop_density = estimate / (ALAND /27878400)) %>%  #create variable for population density
  filter(pop_density > 50000)

#standalone vector heat map
density_map <- pop2 %>% 
  filter(pop_density > 50000) + #limit to higher-density tracts
  ggplot(aes(fill = pop_density)) +
  geom_sf(color = NA, alpha = 0.1) +
  coord_sf(crs = 3857) +
  scale_fill_viridis_c(option = "magma")

# get background raster map
bg_map <- get_stamenmap(bbox = (c(-89.601746,42.886530,-89.092255,43.217187)),
                        zoom = 12,
                        maptype = "toner-lite")


#combine add vector layer to bg map
ggmap(bg_map) +
  geom_sf(data = pop2,
          aes(fill = pop_density), 
          alpha = 0.8,
          color = NA,
          inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "magma")

#try map with leaflet
# see https://juliasilge.com/blog/using-tidycensus/



pop <- get_acs(geography = "block group", 
                     variables = "B01003_001", 
                     state = "WI",
                     county = "Dane",
                     geometry = TRUE,
                     keep_geo_vars = TRUE) 

pop2 <- pop %>% 
  mutate(pop_density = estimate / (ALAND /27878400)) %>%  #create variable for population density
  drop_na %>%
  filter(pop_density > 100000) #for visualization purposes, drop low density BGs



pal <- colorBin(palette = "viridis", domain = pop2$pop_density, bins = 5)

pop2 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(popup = ~ paste0(str_extract(NAME.y, "^([^,]*)"),
                               "<br>",
                               round(pop_density, 0),
                               " people/sq mi"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(pop_density)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ pop_density,
            title = "Population density",
            opacity = 1)
