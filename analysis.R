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
                     variables = "B01003_001", 
                     state = "WI",
                     county = "Dane",
                     geometry = TRUE,
                     keep_geo_vars = TRUE) 

pop2 <- pop %>% 
  mutate(pop_density = estimate / (ALAND /27878400)) %>%  #create variable for population density
  drop_na %>%
  filter(pop_density > 100000) #for visualization purposes, drop low density BGs

#try map with leaflet
# see https://juliasilge.com/blog/using-tidycensus/


pal <- colorBin(palette = "viridis", domain = pop2$pop_density, bins = 5)

pop2 %>%
  st_transform(crs = "+init=epsg:4326") %>% #don't fully understand
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "Stamen.TonerLite") %>%
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
