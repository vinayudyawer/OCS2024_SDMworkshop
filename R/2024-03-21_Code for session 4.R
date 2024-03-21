## Session 4 script

library(tidyverse)
library(sf)
library(terra)
library(ggspatial)

## Input occurrence data

tracks <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_2/mp_fit_pred.csv')
null_tracks <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_2/null_data.csv')

model_extent <- vect("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/mod_ext.GeoJSON")

null_sf <-
  null_tracks %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
  vect()

bg <-
  model_extent %>% st_as_sf() %>% 
  st_sample(1500) %>% 
  vect() %>% 
  difference(env_stack[[1]])

plot(bg, col = "grey")
plot(model_extent, add = T)
plot(null_sf ,add = T)


# quick plot to show the presence and pseudo absences

ggplot() +
  annotation_map_tile()



## Input environmental data

env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/env_layers.tif")


## 



