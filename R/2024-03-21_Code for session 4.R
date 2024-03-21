## Session 4 script

library(tidyverse)
library(terra)
library(ggspatial)

## add land for remove points on land (internal only not for the course)
## Land data
# sf_use_s2(FALSE)
# 
# land <- 
#   rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
#   st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
#   st_set_crs(4326) %>% 
#   summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>% 
#   st_make_valid()
# 
# sf_use_s2(TRUE)


## Input occurrence data

tracks <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_4/track_data.csv')

null_tracks <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_4/null_data.csv')

background <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_4/bg_data.csv')

model_extent <- vect("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/mod_ext.GeoJSON")


# quick plot to show the presence and pseudo absences

ggplot() +
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(model_extent, fill = NA) +
  geom_spatial_point(data = background, aes(x = lon, y = lat), col = "grey", crs = 4326) +
  geom_spatial_point(data = null_tracks, aes(x = lon, y = lat), col = "pink", crs = 4326) +
  geom_spatial_point(data = tracks, aes(x = lon, y = lat), col = "firebrick", crs = 4326) +
  theme_void()



## Input environmental data
env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/env_layers.tif")

# quick plot

plot(env_stack)



## extract environmental data for each occurrence, pseudo-absence and background point

tracks_vec <- vect(tracks, geom=c("lon", "lat"), crs = "EPSG:4326")
null_vec <- vect(null_tracks, geom=c("lon", "lat"), crs = "EPSG:4326")
bg_vec <- vect(tracks, geom=c("lon", "lat"), crs = "EPSG:4326")


## Now lets build a model





