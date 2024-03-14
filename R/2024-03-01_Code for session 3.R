## Setup data and generate plots for session 3

library(tidyverse)
library(lubridate)
library(mapview)
library(sf)
library(terra)
library(rnaturalearth)
library(dismo)

## --------------------------------------------------------------------------------------------------- ##
## Land data
sf_use_s2(FALSE)

land <- 
  ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
  st_set_crs(4326) %>% 
  summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>% 
  st_make_valid()

sf_use_s2(TRUE)
  
mapview(land)

## --------------------------------------------------------------------------------------------------- ##
## Occurrence data

# occ <- 
#   read_tsv("Data/Oceanic whitetip_gbif.csv") %>% 
#   transmute(species, common_name = "Oceanic Whitetip Shark", 
#             date_time = ymd_hm(eventDate),
#             lon = decimalLongitude, lat = decimalLatitude, issue) %>% 
#   filter(!is.na(lon)) %>% 
#   filter(between(lon, 31.5, 105)) %>% 
#   mutate(a = case_when(str_detect(issue, pattern = "CONTINENT_DERIVED_FROM_COORDINATES") ~ 1,
#                        TRUE ~ 0)) %>% 
#   filter(a %in% 0) %>% 
#   select(-a, -issue)
# 
# write_csv(occ, "Data/Oceanic whitetip occ.csv")

occ <- read_csv("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/Oceanic%20whitetip%20occ.csv")

occ_sf <-
  occ %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)

occ_vect <- vect(occ_sf)

mapview(occ_vect)

## --------------------------------------------------------------------------------------------------- ##
## Pseudo-absence data
occ_buff <- 
  occ_sf %>% 
  st_buffer(dist = 40e4) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  rename(geometry = x)

## model extent polygon
mod_ext <- 
  st_bbox(occ_buff) %>% 
  st_as_sfc() %>% 
  st_as_sf()

sf_use_s2(FALSE)
l_diff <- st_difference(occ_buff, land)
l_diff2 <- st_difference(mod_ext, land)

pseudo <- 
  st_sample(l_diff, 500) %>% 
  st_as_sf() %>% 
  bind_rows(st_sample(l_diff2, 500) %>% 
              st_as_sf())
sf_use_s2(TRUE)

mapview(l_diff2) + l_diff + pseudo

# pseudo %>% 
#   as_Spatial() %>% 
#   as_tibble() %>% 
#   transmute(id = 1:n(), lon = coords.x1, lat = coords.x2) %>% 
#   write_csv(file = "data/Oceanic whitetip peudo_absence.csv")
#
# st_write(mod_ext, "~/Desktop/mod_ext.GeoJSON")

pseudo_vect <- 
  read_csv("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/Oceanic%20whitetip%20peudo_absence.csv") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
  vect()

## --------------------------------------------------------------------------------------------------- ##
## Configure environmental data
# mod_ext <- vect("~/Desktop/mod_ext.GeoJSON")
# 
# ras <- rast("~/Desktop/Workshop layers/Primary productivity_mean.tif")
# ras_crop <-
#   ras %>%
#   crop(mod_ext)
# plot(ras_crop)
# writeRaster(x = ras_crop, filename = "~/Desktop/env_layers/primary_productivity.tif")

# env_stack <-
#   list.files("~/Desktop/env_layers", full.names = T) %>%
#   rast()
# 
# names(env_stack) <- c("bathymetry", "current_velocity", "mixed_layer_depth", "temperature")
# plot(env_stack)
# 
# writeRaster(env_stack, "data/env_layers.tif", overwrite = T)

env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/env_layers.tif")

ext_occ <- extract(env_stack, occ_vect)
ext_pseudo <- extract(env_stack, pseudo_vect)

full_dataset <- 
  as_tibble(ext_occ) %>% mutate(pa = 1, lat = occ_vect$lat, lon = occ_vect$lon, test_train = kfold(ext_occ, 5)) %>% 
  bind_rows(as_tibble(ext_pseudo) %>% mutate(pa = 0, lat = pseudo_vect$lat, lon = pseudo_vect$lon, test_train = kfold(ext_pseudo, 5))) %>% 
  dplyr::select(-ID) %>% 
  mutate(test_train = case_when(test_train %in% 1 ~ "test", TRUE ~ "train"))

# write_csv(full_dataset, "data/model_data.csv")

## --------------------------------------------------------------------------------------------------- ##
## Build models

library(mgcv)
library(stats)
library(dismo)

model_data <- read_csv("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/model_data.csv")












