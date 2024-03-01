## Setup data and generate plots for session 3

library(tidyverse)
library(lubridate)
library(mapview)
library(sf)
library(terra)



## Input data

land <- 
  st_read("~/Dropbox/GIS Data/World/world.shp", crs = 4326) %>% 
  st_make_valid()



## occurrence data
occ <- 
  read_tsv("Data/Oceanic whitetip_gbif.csv") %>% 
  transmute(species, common_name = "Oceanic Whitetip Shark", 
            date_time = ymd_hm(eventDate),
            lon = decimalLongitude, lat = decimalLatitude, issue) %>% 
  filter(!is.na(lon)) %>% 
  filter(between(lon, 31.5, 105)) %>% 
  mutate(a = case_when(str_detect(issue, pattern = "CONTINENT_DERIVED_FROM_COORDINATES") ~ 1,
                       TRUE ~ 0)) %>% 
  filter(a %in% 0) %>% 
  select(-a, -issue)

occ_sf <-
  occ %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)

mapview(occ_sf)


# write_csv(occ, "Data/Oceanic whitetip occ.csv")


## pseudo-absence data


occ_buff <- 
  occ_sf %>% 
  st_buffer(dist = 30e4) %>% 
  st_union() %>% 
  st_as_sf()

mapview(occ_buff)



