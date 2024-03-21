## Code for Session 1

## --------------------------------------------------------------------------------------------------------- ##
# Load the whale shark data using the 'read.csv' function from your local repository
whaleshark <- read.csv('/location of data/Whalesharks_Maldives.csv', header = TRUE)

## --------------------------------------------------------------------------------------------------------- ##
# Load the whale shark data using the 'read.csv' function directly from github 
whaleshark <- read.csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/Whaleshark.csv', header = TRUE)

## --------------------------------------------------------------------------------------------------------- ##
# Now, let's do a quick exploration of the data using base R to get familiar with it. 
summary(whaleshark) # Provides a summary of the data frame 
class(whaleshark) # tells us what form 'whaleshark' is in (data frame)

## --------------------------------------------------------------------------------------------------------- ##
# Now lets explore what form each of our columns are in
class(whaleshark$ANIMALID) # character
class(whaleshark$DATE) # character
class(whaleshark$LONGITUDE) # numeric
class(whaleshark$LATITUDE) # numeric
class(whaleshark$ARGOSCLASS) # character

## --------------------------------------------------------------------------------------------------------- ##
#First, lets load up lubridate using the library package. 
library(lubridate)

# Now we can use the 'ymd_hms' function to reformat our 'DATE' field as a POSIXct. 
whaleshark$DATE <- ymd_hms(whaleshark$DATE)

# Or we can use the parse_date_time function
whaleshark$DATE <- parse_date_time(whaleshark$DATE, "Ymd HMS")

# We can check if it worked by using the class function
class(whaleshark$DATE)


## --------------------------------------------------------------------------------------------------------- ##
# Lets practice by creating a new field with the time zone as the local time (Indian Ocean/Maldives UTC +5:00). 
whaleshark$LOCAL_DATE_TIME <- with_tz(whaleshark$DATE, tzone = "Indian/Maldives")

# You may also be looking at a larger temporal resolution and want to separate the 'date' from the 'time' 
# field, we can also use the 'date' and 'time' functions in lubridate to do this!

whaleshark$date <- date(whaleshark$DATE) # Create a new column with only date 
whaleshark$time <- format(whaleshark$DATE, "%H:%M:%S") # create a new column with only time


## --------------------------------------------------------------------------------------------------------- ##
##### One B: `Tidyverse` package 

# Load 'tidyverse' - which we will use for data cleaning, filtering, and visualization 
library(tidyverse)

whaleshark <- read_csv('/location of data/Whalesharks_Maldives.csv')

# You can also use read_csv to input data directly from a website URL
whaleshark <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/Whaleshark.csv')

# Note, right now we are reading over our previous 'whaleshark' data frame. So any manipulations you've done to the data frame will be reset. 


## --------------------------------------------------------------------------------------------------------- ##
#### Two: Data Exploration 

whaleshark %>% class() # In English: take the 'whaleshark' data frame and run the class function on it. 


whaleshark %>% View() # opens data in a new window
whaleshark %>% head() # first 6 rows by default
whaleshark %>% tail(10) # specify we want to look at the last 10 rows
whaleshark %>% nrow() # number of rows in the data frame
whaleshark %>% ncol() # number of columns in the data frame
whaleshark %>% str() # provides internal structure of an R object
whaleshark %>% summary() # provides result summary of the data frame

whaleshark$DATE %>% class()

## --------------------------------------------------------------------------------------------------------- ##
#### Three: Data manipulation and filtering  

whaleshark %>% 
  subset(ANIMALID == "M-150") %>% # subset dataset to include only the whale shark 'M-150'
  nrow() # count number of rows (i.e. detections) from 'M-150'


whaleshark %>% # take the whale shark df
  mutate(date = date(DATE)) %>% # then create a new column called date from the date time column
  subset(ANIMALID == "M-150") %>% # then subset to just the data from shark M-150 
  with(table(date)) %>% # create a table with the number of detections per day
  plot(type = 'b', xlab = "Date", ylab = "Number of detections", col = 'lightblue') # plot detections over time 


## --------------------------------------------------------------------------------------------------------- ##
##### `dplyr` package: using dplyr for data wrangling
#Load up the `dplyr` package

library(dplyr)

# select
whaleshark %>% 
  dplyr::select(ANIMALID, DATE, LONGITUDE, LATITUDE)# %>% # columns we want to include
#  OR ALTERNATIVELY 
# dplyr::select(-ARGOSCLASS) # the minus symbol denotes columns we want to drop


# filter
whaleshark %>%
  filter(ANIMALID == 'M-150') %>%
  arrange(DATE) # arrange M-150's detections in chronological order

#`group_by` and `summarise`
whaleshark %>%
  group_by(ANIMALID) %>%
  summarise(NumDetections = n()) # summarise number of detections per tagged shark

#`mutate` 
whaleshark <-
  whaleshark %>% 
  rename(DATETIME = DATE) %>% # change the name of our DATETIME column
  mutate(DATE = as.Date(DATETIME)) %>% # add a column to the whale shark data with just date of each detection
  mutate(TIME = format(DATETIME, format = "%H:%M:%S")) # add a column for just time 


## --------------------------------------------------------------------------------------------------------- ##
#### Four: Data visualization using `ggplot`

library(ggplot2)  

help.search("geom_", package = "ggplot2")


## --------------------------------------------------------------------------------------------------------- ##
##### Basic plotting and data visualization 

whaleshark %>%
  group_by(ANIMALID, DATE) %>% 
  summarise(daily_detections = n()) %>% # use summarise to calculate numbers of detections per day per animal
  ggplot(mapping = aes(x = ANIMALID, y = daily_detections, color = ANIMALID)) + # define the aesthetic map (what to plot)
  xlab("Animal ID") + ylab("Number of detections per day") +
  geom_boxplot() + # define the geometric object (how to plot it).. in this case a boxplot 
  theme(legend.position = "none")


whaleshark %>%
  ggplot(mapping = aes(x = DATE, y = ANIMALID, color = ANIMALID)) + 
  xlab("Date") + ylab("Animal ID") +
  geom_point() +
  theme(legend.position = "none")


## --------------------------------------------------------------------------------------------------------- ##
#### Five: Spatial Mapping in **R**
## --------------------------------------------------------------------------------------------------------- ##

##### Five A: Introduction to the `sf` package for spatial mapping 

###### Using the `sf` package to create spatial objects
library(sf)

# `st_as_sf()` 

whaleshark_sf <-
  whaleshark %>% 
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs= 4326, remove = F)

head(whaleshark_sf)


## --------------------------------------------------------------------------------------------------------- ##
##### Five B: Plotting a spatial object using `ggplot2`

ggplot(whaleshark_sf, aes(color = ANIMALID)) + 
  geom_sf() +
  labs(x = "Longitude", y = "Latitutde")


whaleshark_path <- 
  whaleshark_sf %>%
  group_by(ANIMALID) %>% #Group by animal ID so that each animal has it's own unique path
  arrange(DATETIME) %>% # arrange by the date to ensure data are in the correct sequence 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") # converts our points sf to a path sf

      
ggplot() + 
  geom_sf(data = whaleshark_sf, aes(color = ANIMALID)) +
  geom_sf(data = whaleshark_path, aes(color = ANIMALID)) +
  labs(x = "Longitude", y = "Latitutde")
  

## --------------------------------------------------------------------------------------------------------- ##
##### Five C: Introduction to the `ggspatial package` for static spatial mapping 
library(ggspatial)

M150_sf <- whaleshark_sf %>%
  filter(ANIMALID == 'M-150')

M150_path <-
  M150_sf %>%
  arrange(DATETIME) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


###### ***OSM basemap*** 
ggplot() +
  annotation_map_tile(type = "osm") +
  layer_spatial(data = M150_sf) +
  layer_spatial(data = M150_path) +
  labs(x = "Longitude", y = "Latitude")


###### ***Carto basemap***
ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 8) +
  layer_spatial(data = M150_sf) +
  layer_spatial(data = M150_path) +
  labs(x = "Longitude", y = "Latitude")


###### ***Non-standard basemaps***
# 1. *ESRI Satellite imagery* 

esri_sat <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Imagery/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() +
  annotation_map_tile(type = esri_sat, zoom = 8) +
  layer_spatial(data = M150_sf, color = 'white')  +
  layer_spatial(data = M150_path, color = 'white') +
  labs(x = "Longitude", y = "Latitude")


# 2. *ESRI topographical imagery* 

esri_topo <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Topo_Map/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() +
  annotation_map_tile(type = esri_topo, zoom = 8) +
  layer_spatial(data = M150_sf)  +
  layer_spatial(data = M150_path) +
  labs(x = "Longitude", y = "Latitude")

ggplot() +
  annotation_map_tile(type = 'osm', zoom = 8) +
  layer_spatial(data = M150_sf) +
  layer_spatial(data = M150_path) +
  annotation_scale() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude")


## --------------------------------------------------------------------------------------------------------- ##
##### Six: Making dynamic maps using the `gganimate` package

library(gganimate)

ggplot() +
  annotation_map_tile(type = esri_sat) +
  layer_spatial(data = M150_sf, color = 'white')  +
  layer_spatial(data = M150_path, color = 'red') +
  annotation_scale(text_col = "white") +
  transition_time(DATETIME) +
  labs(x = "Longitude", y = "Latitude")


ggplot() +
  annotation_map_tile(type = esri_sat) +
  geom_spatial_path(data = M150_sf, aes(x = LONGITUDE, y = LATITUDE), crs = 4326, color = 'white')  +
  geom_spatial_point(data = M150_sf, aes(x = LONGITUDE, y = LATITUDE), crs = 4326, color = 'red') +
  annotation_scale(text_col = "white") +
  transition_reveal(DATETIME) +
  labs(x = "Longitude", y = "Latitude")


## --------------------------------------------------------------------------------------------------------- ##
##### Seven: Using the Mapview package to create interactive maps 

library(mapview)
mapviewOptions(fgb = FALSE) 

whaleshark_paths <-
  whaleshark_sf %>%
  arrange(DATETIME) %>%
  group_by(ANIMALID) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

mapview(whaleshark_sf, zcol = "ANIMALID")
mapview(whaleshark_paths)


map1<-
  mapview(whaleshark_sf,
          zcol = "ANIMALID",
          burst = T,
          map.types = "Esri.WorldImagery",
          legend = F,
          homebutton = F,
          cex = 5) +
  mapview(whaleshark_paths,
          zcol = "ANIMALID",
          burst = T,
          legend = F,
          homebutton = F)

map1


map1@object
map1@map


library(leaflet)

map2 <-
  map1@map %>%
  addLayersControl(baseGroups = unique(whaleshark_sf$ANIMALID),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(unique(whaleshark_sf$ANIMALID))

map2

mapview::mapshot(map2, url = "14_Spatial_Interactive2.html", remove_controls = NULL, selfcontained = TRUE)




## --------------------------------------------------------------------------------------------------------- ##
## --------------------------------------  END OF SESSION  ------------------------------------------------- ##
## --------------------------------------------------------------------------------------------------------- ##




