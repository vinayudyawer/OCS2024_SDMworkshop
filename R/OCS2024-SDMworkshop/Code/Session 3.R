## Code for Session 3


## --------------------------------------------------------------------------------------------------------- ##
## Lets first load some useful packages
library(tidyverse)
library(sf)
library(terra)
library(raster)
library(ggspatial)
library(dismo)
library(stats)
library(randomForest)
library(mgcv)
library(visreg)


## --------------------------------------------------------------------------------------------------------- ##
##### ***Occurrence data***
## We can call on the data from the workshop GitHub page directly

occ <- read_csv("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/Oceanic%20whitetip%20occ.csv")

occ_vect <-
  occ %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
  vect()

ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(occ_vect, color = "black") +
  annotation_scale(width_hint = 0.2, location = "br") +
  theme_void()


## --------------------------------------------------------------------------------------------------------- ##
##### ***Absence or Pseudo-absence data***

## Lets define our model extent to within our occurrence data
model_extent <- st_read("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/mod_ext.GeoJSON")

## upload pseudo absence points from the GitHub page
pseudo_vect <- 
  read_csv("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/Oceanic%20whitetip%20peudo_absence.csv") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
  vect()


ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(pseudo_vect, color = "red", alpha = 0.3) +
  layer_spatial(occ_vect, color = "black") +
  layer_spatial(model_extent, fill = NA, col = "black", lwd = 0.5) +
  annotation_scale(width_hint = 0.2, location = "br") +
  theme_void()


## --------------------------------------------------------------------------------------------------------- ##
##### ***Environmental predictors***
## Lets download the example environmental variables and plot them
env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/env_layers.tif")

plot(env_stack)


## --------------------------------------------------------------------------------------------------------- ##
#### Model fitting

## Lets extract environmental variables associated with all the occurrence and pseudo-absence data

# occurrence data extraction
ext_occ <- 
  extract(env_stack, occ_vect) %>% 
  mutate(pa = 1, 
         lat = occ_vect$lat, 
         lon = occ_vect$lon, 
         kfold = kfold(ext_occ, 5),
         test_train = ifelse(kfold == 1, "test", "train"))

# pseudo-absence data extraction
ext_pseudo <- 
  extract(env_stack, pseudo_vect) %>% 
  mutate(pa = 0, 
         lat = pseudo_vect$lat, 
         lon = pseudo_vect$lon, 
         kfold = kfold(ext_pseudo, 5),
         test_train = ifelse(kfold == 1, "test", "train"))

# put together the dataset and configure them to build models
model_data <- 
  bind_rows(ext_occ, ext_pseudo) %>% 
  as_tibble() %>% 
  transmute(pa = factor(pa), lon, lat, bathymetry, current_velocity, 
            mixed_layer_depth, temperature, test_train) %>% 
  na.omit()


## --------------------------------------------------------------------------------------------------------- ##
## Now we can partition our data into 'training' datasets (to model) 
## and 'testing' datasets (to evaluate models)

training_data <- 
  model_data %>% 
  filter(test_train %in% "train")

test_presence <- 
  model_data %>% 
  filter(test_train %in% "test") %>% 
  filter(pa %in% 1) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  as_Spatial()

test_absence <-
  model_data %>% 
  filter(test_train %in% "test") %>% 
  filter(pa %in% 0) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  as_Spatial()



## --------------------------------------------------------------------------------------------------------- ##
##### Linear models

## Build GLM model
glm_mod <- glm(pa ~ bathymetry + current_velocity + mixed_layer_depth + temperature, 
               data = training_data, family = binomial(link = "logit"))


summary(glm_mod)

visreg(glm_mod, scale = "response", gg = TRUE)

visreg2d(glm_mod, xvar = "bathymetry", yvar = "temperature", plot.type = "persp", scale = "response",
         xlab = "Bathymetry (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Probability of presence",
         theta = 145, phi = 15, zlim = c(0,1))

## using the evaluate() function in the `dismo` package
glm_eval <- evaluate(p = test_presence, a = test_absence, model = glm_mod)

plot(glm_eval, "ROC", type = "l")

## We can predict and plot the model response (continuous value between 0 and 1)
glm_predict <- terra::predict(env_stack, glm_mod, type = "response")

plot(glm_predict)

## We can now threshold the output to a map identifying the species distribution
# we need to first predict the model 'link' function
glm_link <- terra::predict(env_stack, glm_mod, type = "link")

# define the threshold using the evaluation metrics estimated earlier
glm_threshold <- threshold(glm_eval, stat = 'spec_sens')

plot(glm_link > glm_threshold)


## --------------------------------------------------------------------------------------------------------- ##
##### Non-linear models             

## Build GAM model
gam_mod <- gam(pa ~ s(bathymetry) + s(current_velocity) + s(mixed_layer_depth) + s(temperature),
               data = training_data, family = binomial(link = "logit"))


summary(gam_mod)

visreg(gam_mod, scale = "response", gg = TRUE)

visreg2d(gam_mod, xvar = "bathymetry", yvar = "temperature", scale = "response", plot.type = "persp",
         xlab = "Bathymetry (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Probability of presence",
         theta = 145, phi = 15, zlim = c(0,1))

## using the evaluate() function in the `dismo` package
gam_eval <- evaluate(p = test_presence, a = test_absence, model = gam_mod)

plot(gam_eval, "ROC", type = "l")

## We can predict and plot the model response (continuous value between 0 and 1)
gam_predict <- terra::predict(env_stack, gam_mod, type = "response")

plot(gam_predict)

## We can now threshold the output to a map identifying the species distribution
# we need to first predict the model 'link' function
gam_link <- terra::predict(env_stack, gam_mod, type = "link")

# define the threshold using the evaluation metrics estimated earlier
gam_threshold <- threshold(gam_eval, stat = 'spec_sens')

plot(gam_link > gam_threshold)


## --------------------------------------------------------------------------------------------------------- ##
##### Classification models

## Build Random Forest model
rf_mod <- randomForest(pa ~ bathymetry + current_velocity + mixed_layer_depth + temperature,
                       data = model_data, ntree = 1000, nodesize = 10, importance = T)


rf_mod

visreg(rf_mod, gg = TRUE)

visreg2d(rf_mod, xvar = "bathymetry", yvar = "temperature", plot.type = "persp",
         xlab = "Bathymetry (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Probability of presence",
         theta = 145, phi = 15)

## using the evaluate() function in the `dismo` package
rf_eval <- evaluate(p = test_presence, a = test_absence, model = rf_mod, type = "prob")

plot(rf_eval, "ROC", type = "l")

## We can predict and plot the model probability (continuous value between 0 and 1)
rf_predict <- terra::predict(env_stack, rf_mod, type = "prob")

plot(rf_predict[[2]])

## We can now threshold the output to a map identifying the species distribution
# for randomForest models we model the 'response' to get the thresholded output
rf_link <- terra::predict(env_stack, rf_mod, type = "response")

plot(rf_link)



## --------------------------------------------------------------------------------------------------------- ##
##### Maximum Entropy                      

## Setup the input data to enable the maxent() function in `dismo`
presence_data <- 
  training_data %>% 
  filter(pa %in% 1) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  as_Spatial()

absence_data <- 
  training_data %>% 
  filter(pa %in% 0) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  as_Spatial()

predictors <- raster::stack(env_stack)

## Build the MaxEnt model
maxent_mod <- maxent(x = predictors, p = presence_data, a = absence_data)

maxent_mod

response(maxent_mod)

## using the evaluate() function in the `dismo` package
maxent_eval <- evaluate(p = test_presence, a = test_absence, model = maxent_mod)

plot(maxent_eval, "ROC", type = "l")
plot(maxent_mod)

## We can predict and plot the model probability (continuous value between 0 and 1)
maxent_predict <- terra::predict(maxent_mod, env_stack)

plot(maxent_predict)

## We can now threshold the output to a map identifying the species distribution
# define the threshold using the evaluation metrics estimated earlier
maxent_threshold <- threshold(maxent_eval, stat = 'spec_sens')

plot(maxent_predict > maxent_threshold)


## --------------------------------------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------------------------------------- ##
#### **Ensemble Models**

## Lets create a raster stack compiling all our predictions so far
models <- c(glm_predict, gam_predict, rf_predict, maxeny_predict)

names(models) <- c("GLM", "GAM", "RF", "MaxEnt")

plot(models)

## Simple average model
average_model <- mean(models)
average_threshold_map <- average_model> 0.5

plot(average_model)
plot(average_threshold_map)


## Now with a weighted mean model (weighted using model AUCs)
w_average_model <- weighted.mean(models, w = c(0.675, 0.81, 0.5, 0.797))
w_average_threshold_map <- w_average_model> 0.5

plot(w_average_model)
plot(w_average_threshold_map)





## --------------------------------------------------------------------------------------------------------- ##
## --------------------------------------  END OF SESSION  ------------------------------------------------- ##
## --------------------------------------------------------------------------------------------------------- ##







