## Code for Session 4


## --------------------------------------------------------------------------------------------------------- ##
## Lets load up some useful packages

library(tidyverse)
library(terra)
library(ggspatial)
library(plotly)
library(patchwork)
library(visreg)
library(dismo)
library(mgcv)
library(mgcViz)


## --------------------------------------------------------------------------------------------------------- ##
#### Input occurrence, absence and environmental data

## Lets upload the cleaned up track data we processed in Session 2
tracks <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_4/track_data.csv')

## Lets also upload the null tracks we simulated in Session 2 
null_tracks <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_4/null_data.csv')

## Including the simulated tracks, we can also use background data (random points within the model extent)
background <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_4/bg_data.csv')

## And finally the spatial extent of our model
model_extent <- 
  vect("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/mod_ext.GeoJSON")


## Lets now produce a quick plot to show the presence (red), pseudo-absences (pink) and background (grey) datasets:
  
ggplot() +
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(model_extent, fill = NA) +
  geom_spatial_point(data = background, aes(x = lon, y = lat), col = "grey", crs = 4326) +
  geom_spatial_point(data = null_tracks, aes(x = lon, y = lat), col = "pink", crs = 4326) +
  geom_spatial_point(data = tracks, aes(x = lon, y = lat), col = "firebrick", crs = 4326) +
  facet_wrap(~id) +
  theme_void()



## --------------------------------------------------------------------------------------------------------- ##
#### Input environmental data
env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/env_layers.tif")

# quick plot

plot(env_stack)




## --------------------------------------------------------------------------------------------------------- ##
#### Extract environmental data for each occurrence, pseudo-absence and background point

tracks_vec <- vect(tracks, geom=c("lon", "lat"), crs = "EPSG:4326")
null_vec <- vect(null_tracks, geom=c("lon", "lat"), crs = "EPSG:4326")
bg_vec <- vect(background, geom=c("lon", "lat"), crs = "EPSG:4326")

tracks_env <- 
  tracks %>% 
  bind_cols(extract(env_stack, tracks_vec)[-1])

null_env <- 
  null_tracks %>% 
  bind_cols(extract(env_stack, null_vec)[-1])

bg_env <- 
  background %>% 
  bind_cols(extract(env_stack, bg_vec)[-1])



model_data <- 
  bind_rows(tracks_env, null_env, bg_env) %>% 
  mutate(presence = case_when(type %in% c("occurrence") ~ 1, TRUE ~ 0)) %>% 
  filter_at(vars(bathymetry, current_velocity, mixed_layer_depth, temperature), all_vars(!is.na(.)))

model_data


## --------------------------------------------------------------------------------------------------------- ##
#### Explore the distributions of the predictor variables

model_data %>% ggplot(aes(x = bathymetry)) + geom_histogram() + theme_bw() +
  model_data %>% ggplot(aes(x = temperature)) + geom_histogram() + theme_bw() +
  model_data %>% ggplot(aes(x = current_velocity)) + geom_histogram() + theme_bw() +
  model_data %>% ggplot(aes(x = mixed_layer_depth)) + geom_histogram() + theme_bw()


trans_data <-
  model_data %>%
  mutate(current_velocity = log10(current_velocity),
         temperature = exp(temperature),
         mixed_layer_depth = log10(mixed_layer_depth))


trans_data %>% ggplot(aes(x = bathymetry)) + geom_histogram() + theme_bw() +
  trans_data %>% ggplot(aes(x = temperature)) + geom_histogram() + theme_bw() +
  trans_data %>% ggplot(aes(x = current_velocity)) + geom_histogram() + theme_bw() +
  trans_data %>% ggplot(aes(x = mixed_layer_depth)) + geom_histogram() + theme_bw()


env_trans <- env_stack

env_trans$current_velocity <- log10(env_trans$current_velocity)
env_trans$temperature <- exp(env_trans$temperature)
env_trans$mixed_layer_depth <- log10(env_trans$mixed_layer_depth)

plot(env_trans)



## --------------------------------------------------------------------------------------------------------- ##
#### Visualise the ecological niche
library(plotly)

trans_data %>% 
  plot_ly(x = ~bathymetry, 
          y = ~temperature,
          z = ~current_velocity,
          color = ~type) %>% 
  add_markers()



## --------------------------------------------------------------------------------------------------------- ##
#### Creating a Species Distribution Model (SDM)

training_data <- 
  trans_data %>% 
  filter(test_train %in% "train")

test_presence <- 
  trans_data %>% 
  filter(test_train %in% "test") %>% 
  filter(presence %in% 1) %>% 
  vect(crs = "EPSG:4326")

test_absence <-
  trans_data %>% 
  filter(test_train %in% "test") %>% 
  filter(presence %in% 0) %>% 
  vect(crs = "EPSG:4326")


## --------------------------------------------------------------------------------------------------------- ##
##### ***Building the model***
library(mgcv)

## Lets build a GAMM with the simplist configuration of a non-linear model
gamm_mod <- gamm(presence ~ s(bathymetry) + s(temperature) + 
                   s(current_velocity) + s(mixed_layer_depth), 
                 data = training_data, ## training input data
                 random = list(id = ~id), ## id as a random variable
                 method = "REML", 
                 family = binomial("logit")) ## using a binomial framework


## Lets separate the GAM portion of the GAMM model 
mod <- gamm_mod$gam


library(mgcViz)

mod_viz <- getViz(mod)

print(
  plot(mod_viz) + 
    l_ciPoly(alpha = 0.5) + 
    l_fitLine() + 
    l_rug() +
    theme_bw(), 
  pages = 1)


vis.gam(mod, view = c("bathymetry", "temperature"), theta = 145, phi = 15, type = "response")

vis.gam(mod, view = c("mixed_layer_depth", "current_velocity"), theta = 145, phi = 15, type = "response")



## --------------------------------------------------------------------------------------------------------- ##
##### ***Evaluating the model***

library(dismo)

gamm_eval <- dismo::evaluate(p = test_presence, a = test_absence, model = mod)

plot(gamm_eval, "ROC", type = "l")



## --------------------------------------------------------------------------------------------------------- ##
##### ***Model prediction***

gamm_predict <- terra::predict(env_trans, mod, type = "response")

plot(gamm_predict)


## We can now threshold the output to a map identifying the species distribution
# we need to first predict the model 'link' function
gamm_link <- terra::predict(env_trans, mod, type = "link")

plot(gamm_link)

# define the threshold using the evaluation metrics estimated earlier
gamm_threshold <- threshold(gamm_eval, stat = 'spec_sens')

plot(gamm_link > gamm_threshold)



## --------------------------------------------------------------------------------------------------------- ##
#### Modelling move persistance (g)

mp_mod <- gam(g ~ s(bathymetry) + s(temperature) + 
                s(current_velocity) + s(mixed_layer_depth), 
               data = trans_data, 
              family = quasibinomial("logit"))

summary(mp_mod)


visreg::visreg(mp_mod, scale = "response", partial = T,  gg = T)


visreg2d(mp_mod, xvar = "mixed_layer_depth", yvar = "temperature", scale = "response", plot.type = "persp",
         xlab = "Mixed Layer Depth (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Move persistence",
         theta = 145, phi = 15, zlim = c(0,1))


mp_resp <- terra::predict(env_trans, mp_mod, type = "response")

plot(mp_resp)





## --------------------------------------------------------------------------------------------------------- ##
## --------------------------------------  END OF SESSION  ------------------------------------------------- ##
## --------------------------------------------------------------------------------------------------------- ##









