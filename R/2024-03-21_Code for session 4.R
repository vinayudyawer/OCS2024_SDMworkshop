## Session 4 script

library(tidyverse)
library(terra)
library(ggspatial)
library(plotly)
library(patchwork)

## ---------------------------------------------------------------------------------------------------------------- ##
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

## ---------------------------------------------------------------------------------------------------------------- ##
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
  facet_wrap(~id) +
  theme_void()

## ---------------------------------------------------------------------------------------------------------------- ##
## Input environmental data
env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/env_layers.tif")

# quick plot

plot(env_stack)


## ---------------------------------------------------------------------------------------------------------------- ##
## extract environmental data for each occurrence, pseudo-absence and background point

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


## ---------------------------------------------------------------------------------------------------------------- ##
# combine into a single data frame to allow for visualising and modelling

# model_data <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_4/model_data.csv')

model_data <- 
  bind_rows(tracks_env, null_env, bg_env) %>% 
  mutate(presence = case_when(type %in% c("occurrence") ~ 1,
                              TRUE ~ 0)) %>% 
  filter_at(vars(bathymetry, current_velocity, mixed_layer_depth, temperature), all_vars(!is.na(.)))
  

# lets look at the distrutbutions of the environmental variables
model_data %>% ggplot(aes(x = bathymetry)) + geom_histogram() + theme_bw() +
model_data %>% ggplot(aes(x = temperature)) + geom_histogram() + theme_bw() +
model_data %>% ggplot(aes(x = current_velocity)) + geom_histogram() + theme_bw() +
model_data %>% ggplot(aes(x = mixed_layer_depth)) + geom_histogram() + theme_bw()


# transform data to correct skewness in variables
# trans_data <-
#   model_data %>% 
#   mutate(current_velocity = log10(current_velocity),
#          temperature = exp(temperature),
#          mixed_layer_depth = log10(mixed_layer_depth))
# 
# 
# trans_data %>% ggplot(aes(x = bathymetry)) + geom_histogram() + theme_bw() +
# trans_data %>% ggplot(aes(x = temperature)) + geom_histogram() + theme_bw() +
# trans_data %>% ggplot(aes(x = current_velocity)) + geom_histogram() + theme_bw() +
# trans_data %>% ggplot(aes(x = mixed_layer_depth)) + geom_histogram() + theme_bw()
# 
# lets visualise the data in environmental space

model_data %>% 
  plot_ly(x = ~bathymetry, 
        y = ~temperature,
        z = ~current_velocity,
        color = ~type) %>% 
  add_markers()


## split data into test and train datasets
?kfold

## Now lets build a model
library(mgcv)

gamm_mod <- gamm(presence ~ s(bathymetry) + s(temperature) + 
                   s(current_velocity) + s(mixed_layer_depth), 
                 data = model_data,
                 random = list(id = ~id),
                 method = "REML", niterPQL = 1,
                 family = binomial("logit"))


mod <- gamm_mod$gam

## response curves
library(mgcViz)

mod_viz <- getViz(mod)

print(
  plot(mod_viz) + 
    l_ciPoly(alpha = 0.5) + 
    l_fitLine() + 
    l_rug() +
    theme_bw(), 
  pages = 1)


## using the evaluate() function in the `dismo` package
gamm_eval <- dismo::evaluate(p = test_presence, a = test_absence, model = mod)

plot(gam_eval, "ROC", type = "l")


pred <- terra::predict(env_stack, mod)

plot(pred)





