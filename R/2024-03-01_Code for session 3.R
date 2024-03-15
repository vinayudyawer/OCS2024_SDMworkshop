## Setup data and generate plots for session 3

library(tidyverse)
library(lubridate)
library(mapview)
library(sf)
library(terra)
library(rnaturalearth)
library(dismo)
library(ggspatial)
library(patchwork)

esri_sat <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Imagery/MapServer/tile/${z}/${y}/${x}.jpeg')

esri_ngeo <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')

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

occ_plot <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(mod_ext, fill = NA, col = "white", lwd = 0) +
  layer_spatial(occ_vect, color = "black") +
  annotation_scale(width_hint = 0.2, location = "br") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme_void()

ggsave("images/session_3/1_occ_plot.png", occ_plot, width = 6, height = 5)

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
mod_ext <- st_read("~/Desktop/mod_ext.GeoJSON")

pseudo_vect <- 
  read_csv("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/Oceanic%20whitetip%20peudo_absence.csv") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
  vect()

pseudo_plot <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(pseudo_vect, color = "red", alpha = 0.3) +
  layer_spatial(occ_vect, color = "black") +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  annotation_scale(width_hint = 0.2, location = "br") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme_void()

ggsave("images/session_3/2_pseudo_plot.png", pseudo_plot, width = 6, height = 5)

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
# env_stack <- aggregate(env_stack, fact = 4)
# plot(env_stack)
# writeRaster(env_stack, "data/env_layers.tif", overwrite = T)


env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/env_layers.tif")


eplot4 <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  # layer_spatial(pseudo_vect, color = "red", alpha = 0.3) +
  # layer_spatial(occ_vect, color = "white") +
  layer_spatial(data = env_stack[[4]]) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  annotation_scale(width_hint = 0.2, location = "br") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_fill_viridis_c(na.value = NA, option = "H") +
  labs(title = "Sea Surface Temperature") +
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())

eplot <- eplot1 + eplot2 + eplot3 + eplot4 + plot_layout(nrow = 1)
ggsave("images/session_3/3_env_plot.png", eplot, width = 20, height = 5)

ext_occ <- extract(env_stack, occ_vect)
ext_pseudo <- extract(env_stack, pseudo_vect)

full_dataset <- 
  as_tibble(ext_occ) %>% mutate(pa = 1, lat = occ_vect$lat, lon = occ_vect$lon, test_train = kfold(ext_occ, 5)) %>% 
  bind_rows(as_tibble(ext_pseudo) %>% mutate(pa = 0, lat = pseudo_vect$lat, lon = pseudo_vect$lon, test_train = kfold(ext_pseudo, 5))) %>% 
  dplyr::select(-ID) %>% 
  mutate(test_train = case_when(test_train %in% 1 ~ "test", TRUE ~ "train")) %>% 
  transmute(pa, lon, lat, bathymetry, current_velocity, mixed_layer_depth, temperature, test_train) %>% 
  na.omit()

# write_csv(full_dataset, "data/model_data.csv")

## --------------------------------------------------------------------------------------------------- ##
## Build models

## Explore interactions

library(mgcv)
library(randomForest)
library(dismo)
library(visreg)

model_data <- 
  read_csv("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/model_data.csv") %>% 
  mutate(pa = factor(pa))

training_data <- model_data %>% filter(test_train %in% "train")

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

## GLM model
glm_mod <- glm(pa ~ bathymetry + current_velocity + mixed_layer_depth + temperature, 
               data = training_data, family = binomial(link = "logit"))

summary(glm_mod)

a <- visreg(glm_mod, xvar = "bathymetry", scale = "response", xlab = "Bathymetry (m)", ylab = "Probability of presence", gg = T) + theme_bw()
b <- visreg(glm_mod, xvar = "current_velocity", scale = "response", xlab = "Current Velocity (ms-1)", ylab = NULL, gg = T) + theme_bw()
c <- visreg(glm_mod, xvar = "mixed_layer_depth", scale = "response", xlab = "Mixed Layer Depth (m)", ylab = NULL, gg = T) + theme_bw()
d <- visreg(glm_mod, xvar = "temperature", scale = "response", xlab = "Sea Surface Temperature (˚C)", ylab = NULL, gg = T) + theme_bw()

glm_plot <- (a + b + c + d) + plot_layout(nrow = 1)
ggsave("images/session_3/4_glm_visreg.png", glm_plot, width = 20, height = 5)

visreg2d(glm_mod, xvar = "bathymetry", yvar = "temperature", plot.type = "persp", scale = "response",
         xlab = "Bathymetry (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Probability of presence",
         theta = 145, phi = 15, zlim = c(0,1))

glm_eval <- evaluate(p = test_presence, a = test_absence, model = glm_mod)

plot(glm_eval, "ROC", type = "l")

glm_threshold <- threshold(glm_eval, stat = 'spec_sens')

glm_resp <- terra::predict(env_stack, glm_mod, type = "response")
glm_prediction <- terra::predict(env_stack, glm_mod, type = "link")
glm_tmap <- glm_prediction > glm_threshold
values(glm_tmap)[values(glm_tmap) < 1] <- NA

glm_a <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(glm_resp) +
  # layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_viridis_c(na.value = NA, name = "Probability\nof presence") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15), legend.title.position = "top",
        legend.text = element_text(color = "white"), legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())

glm_b <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(glm_tmap) +
  layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_gradient(low = "darkgreen", high = "forestgreen", na.value = NA) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())

glm_pred <- glm_a + glm_b
ggsave("images/session_3/7_glm_map.png", glm_pred, width = 12, height = 5)


## GAM model
gam_mod <- gam(pa ~ s(bathymetry) + s(current_velocity) + s(mixed_layer_depth) + s(temperature),
               data = training_data, family = binomial(link = "logit"))

summary(gam_mod)

a <- visreg(gam_mod, xvar = "bathymetry", scale = "response", xlab = "Bathymetry (m)", ylab = "Probability of presence", gg = T) + theme_bw()
b <- visreg(gam_mod, xvar = "current_velocity", scale = "response", xlab = "Current Velocity (ms-1)", ylab = NULL, gg = T) + theme_bw()
c <- visreg(gam_mod, xvar = "mixed_layer_depth", scale = "response", xlab = "Mixed Layer Depth (m)", ylab = NULL, gg = T) + theme_bw()
d <- visreg(gam_mod, xvar = "temperature", scale = "response", xlab = "Sea Surface Temperature (˚C)", ylab = NULL, gg = T) + theme_bw()

gam_plot <- (a + b + c + d) + plot_layout(nrow = 1)
ggsave("images/session_3/8_gam_visreg.png", gam_plot, width = 20, height = 5)

visreg2d(gam_mod, xvar = "bathymetry", yvar = "temperature", scale = "response", plot.type = "persp",
         xlab = "Bathymetry (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Probability of presence",
         theta = 145, phi = 15, zlim = c(0,1))

gam_eval <- evaluate(p = test_presence, a = test_absence, model = gam_mod)

plot(gam_eval, "ROC", type = "l")

gam_threshold <- threshold(gam_eval, stat = 'spec_sens')

gam_resp <- terra::predict(env_stack, gam_mod, type = "response")
gam_prediction <- terra::predict(env_stack, gam_mod, type = "link")
gam_tmap <- gam_prediction > gam_threshold
values(gam_tmap)[values(gam_tmap) < 1] <- NA

gam_a <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(gam_resp) +
  # layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_viridis_c(na.value = NA, name = "Probability\nof presence") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15), legend.title.position = "top",
        legend.text = element_text(color = "white"), legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())

gam_b <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(gam_tmap) +
  layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_gradient(low = "darkgreen", high = "forestgreen", na.value = NA) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())

gam_pred <- gam_a + gam_b
ggsave("images/session_3/11_gam_map.png", gam_pred, width = 12, height = 5)


## Random Forest
rf_mod <- randomForest(pa ~ bathymetry + current_velocity + mixed_layer_depth + temperature,
                       data = model_data, ntree = 1000, nodesize = 10, importance = T)

rf_mod
visreg(rf_mod)

a <- visreg(rf_mod, xvar = "bathymetry", xlab = "Bathymetry (m)", ylab = "Probability of presence", gg = T) + theme_bw()
b <- visreg(rf_mod, xvar = "current_velocity", xlab = "Current Velocity (ms-1)", ylab = NULL, gg = T) + theme_bw()
c <- visreg(rf_mod, xvar = "mixed_layer_depth", xlab = "Mixed Layer Depth (m)", ylab = NULL, gg = T) + theme_bw()
d <- visreg(rf_mod, xvar = "temperature", xlab = "Sea Surface Temperature (˚C)", ylab = NULL, gg = T) + theme_bw()

rf_plot <- (a + b + c + d) + plot_layout(nrow = 1)
ggsave("images/session_3/12_rf_visreg.png", rf_plot, width = 20, height = 5)

visreg2d(rf_mod, xvar = "bathymetry", yvar = "temperature", plot.type = "persp",
         xlab = "Bathymetry (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Probability of presence",
         theta = 145, phi = 15)

rf_eval <- evaluate(p = test_presence, a = test_absence, model = rf_mod, type = "prob")

plot(rf_eval, "ROC", type = "l")

rf_resp <- terra::predict(env_stack, rf_mod, type = "prob")
rf_tmap <- terra::predict(env_stack, rf_mod, type = "response")
# values(rf_tmap)[values(rf_tmap) %in% 2] <- NA
rf_pmap <- rf_resp[[2]]

rf_a <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(rf_pmap) +
  # layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_viridis_c(na.value = NA, name = "Probability\nof presence") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15), legend.title.position = "top",
        legend.text = element_text(color = "white"), legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())

rf_b <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(rf_tmap) +
  layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_manual(values = c(NA, "forestgreen"), na.value = NA) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())

rf_pred <- rf_a + rf_b
ggsave("images/session_3/15_rf_map.png", rf_pred, width = 12, height = 5)

## Maxent model
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

predictors <- stack(env_stack)

maxent_mod <- maxent(x = predictors, p = presence_data, a = absence_data)

maxent_mod

a <-
  response(maxent_mod, at = mean, var = "bathymetry") %>% 
  bind_cols(response(maxent_mod, at = sd, var = "bathymetry")[,2]) %>% 
  rename(x = V1, mean = p, sd = 3) %>% 
  mutate(hi = mean + sd, lo = mean - sd) %>% 
  ggplot() + 
  geom_ribbon(aes(x = x, ymin = lo, ymax = hi), fill = "lightgrey") +
  geom_path(aes(x = x, y = mean), col = "#619CFF", lwd = 1) +
  geom_rug(data = as_tibble(presence_data), aes(x = bathymetry), sides = "t", inherit.aes = F, col = "darkgrey") +
  geom_rug(data = as_tibble(absence_data), aes(x = bathymetry), sides = "b", inherit.aes = F, col = "darkgrey") +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Bathymetry (m)", y = "Probability of presence") +
  theme_bw()
b <-
  response(maxent_mod, at = mean, var = "current_velocity") %>%
  bind_cols(response(maxent_mod, at = sd, var = "current_velocity")[,2]) %>% 
  rename(x = V1, mean = p, sd = 3) %>% 
  mutate(hi = mean + sd, lo = mean - sd) %>% 
  ggplot() + 
  geom_ribbon(aes(x = x, ymin = lo, ymax = hi), fill = "lightgrey") +
  geom_path(aes(x = x, y = mean), col = "#619CFF", lwd = 1) +
  geom_rug(data = as_tibble(presence_data), aes(x = current_velocity), sides = "t", inherit.aes = F, col = "darkgrey") +
  geom_rug(data = as_tibble(absence_data), aes(x = current_velocity), sides = "b", inherit.aes = F, col = "darkgrey") +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Current Velocity (ms-1)", y = "Probability of presence") +
  theme_bw()
c <-
  response(maxent_mod, at = mean, var = "mixed_layer_depth") %>%
  bind_cols(response(maxent_mod, at = sd, var = "mixed_layer_depth")[,2]) %>% 
  rename(x = V1, mean = p, sd = 3) %>% 
  mutate(hi = mean + sd, lo = mean - sd) %>% 
  ggplot() + 
  geom_ribbon(aes(x = x, ymin = lo, ymax = hi), fill = "lightgrey") +
  geom_path(aes(x = x, y = mean), col = "#619CFF", lwd = 1) +
  geom_rug(data = as_tibble(presence_data), aes(x = mixed_layer_depth), sides = "t", inherit.aes = F, col = "darkgrey") +
  geom_rug(data = as_tibble(absence_data), aes(x = mixed_layer_depth), sides = "b", inherit.aes = F, col = "darkgrey") +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Mixed Layer Depth (m)", y = "Probability of presence") +
  theme_bw()
d <-
  response(maxent_mod, at = mean, var = "temperature") %>%
  bind_cols(response(maxent_mod, at = sd, var = "temperature")[,2]) %>% 
  rename(x = V1, mean = p, sd = 3) %>% 
  mutate(hi = mean + sd, lo = mean - sd) %>% 
  ggplot() + 
  geom_ribbon(aes(x = x, ymin = lo, ymax = hi), fill = "lightgrey") +
  geom_path(aes(x = x, y = mean), col = "#619CFF", lwd = 1) +
  geom_rug(data = as_tibble(presence_data), aes(x = temperature), sides = "t", inherit.aes = F, col = "darkgrey") +
  geom_rug(data = as_tibble(absence_data), aes(x = temperature), sides = "b", inherit.aes = F, col = "darkgrey") +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Sea Surface Temperature (˚C)", y = "Probability of presence") +
  theme_bw()

maxent_plot <- (a + b + c + d) + plot_layout(nrow = 1)
ggsave("images/session_3/16_maxent_visreg.png", maxent_plot, width = 20, height = 5)

max_eval <- evaluate(p = test_presence, a = test_absence, model = maxent_mod)

plot(max_eval, "ROC", type = "l")
plot(maxent_mod)

max_threshold <- threshold(max_eval, stat = 'spec_sens')

max_pred <- terra::predict(maxent_mod, env_stack)
max_tmap <- max_pred > max_threshold
values(max_tmap)[values(max_tmap) < 1] <- NA


max_a <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(max_pred) +
  # layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_viridis_c(na.value = NA, name = "Probability\nof presence") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15), legend.title.position = "top",
        legend.text = element_text(color = "white"), legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())

max_b <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(max_tmap) +
  layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_gradient(low = "darkgreen", high = "forestgreen", na.value = NA) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())

max_pl <- max_a + max_b
ggsave("images/session_3/18_max_map.png", max_pl, width = 12, height = 5)


## Ensemble models

model_maps <- 
  (glm_a + labs(title = "GLM (AUC = 0.675)")) +
  (gam_a + labs(title = "GAM (AUC = 0.81)")) +
  (rf_a + labs(title = "Random Forest (AUC = 0.5)")) +
  (max_a + labs(title = "Maxent (AUC = 0.797)")) +
  plot_layout(nrow = 1)

ggsave("images/session_3/19_ensemble_plot.png", model_maps, width = 20, height = 5)


models <- c(glm_resp, gam_resp, rf_pmap, max_pred)
names(models) <- c("GLM", "GAM", "RF", "Max")

average_model <- mean(models)
average_tmap <- average_model> 0.5
values(average_tmap)[values(average_tmap) < 1] <- NA

w_average_model <- weighted.mean(models, w = c(0.675, 0.81, 0.5, 0.797))
w_average_tmap <- w_average_model> 0.5
values(w_average_tmap)[values(w_average_tmap) < 1] <- NA

plot(c(average_model, w_average_model, average_tmap, w_average_tmap))

ens_a <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(average_model) +
  # layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_viridis_c(na.value = NA, name = "Probability\nof presence") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  labs(title = "Mean model") +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15), legend.title.position = "top",
        legend.text = element_text(color = "white"), legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())

ens_am <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(average_tmap) +
  layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_gradient(low = "darkgreen", high = "forestgreen", na.value = NA) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())


ens_w <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(w_average_model) +
  # layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_viridis_c(na.value = NA, name = "Probability\nof presence") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  labs(title = "Weighted mean model") +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15), legend.title.position = "top",
        legend.text = element_text(color = "white"), legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())

ens_wm <-
  ggplot() + 
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(w_average_tmap) +
  layer_spatial(occ_vect, color = "white", alpha = 0.5) +
  layer_spatial(mod_ext, fill = NA, col = "black", lwd = 0.5) +
  # annotation_scale(width_hint = 0.2, location = "br") +
  scale_fill_gradient(low = "darkgreen", high = "forestgreen", na.value = NA) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())

ens_plot <- ens_a + ens_w + ens_am + ens_wm + plot_layout(nrow = 2, ncol = 2)
ggsave("images/session_3/20_ens_map.png", ens_plot, width = 12, height = 10)
