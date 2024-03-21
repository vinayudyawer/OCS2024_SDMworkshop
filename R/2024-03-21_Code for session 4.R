## Session 4 script

library(tidyverse)
library(terra)
library(ggspatial)
library(plotly)
library(patchwork)
library(dismo)
library(visreg)

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

# a <- 
ggplot() +
  annotation_map_tile('cartolight', zoom = 4) +
  layer_spatial(model_extent, fill = NA) +
  geom_spatial_point(data = background, aes(x = lon, y = lat), col = "grey", crs = 4326) +
  geom_spatial_point(data = null_tracks, aes(x = lon, y = lat), col = "pink", crs = 4326) +
  geom_spatial_point(data = tracks, aes(x = lon, y = lat), col = "firebrick", crs = 4326) +
  facet_wrap(~id) +
  theme_void()

# ggsave("images/session_4/1_plot.png", width = 10, height = 6)

## ---------------------------------------------------------------------------------------------------------------- ##
## Input environmental data
env_stack <- rast("https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/session_3/env_layers.tif")
# env_stack$lon <- rep(seq(30.2, 108.4, by = 0.2), dim(env_stack)[1])
# env_stack$lat <- rep(seq(-32.55, 30.65, by = 0.2), each = dim(env_stack)[2])

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
  

## ---------------------------------------------------------------------------------------------------------------- ##
# lets look at the distrutbutions of the environmental variables
# a <-
model_data %>% ggplot(aes(x = bathymetry)) + geom_histogram() + theme_bw() +
model_data %>% ggplot(aes(x = temperature)) + geom_histogram() + theme_bw() +
model_data %>% ggplot(aes(x = current_velocity)) + geom_histogram() + theme_bw() +
model_data %>% ggplot(aes(x = mixed_layer_depth)) + geom_histogram() + theme_bw() 

# ggsave("images/session_4/3_plot.png", a, width = 8, height = 6)

# transform data to correct skewness in variables
trans_data <-
  model_data %>%
  mutate(current_velocity = log10(current_velocity),
         temperature = exp(temperature),
         mixed_layer_depth = log10(mixed_layer_depth))


# b <- 
trans_data %>% ggplot(aes(x = bathymetry)) + geom_histogram() + theme_bw() +
trans_data %>% ggplot(aes(x = temperature)) + geom_histogram() + theme_bw() +
trans_data %>% ggplot(aes(x = current_velocity)) + geom_histogram() + theme_bw() +
trans_data %>% ggplot(aes(x = mixed_layer_depth)) + geom_histogram() + theme_bw()

# ggsave("images/session_4/4_plot.png", b, width = 8, height = 6)

## we also have to transform the env_stack
env_trans <- env_stack

env_trans$current_velocity <- log10(env_trans$current_velocity)
env_trans$temperature <- exp(env_trans$temperature)
env_trans$mixed_layer_depth <- log10(env_trans$mixed_layer_depth)

plot(env_trans)

# lets visualise the data in environmental space

trans_data %>% 
  plot_ly(x = ~bathymetry, 
        y = ~temperature,
        z = ~current_velocity,
        color = ~type) %>% 
  add_markers()


## ---------------------------------------------------------------------------------------------------------------- ##
## split data into test and train datasets
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


## ---------------------------------------------------------------------------------------------------------------- ##
## Now lets build a model
library(mgcv)

gamm_mod <- gamm(presence ~ s(bathymetry) + s(temperature) + 
                   s(current_velocity) + s(mixed_layer_depth), 
                 data = training_data,
                 random = list(id = ~id),
                 method = "REML",
                 family = binomial("logit"))


mod <- gamm_mod$gam


## ---------------------------------------------------------------------------------------------------------------- ##
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


vis.gam(mod, view = c("bathymetry", "temperature"), theta = 145, phi = 15, type = "response")

vis.gam(mod, view = c("mixed_layer_depth", "current_velocity"), theta = 145, phi = 15, type = "response")


## ---------------------------------------------------------------------------------------------------------------- ##
## using the evaluate() function in the `dismo` package
gamm_eval <- dismo::evaluate(p = test_presence, a = test_absence, model = mod)

plot(gamm_eval, "ROC", type = "l")



## ---------------------------------------------------------------------------------------------------------------- ##
## We can predict and plot the model response (continuous value between 0 and 1)
gamm_resp <- terra::predict(env_trans, mod, type = "response")

plot(gamm_resp)

## We can now threshold the output to a map identifying the species distribution
# we need to first predict the model 'link' function
gamm_prediction <- terra::predict(env_trans, mod, type = "link")

plot(gamm_prediction)

# define the threshold using the evaluation metrics estimated earlier
gamm_threshold <- threshold(gamm_eval, stat = 'spec_sens')

plot(gamm_prediction > gamm_threshold)

## ---------------------------------------------------------------------------------------------------------------- ##
# gam_tmap <- gamm_prediction > gamm_threshold
# values(gam_tmap)[values(gam_tmap) < 1] <- NA
# 
# gam_a <-
#   ggplot() + 
#   annotation_map_tile('cartolight', zoom = 4) +
#   layer_spatial(gamm_resp) +
#   # layer_spatial(occ_vect, color = "white", alpha = 0.5) +
#   layer_spatial(model_extent, fill = NA, col = "black", lwd = 0.5) +
#   # annotation_scale(width_hint = 0.2, location = "br") +
#   scale_fill_viridis_c(na.value = NA, name = "Probability\nof presence") +
#   scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
#   scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
#   theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15), legend.title.position = "top",
#         legend.text = element_text(color = "white"), legend.title = element_text(color = "white"),
#         legend.background = element_rect(fill = NA, color = NA),
#         legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())
# 
# gam_b <-
#   ggplot() + 
#   annotation_map_tile('cartolight', zoom = 4) +
#   layer_spatial(gam_tmap) +
#   layer_spatial(tracks_vec %>% 
#                   st_as_sf() %>% 
#                   group_by(id) %>% 
#                   summarise(do_union=F) %>% 
#                   st_cast("LINESTRING"), color = "red", lwd = 1.5) +
#   layer_spatial(model_extent, fill = NA, col = "black", lwd = 0.5) +
#   # annotation_scale(width_hint = 0.2, location = "br") +
#   scale_fill_gradient(low = "darkgreen", high = "forestgreen", na.value = NA) +
#   scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
#   scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
#   theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())
# 
# gam_pred <- gam_a + gam_b
# ggsave("images/session_4/gamm_map.png", gam_pred, width = 12, height = 5)
## ---------------------------------------------------------------------------------------------------------------- ##

## modelling move persistance (g)

mp_mod <- gam(g ~ s(bathymetry) + s(temperature) + 
                s(current_velocity) + s(mixed_layer_depth), 
               data = trans_data, 
              family = quasibinomial("logit"))

# visreg::visreg(mp_mod, scale = "response", partial = T, gg = T)

a <- visreg(mp_mod, xvar = "bathymetry", scale = "response", partial = T, xlab = "Bathymetry (m)", ylab = bquote("Move persistance"~(gamma[t])), gg = T) + theme_bw()
b <- visreg(mp_mod, xvar = "current_velocity", scale = "response", partial = T,xlab = "Current Velocity (ms-1)", ylab = NULL, gg = T) + theme_bw()
c <- visreg(mp_mod, xvar = "mixed_layer_depth", scale = "response", partial = T,xlab = "Mixed Layer Depth (m)", ylab = NULL, gg = T) + theme_bw()
d <- visreg(mp_mod, xvar = "temperature", scale = "response", partial = T,xlab = "Sea Surface Temperature (˚C)", ylab = NULL, gg = T) + theme_bw()

gam_plot <- (a + b + c + d) + plot_layout(nrow = 1)
gam_plot
# ggsave("images/session_4/mp_visreg.png", gam_plot, width = 20, height = 5)

visreg2d(mp_mod, xvar = "mixed_layer_depth", yvar = "temperature", scale = "response", plot.type = "persp",
         xlab = "Mixed Layer Depth (m)", ylab = "Sea Surface Temparature (˚C)", zlab = "Probability of presence",
         theta = 145, phi = 15, zlim = c(0,1))


mp_resp <- terra::predict(env_trans, mp_mod, type = "response")

plot(mp_resp)

# mp_a <-
#   ggplot() +
#   annotation_map_tile('cartolight', zoom = 4) +
#   layer_spatial(mp_resp) +
#   geom_spatial_path(data = tracks, aes(x = lon, y = lat, group = id, color = g), crs = 4326, lwd = 1.5) +
#   layer_spatial(model_extent, fill = NA, col = "black", lwd = 0.5) +
#   # annotation_scale(width_hint = 0.2, location = "br") +
#   scale_color_viridis_c(option = "A", name = bquote("Track move\npersistance"~(gamma[t])), limits = c(0,1)) +
#   scale_fill_distiller(palette = "YlGnBu", direction = -1, na.value = NA, limits = c(0,1),
#                        name = bquote("Modelled move\npersistance"~(gamma[t]))) +
#   labs(x = NULL, y = NULL) +
#   scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
#   scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
#   theme(legend.position = "bottom", legend.title.position = "top",
#         legend.direction = "horizontal", axis.text = element_blank(), axis.ticks = element_blank())
# 
# ggsave("images/session_4/mp_cont.png", width = 7, height = 7)



