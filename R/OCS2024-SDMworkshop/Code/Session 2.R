## Code for Session 2


## --------------------------------------------------------------------------------------------------------- ##
#### Step 1: Input and format data 

library(aniMotum)

raw_data <- read_csv('https://raw.githubusercontent.com/vinayudyawer/OCS2024_SDMworkshop/main/data/Whaleshark.csv')

head(raw_data)


## --------------------------------------------------------------------------------------------------------- ##
##### Format the data 
# Using transmute from the Dplyr package 
tagdat <- raw_data %>%
  dplyr::transmute(id = ANIMALID,
                   date = DATE, 
                   lc= ARGOSCLASS,
                   lon = LONGITUDE, 
                   lat = LATITUDE)

# Using format_data from the aniMotum Package) 
tagdat2 <- format_data(
  raw_data,
  id = "ANIMALID",
  date = "DATE",
  lc = "ARGOSCLASS",
  coord = c("LONGITUDE", "LATITUDE")
)

head(tagdat)


tagdat %>% 
  group_by(id, lc) %>% 
  summarise(num_pos = n()) %>% 
  ggplot(aes(x = id, y = num_pos, fill = lc)) +
  geom_col(position = "fill") +
  labs(x = "Tag ID", y = "Proportion of fixes", fill = "Location\nClass") +
  theme_bw()


## --------------------------------------------------------------------------------------------------------- ##
#### Step 2: Choose and fit a movement model 

# Fitting a Continuous-time move persistence (MP) model to our data 
fit <-
  fit_ssm(x = tagdat, 
          vmax = 1.5, ## maximum speed of whale sharks (in m/s)
          model = "mp", ## Move persistence model
          time.step = 12, ## predict positions every 12 hours
          control = ssm_control(verbose = 0)) ## Lets turn off the progress text

fit


fit2 <-
  fit_ssm(x = tagdat, 
          vmax = 1.5, ## maximum speed of whale sharks (in m/s)
          model = "mp", ## Move persistence model
          time.step = 48, ## predict positions every 24 hours
          spdf = FALSE, ## turn off the pre-filter step
          control = ssm_control(verbose = 0)) ## Lets turn off the progress text

fit2


fit3 <- tagdat %>%
  dplyr::filter(id %in% c("M-150", "M-130")) %>%
  fit_ssm(vmax = 1.5, ## maximum speed of whale sharks (in m/s)
          model = "mp", ## Move persistence model
          time.step = 48, ## predict positions every 24 hours
          spdf = FALSE, ## turn off the pre-filter step
          control = ssm_control(verbose = 0)) ## Lets turn off the progress text

summary(fit3)

## Lets have a look at the fitted component of the model 
## (original data, corrected by including positional error)

plot(fit3, 
     what = "fitted", ## what component of the model to plot ('fitted', 'predicted' or 'rerouted')
     type = 2, ## type of plot to make
     pages = 1, 
     ncol = 2)



## --------------------------------------------------------------------------------------------------------- ##
#### Step 3: Check model fit 

resid <- osar(fit3)

## Lets check our model fit for both tracks
plot(resid, type = "qq")
plot(resid, type = "acf")


plot(fit3, 
     what = "predicted", 
     type = 2,
     pages = 1,
     ncol = 2)


## --------------------------------------------------------------------------------------------------------- ##
#### Step 4: Visualize move persistence estimates

plot(fit3,
     what = "predicted",
     type = 3,
     pages = 1,
     ncol = 2,
     normalise = TRUE)

plot(fit3,
     what = "predicted",
     type = 4,
     pages = 1,
     ncol = 2,
     normalise = TRUE)


# Lets plot our own version of the predicted component using mapview
pred_data <- grab(fit3,
                  what = "predicted",
                  normalise = TRUE)

# Lets convert this data frame into a point dataset using `sf`
library(sf)

pred_sf <-
  pred_data %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)

# Lets convert the point dataset into a path

pred_path <- 
  pred_sf %>%
  group_by(id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


## --------------------------------------------------------------------------------------------------------- ##
##### ***Animate the tracks using gganimate!*** 
  
library(ggspatial)
library(gganimate)

## NOTE: the creation of the animation can take a long time!

esri_sat <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Imagery/MapServer/tile/${z}/${y}/${x}.jpeg')
track <- 
  ggplot() +
  annotation_map_tile(type = esri_sat) +
  geom_spatial_path(data = pred_data, aes(x = lon, y = lat, group = id), col = "white", crs = 4326) + 
  geom_spatial_point(data = pred_data, aes(x = lon, y = lat, group = id, col = id), crs = 4326) +
  labs(title = 'Date: {as.Date(frame_along)}', x = 'Longitude', y = 'Latitude') +
  annotation_scale(text_col = "white") +
  transition_reveal(date)

anim <- animate(track, width = 7, height = 5, units = "in", res = 250, nframes = 250, fps = 50,
                render = gifski_renderer(loop = FALSE))

save_animation(animation = anim, file = "~/Desktop/session2_anim.gif")


## --------------------------------------------------------------------------------------------------------- ##
## Now lets use our lovely `mapview` and `leaflet` knowledge from session one to plot a nice, interactive plot of move persistence data! 
  
library(leaflet)
library(mapview)
mapview::mapviewOptions(fgb = FALSE)

color_palette <- colorRampPalette(hcl.colors(10, palette = "Reds 3"))

m_130 <-
  mapview(pred_path %>% filter(id %in% "M-130"), alpha = 1, color = "white", homebutton = F, 
          legend = F, map.type = c("Esri.WorldImagery"), layer.name = "M-130") +
  mapview(pred_sf %>% filter(id %in% "M-130"), alpha.regions = 1, alpha = 0, zcol = "g",
          homebutton = F, legend = F, cex = 3, layer.name = "M-130", col.regions = color_palette(93)) +
  mapview(pred_sf %>% filter(id %in% "M-130") %>% slice(1), alpha.regions = 1, alpha = 0,
          col.regions = "darkgreen", homebutton = F, legend = F, layer.name = "M-130") +
  mapview(pred_sf %>% filter(id %in% "M-130") %>% slice(n()), alpha.regions = 1, alpha = 0,
          col.regions = "firebrick", homebutton = F, legend = F, layer.name = "M-130")

m_150 <-
  mapview(pred_path %>% filter(id %in% "M-150"), alpha = 1, color = "white", homebutton = F, 
          legend = F, map.type = c("Esri.WorldImagery"), layer.name = "M-150") +
  mapview(pred_sf %>% filter(id %in% "M-150"), alpha.regions = 1, alpha = 0, zcol = "g",
          homebutton = F, legend = F, cex = 3, layer.name = "M-150", col.regions = color_palette(54)) +
  mapview(pred_sf %>% filter(id %in% "M-150") %>% slice(1), alpha.regions = 1, alpha = 0,
          col.regions = "darkgreen", homebutton = F, legend = F, layer.name = "M-150") +
  mapview(pred_sf %>% filter(id %in% "M-150") %>% slice(n()), alpha.regions = 1, alpha = 0,
          col.regions = "firebrick", homebutton = F, legend = F, layer.name = "M-150")

mm <- 
  (m_130 + m_150)@map %>% 
  addLayersControl(
    baseGroups = c("M-130", "M-150"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(colors = color_palette(11), 
            labels = round(seq(0, 1, by = 0.1), 2),
            title = "g", opacity = 1)

mm


## --------------------------------------------------------------------------------------------------------- ##
#### Step 5: Simulated tracks

## Lets use the tagdat dataframe to build our whaleshark crw model

whaleshark_fit <-
  fit_ssm(x = tagdat, 
          vmax = 1.5, ## maximum speed of whale sharks (in m/s)
          model = "crw", ## Move persistence model
          time.step = 24, ## predict positions every 24 hours
          spdf = FALSE) ## turn off the pre-filter step

  
null_fit <- 
  sim_fit(whaleshark_fit, ## the ssm model we want to base our null models on
          what = "predicted", ## component of the model to use
          reps = 20) ## number of replicated simulations per animal

plot(null_fit, ncol = 3)


## load a rasterised file that provides information on the gradient of landmasses
library(terra)

load(system.file("extdata/grad.rda", package = "aniMotum"))
grad <- terra::unwrap(grad)

## Now lets rerun the simulation including the gradient object

null_fit2 <-
  sim_fit(
    whaleshark_fit,
    what = "predicted",
    reps = 20,
    grad = grad,
    beta = c(-300, -300)
  )

plot(null_fit2, ncol = 3)


## Lets filter out the top 50% of tracks that are spatially similar to our original track

null_fit2_filtered <-
  sim_filter(null_fit2, ## null simulation to filter
             keep = 0.5, ## proportion of tracks to keep
             var = c("lon", "lat"), ## variables to compare with original track
             FUN = "mean") ## filter to use a mean value of the above variables

plot(null_fit2_filtered, ncol = 3)


## We can then finally extract all the tracks as a data.frame and save it as a .csv file

null_dat <-
  null_fit2_filtered %>%
  unnest(cols = sims)

write_csv(null_dat, "null_data.csv")




## --------------------------------------------------------------------------------------------------------- ##
## --------------------------------------  END OF SESSION  ------------------------------------------------- ##
## --------------------------------------------------------------------------------------------------------- ##







