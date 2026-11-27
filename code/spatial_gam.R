#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# GAM representation
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project/datasets")

library(data.table)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyr)
library(dplyr)

filename = "SOCAT2025_gridded_restricted.csv"

df <- read.csv(filename, header=TRUE)
lon <- read.csv("lon_grid.csv", header=TRUE)
lat <- read.csv("lat_grid.csv", header=TRUE)

all_pixels <- expand.grid(
  lon = lon[[1]],
  lat = lat[[1]]
)

# %%%%%%%%%%%%%% Continents and islands
missing_pixels <- anti_join(
  all_pixels,
  distinct(df, lon, lat),
  by = c("lon", "lat")
)

# Count the number of measure per square
counts <- df %>%
  group_by(lon, lat) %>%
  summarise(n_times = n(), .groups = "drop")

# Only keep data with sufficient number of measurements
n_obs_floor <- 30
floored_counts <- counts %>%
  filter(n_times < n_obs_floor)

# 
floored_df <- df %>%
  filter(
    paste(lon, lat) %in% paste(floored_counts$lon,
                               floored_counts$lat)
  )

# All the "sufficient measurements" time series
timeseries <- floored_df %>%
  arrange(lon, lat, time) %>%
  group_by(lon, lat) %>%
  group_split()


spatial_by_month <- floored_df %>%
  arrange(lon, lat, time) %>%
  group_by(time) %>%
  group_split()

max_time <- floored_df %>%
  count(time, name = "n_observations") %>%
  slice_max(n_observations, n = 1)

spatial_max <- floored_df %>%
  filter(time == max_time$time)


##################

ggplot(spatial_max, aes(x = lon, y = lat)) +
  geom_tile(fill = "black") +
  geom_tile(
    data = missing_pixels,
    aes(x = lon, y = lat),
    fill = "orange"
  ) +
  scale_fill_viridis_c(
    name = "Number of\ntime observations"
  ) +
  coord_fixed() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Spatial distribution of fCO2 observations"
  ) +
  theme_bw()

ggplot(spatial_max, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = fco2 )) +
  geom_tile(
    data = missing_pixels,
    aes(x = lon, y = lat),
    fill = "orange"
  ) +
  scale_fill_viridis_c(
    name = "fco2 levels"
  ) +
  coord_fixed() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Spatial distribution of fCO2 observations"
  ) +
  theme_bw()


################# GAM
library(splines)
library(mgcv)

# Thin Plate Regression Spline
gam_fit <- gam(
  fco2 ~ s(lon, lat, time, bs = "tp", k = 100) + s(time, bs = "tp", k = 30),
  data = floored_df,
  method = "REML"
)
summary(gam_fit)

time <- seq(100,660, by=5)

full_grid <- expand.grid(
  lon = lon[[1]],
  lat = lat[[1]],
  time = time
)

# Predict fCO2
pred_grid <- full_grid
pred_grid$fco2_pred <- predict(gam_fit, newdata = pred_grid)

head(pred_grid)

library(viridis)

ggplot(pred_grid[pred_grid$time == time[100], ],
       aes(x = lon, y = lat, fill = fco2_pred)) +
  geom_raster() +
  scale_fill_viridis_c(
    name = expression(paste("Predicted fCO"[2]))
  ) +
  geom_tile(
    data = missing_pixels,
    aes(x = lon, y = lat),
    fill = "orange"
  ) +
  coord_fixed() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Spatial distribution of predicted fCO2",
    subtitle = paste("Time:", time[1])
  ) +
  theme_bw()

######## GIF

library(gganimate)
library(gifski)

p <- ggplot(pred_grid,
            aes(x = lon, y = lat, fill = fco2_pred)) +
  
  geom_raster() +
  
  scale_fill_viridis_c(
    name = expression(paste("Predicted fCO"[2])),
    limits = range(pred_grid$fco2_pred, na.rm = TRUE)
  ) +
  
  geom_tile(
    data = missing_pixels,
    aes(x = lon, y = lat),
    fill = "orange"
  ) +
  
  coord_fixed() +
  
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Spatial distribution of predicted fCO2",
    subtitle = "Time: {current_frame}"
  ) +
  
  theme_bw() +
  
  transition_manual(time)

# Generate GIF
anim <- animate(
  p,
  nframes = length(unique(pred_grid$time)),
  fps = 5,
  width = 900,
  height = 700,
  renderer = gifski_renderer()
)

anim_save("fCO2_animation.gif", animation = anim)

anim_save(
  filename = file.path(getwd(), "fCO2_animation_2.gif"),
  animation = anim
)
