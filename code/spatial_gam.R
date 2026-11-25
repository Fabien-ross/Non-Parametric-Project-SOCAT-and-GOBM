setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project/datasets")
setwd("~/Documents/Politecnico di Milano/NS/Non-Parametric-Project-SOCAT-and-GOBM/datasets")
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
time <- 1:660
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

head(spatial_max)

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

gam_fit <- gam(
  fco2 ~ s(lon, lat, bs = "tp", k = 100),
  data = floored_df,
  method = "REML"
)
summary(gam_fit)


# Predict fCO2
pred_grid <- all_pixels
pred_grid$fco2_pred <- predict(gam_fit, newdata = all_pixels)

ggplot(pred_grid, aes(x = lon, y = lat, fill = fco2_pred)) +
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
    subtitle = "GAM predictions"
  ) +
  theme_bw()
