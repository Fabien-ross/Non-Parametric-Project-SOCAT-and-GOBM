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

# %%%%%%%%%%%%%% Count the number of measure per pixel
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


#@@@@@@@@@@@@@@@@@@@@@
#@@@@@ Graphics @@@@@@
#@@@@@@@@@@@@@@@@@@@@@

# Distribution of the number of measurements
ggplot(counts, aes(x = n_times)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  labs(
    x = "Number of time observations per lon-lat pair",
    y = "Number of lon-lat pairs",
    title = "Distribution of temporal observations per grid cell"
  ) +
  theme_bw()

# Distribution of the number of measurements per month
ggplot(floored_df, aes(x = time)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  labs(
    x = "Time step",
    y = "Number of observations",
    title = "Distribution of observations over time"
  ) +
  theme_bw()

ggplot(counts, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = n_times)) +
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

# Map of the measurements floored
ggplot(floored_counts, aes(x = lon, y = lat, fill = n_times)) +
  geom_tile() +
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


# First time series
ggplot(timeseries[[3]], aes(x = time, y = fco2)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time",
    y = "fCO2",
    title = "fCO2 time series for one grid cell"
  )

