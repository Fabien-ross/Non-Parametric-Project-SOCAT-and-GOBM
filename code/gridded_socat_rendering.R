setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project")
setwd("~/Documents/Politecnico di Milano/NS/Non-Parametric-Project-SOCAT-and-GOBM")

library(data.table)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyr)
library(dplyr)

socat <- read.csv("datasets/socat_clean.csv", header=TRUE)
gobm <- read.csv("datasets/gobm_clean.csv", header=TRUE)
lon <- read.csv("datasets/lon_grid.csv", header=TRUE)
lat <- read.csv("datasets/lat_grid.csv", header=TRUE)

all_pixels <- expand.grid(
  lon = lon[[1]],
  lat = lat[[1]]
)

head(gobm)

# %%%%%%%%%%%%%% Continents and islands
missing_pixels <- anti_join(
  all_pixels,
  distinct(df, lon, lat),
  by = c("lon", "lat")
)

gobm_na_pixels <- gobm %>%
  filter(is.na(sfco2)) %>%
  distinct(lon, lat)

# %%%%%%%%%%%%%% Count the number of measure per pixel
counts <- socat %>%
  group_by(lon, lat) %>%
  summarise(n_times = n(), .groups = "drop")

# Only keep data with sufficient number of measurements
n_obs_floor <- 30
floored_counts <- counts %>%
  filter(n_times < n_obs_floor)

# 
floored_socat <- socat %>%
  filter(
    paste(lon, lat) %in% paste(floored_counts$lon,
                               floored_counts$lat)
  )

# All the "sufficient measurements" time series
timeseries <- floored_socat %>%
  arrange(lon, lat, date) %>%
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
ggplot(floored_socat, aes(x = time)) +
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

