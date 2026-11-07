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
time <- read.csv("time_grid.csv", header=TRUE)

head(data)

counts <- df %>%
  group_by(lon, lat) %>%
  summarise(n_times = n(), .groups = "drop")

ggplot(counts, aes(x = n_times)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  labs(
    x = "Number of time observations per lon-lat pair",
    y = "Number of lon-lat pairs",
    title = "Distribution of temporal observations per grid cell"
  ) +
  theme_bw()

ggplot(counts, aes(x = lon, y = lat, fill = n_times)) +
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
