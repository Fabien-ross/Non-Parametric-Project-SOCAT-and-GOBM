setwd("~/Documents/Politecnico di Milano/NS/Non-Parametric-Project-SOCAT-and-GOBM")
#setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project")


library(data.table)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyr)
library(dplyr)
library(patchwork)

socat <- read.csv("datasets/socat_clean.csv", header=TRUE)
gobm <- read.csv("datasets/gobm_clean.csv", header=TRUE)
lon <- read.csv("datasets/lon_grid.csv", header=TRUE)
lat <- read.csv("datasets/lat_grid.csv", header=TRUE)

socat_clean<- socat[is.na(socat$fco2) | socat$fco2 <= 1000, ]

# ---- EDA ----

time_means <- setNames(
  
  aggregate(fco2 ~ time, socat, mean),
  
  c("time", "mean")
  
)
get_season <- function(x) {
  ifelse(x%%12 > 4 & x%%12 <= 10, "Summer", "Winter")
}



Variance <- numeric(12)
for (month in 1:12){
  Variance[month]=var(socat$fco2[socat$time %% 12 == month-1], na.rm = TRUE)
  }
plot(Variance)

socat$season <- get_season(socat$time)
time_means$season <- get_season(time_means$time)

ggplot(data = socat, mapping = aes(x = time, y = fco2)) +
  geom_point(mapping = aes(colour = season), size = 1) +
  geom_line(data = time_means, mapping = aes(x = time, y = mean, colour = season), size = 1) +
  scale_colour_manual(values = c(Winter = "blue", Summer = "red")) +
  coord_cartesian(ylim = c(0, 1000))


# Clean version
Variance_clean = numeric(12)
for (month in 1:12){
  Variance_clean[month]=var(socat_clean$fco2[socat_clean$time %% 12 == month-1], na.rm = TRUE)
}
plot(Variance_clean)

socat$season <- get_season(socat$time)
time_means$season <- get_season(time_means$time)

ggplot(data = socat, mapping = aes(x = time, y = fco2)) +
  geom_point(mapping = aes(colour = season), size = 1) +
  geom_line(data = time_means, mapping = aes(x = time, y = mean, colour = season), size = 1) +
  scale_colour_manual(values = c(Winter = "blue", Summer = "red")) +
  coord_cartesian(ylim = c(0, 6000))

# counts in summer / winter 
socat_winter <- socat[socat$season == "Winter",]
socat_summer <- socat[socat$season == "Summer",]


counts_winter <- socat_winter %>%
  group_by(lon, lat) %>%
  summarise(n_times = n(), .groups = "drop")

counts_summer <- socat_summer %>%
  group_by(lon, lat) %>%
  summarise(n_times = n(), .groups = "drop")


all_pixels <- expand.grid(
  lon = lon[[1]],
  lat = lat[[1]]
)

missing_pixels <- anti_join(
  all_pixels,
  distinct(socat_clean, lon, lat),
  by = c("lon", "lat")
)


z_range <- range(c(counts_summer$n_times, counts_winter$n_times), na.rm = TRUE)

make_plot <- function(df, plot_title) {
  ggplot(df, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = n_times)) +
    geom_tile(
      data = missing_pixels,
      aes(x = lon, y = lat),
      fill = "orange"
    ) +
    scale_fill_viridis_c(
      name = "Number of\ntime observations",
      limits = z_range
    ) +
    coord_fixed() +
    labs(
      x = "Longitude",
      y = "Latitude",
      title = plot_title
    ) +
    theme_bw()
}

p_summer <- make_plot(counts_summer, "Summer fCO2 observations")
p_winter <- make_plot(counts_winter, "Winter fCO2 observations")

(p_summer + p_winter) + plot_layout(ncol = 2, guides = "collect")



