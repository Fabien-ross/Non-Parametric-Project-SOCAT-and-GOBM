library(dplyr)
library(ncdf4)
library(CFtime)
library(lubridate)

getwd()
setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project")

# %%%%%%%%%%%%%%%%%%%%% SOCAT

socat <- read.csv("datasets/SOCAT_grid.csv")

head(socat)
names(socat)

socat$valid_tax_date <- as.Date(socat$valid_tax_date)

socat_clean <- socat %>%
  select(
    lon = valid_lon_month,
    lat = valid_lat_month,
    date = valid_tax_date,
    nobs = valid_fco2_count_nobs_month,
    fco2 = valid_fco2_ave_weighted_month,
    temp = valid_sst_ave_weighted_month,
    salinity = valid_salinity_ave_weighted_month
  ) %>%
  filter(
    date >= as.Date("2000-01-01"),
    date <= as.Date("2024-12-31")
  )

socat_clean <- socat_clean %>%
  mutate(date = floor_date(date, unit = "month"))

dim(socat_clean)
unique(socat_clean$date)


# %%%%%%%%%%%%%%%%%%%%% GOBM

ncin  <- nc_open("datasets/GOBM2025.nc")
print(ncin)

lon  <- ncvar_get(ncin, "lon")
lat  <- ncvar_get(ncin, "lat")
time <- ncvar_get(ncin, "time")

# Convert time to calendar dates
dates <- as.Date(time, origin = "1959-01-01")
head(dates)
tail(dates)

start_date <- min(socat_clean$date, na.rm = TRUE)
end_date   <- max(socat_clean$date, na.rm = TRUE)

time_idx <- which(dates >= start_date & dates <= end_date)

dates_selected <- dates[time_idx]

sfco2 <- ncvar_get(
  ncin, "sfco2",
  start = c(1, 1, min(time_idx)),
  count = c(-1, -1, length(time_idx))
)

gobm_clean <- expand.grid(
  lon = lon,
  lat = lat,
  date = dates_selected
) %>%
  mutate(
    sfco2 = as.vector(sfco2)
  )

gobm_clean <- gobm_clean %>%
  mutate(date = floor_date(date, unit = "month"))

# lon between -180 and 180
gobm_clean <- gobm_clean %>%
  mutate(
    lon = ((lon + 180) %% 360) - 180
  )

gobm_clean <- gobm_clean %>%
  filter(
    lon >= -100,
    lon <= 10,
    lat >= 10,
    lat <= 75,
  )

socat_clean <- socat_clean %>%
  filter(
    lon >= -100,
    lon <= 10,
    lat >= 10,
    lat <= 75,
  )


dim(gobm_clean)
head(gobm_clean)

sort(unique(gobm_clean$lon))
sort(unique(socat_clean$lon))
sort(unique(gobm_clean$date))
sort(unique(socat_clean$date))


# @@@@@@@@@@@@@@@@@@ WRITE IT

write.csv(socat_clean, "datasets/socat_full_clean.csv", row.names = FALSE)
write.csv(gobm_clean, "datasets/gobm_full_clean.csv", row.names = FALSE)
