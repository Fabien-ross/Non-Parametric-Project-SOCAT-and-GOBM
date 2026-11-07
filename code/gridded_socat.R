library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)

setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project")
ncfname <- "datasets/SOCATv2026_gridded_monthly.nc"
ncin <- nc_open(ncfname)

time <- ncvar_get(ncin,"tmnth") #days since 1970-01-01

lon <- ncvar_get(ncin,"xlon")
lat <- ncvar_get(ncin,"ylat")

minlon = -99.5
maxlon =  9.5
minlat = 9.5
maxlat =  74.5

lon_idx <- which(lon >= minlon & lon <= maxlon)
lat_idx <- which(lat >= minlat & lat <= maxlat)

nobs <- ncvar_get(ncin,"fco2_count_nobs")
fco2 <- ncvar_get(ncin,"fco2_ave_unwtd")
fco2_min <- ncvar_get(ncin,"fco2_min_unwtd")
fco2_max <- ncvar_get(ncin,"fco2_max_unwtd")
sd <- ncvar_get(ncin,"fco2_std_unwtd")

nobs_sub <- nobs[lon_idx, lat_idx, , drop = FALSE]
fco2_sub <- fco2[lon_idx, lat_idx, , drop = FALSE]
fco2_min_sub <- fco2_min[lon_idx, lat_idx, , drop = FALSE]
fco2_max_sub <- fco2_max[lon_idx, lat_idx, , drop = FALSE]
sd_sub <- sd[lon_idx, lat_idx, , drop = FALSE]

lon_sub <- lon[lon_idx]
lat_sub <- lat[lat_idx]
time_sub <- time[]

lonlat_time <- expand.grid(
  lon = lon_sub,
  lat = lat_sub,
  time = time_sub
)

df <- data.frame(
  lon = lonlat_time$lon,
  lat = lonlat_time$lat,
  time = lonlat_time$time,
  nobs = as.vector(nobs_sub),
  fco2 = as.vector(fco2_sub),
  fco2_min = as.vector(fco2_min_sub),
  fco2_max = as.vector(fco2_max_sub),
  sd = as.vector(sd_sub)
)


