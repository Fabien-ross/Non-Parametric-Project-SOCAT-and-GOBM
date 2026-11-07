library(ncdf4)
library(CFtime)

setwd("~/Documents/Politecnico di Milano/NS/Non-Parametric-Project-SOCAT-and-GOBM")

ncin  <- nc_open("datasets/GOBM2025.nc")
dname <- "sfco2"

lon  <- ncvar_get(ncin, "lon")
lat  <- ncvar_get(ncin, "lat")
time <- ncvar_get(ncin, "time")

# restriction
lon_idx  <- which(lon >= -99.5 & lon <= 9.5)
lat_idx  <- which(lat >= 9.5   & lat <= 74.5)
time_idx <- 133:792

lon_sub  <- lon[lon_idx]
lat_sub  <- lat[lat_idx]
time_sub <- time[time_idx]
length(time_sub)

# data subset, fill values -> NA
fco2_array <- ncvar_get(ncin, dname)
fco2_sub   <- fco2_array[lon_idx, lat_idx, time_idx, drop = FALSE]
fillvalue  <- ncatt_get(ncin, dname, "_FillValue")
if (fillvalue$hasatt) fco2_sub[fco2_sub == fillvalue$value] <- NA

# readable dates for the selected time steps
tunits <- ncatt_get(ncin, "time", "units")
cf     <- CFtime(tunits$value, calendar = "proleptic_gregorian", time)
date   <- as_timestamp(cf)[time_idx]


nc_close(ncin)

# long-format data frame (lon varies fastest, then lat, then time)
grid <- expand.grid(lon = lon_sub, lat = lat_sub, time = 1:660)
grid$date  <- rep(date, each = length(lon_sub) * length(lat_sub))
grid$sfco2 <- as.vector(fco2_sub)

# drop land / missing cells and save
df <- na.omit(grid)
df 
write.csv(df, "datasets/GOBM2025_restricted.csv", row.names = FALSE)
df<-read.csv("datasets/GOBM2025_restricted.csv")
