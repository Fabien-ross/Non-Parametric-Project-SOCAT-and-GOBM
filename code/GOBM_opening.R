library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)

setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project")
ncfname <- "datasets/GOBM2025.nc"
ncin <- nc_open(ncfname)
print(ncin)

dname <- "sfco2"

tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)


time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")

cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
cf

tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))


