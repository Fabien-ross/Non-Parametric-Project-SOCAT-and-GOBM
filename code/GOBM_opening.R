# further info: https://pjbartlein.github.io/REarthSysSci/netCDF.html
library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)

#setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project")
setwd("~/Documents/Politecnico di Milano/NS/Non-Parametric-Project-SOCAT-and-GOBM")

ncfname <- "datasets/GOBM2025.nc"
ncin <- nc_open(ncfname) # open the df 
ncin

dname <- "sfco2"

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon) 
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)
# we have 1° x 1° steps

# get time
time <- ncvar_get(ncin,"time")
time
# we have t in days in steps of months 

tunits <- ncatt_get(ncin,"time","units")
tunits

nt <- dim(time)
nt

fco2_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(fco2_array)


# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

# decode time
cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
cf


timestamps <- as_timestamp(cf) # get character-string times
timestamps

# had to change the command since new version of CFtime
# ls("package:CFtime") => as_timestamp
ls("package:CFtime")
class(timestamps)


time_cf <- parse_timestamps(cf, timestamps) # parse the string into date components
time_cf
class(time_cf)

# In a netCDF file, values of a variable that are either missing or simply not 
# available (i.e. ocean grid points in a terrestrial data set) are flagged using 
# specific “fill values” (_FillValue) or missing values (missing_value),
 
fco2_array[fco2_array==fillvalue$value] <- NA

length(na.omit(as.vector(fco2_array[,,1]))) # one slice at time 1 from the brick 

# amount of na in a layer, probably land 
length(as.vector(fco2_array[,,1]))-length(na.omit(as.vector(fco2_array[,,1]))) 

fco2_array
# get a single slice or layer (January)
m <- 1
fco2_slice <- fco2_array[,,m]
dim(fco2_slice) # fits the dimension 
# quick map
image(lon,lat,fco2_slice, col=rev(brewer.pal(10,"RdBu")))
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(250,275,300,325,350,375,400,425)
levelplot(fco2_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat)) # ie the grid 
dim(lonlat)

fco2_vec <- as.vector(fco2_slice)
length(fco2_vec)

# create dataframe and add names
fco2_df01 <- data.frame(cbind(lonlat,fco2_vec))
names(fco2_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(fco2_df01), 10)

# set path and filename
csvpath <- "datasets/"
csvname <- "GOBM2025_unrestricted.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(fco2_df01),csvfile, row.names=FALSE, sep=",")

GOBM2025<-read.csv("datasets/GOBM2025_unrestricted.csv")



