# Source - https://stackoverflow.com/a/35140673
# Posted by csgillespie
# Retrieved 2026-11-06, License - CC BY-SA 3.0

setwd("~/Documents/Politecnico di Milano/NS/Project/")
library("raster")
r = raster("GCB2025_PRINCETON_fgco2_sfco2_SimA_1959-2024.nc")
head(r)
names(r)
