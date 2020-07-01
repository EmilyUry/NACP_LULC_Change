
setwd("C:/Users/eau6/Desktop/NACP_proj")

library(sp)
library(raster)
library(rgdal)
library(randomForest)
library(rsample)
library(sf)

cols <- c("palegreen4", "aquamarine3", "lightgoldenrod1", "brown3", "burlywood4", "gray80")

### Mosaic regions
#____Do this first, only once

# R1 <- brick("composite2010_R1.tif")
# R2 <- brick("composite2010_R2.tif")
# m.all <- mosaic(R1, R2, fun = mean)
# writeRaster(m.all, "NL_trim_2010.tif", overwrite = T)