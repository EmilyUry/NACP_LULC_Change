### classification of selected natural areas in H301 and H302

setwd("C:/Users/eau6/Desktop/NACP_proj")

library(sp)
library(raster)
library(rgdal)
library(randomForest)
library(rsample)
library(sf)

### Mosaic regions
#____Do this first, only once

# R1 <- brick("composite2017_R1.tif")
# R2 <- brick("composite2017_R2.tif")
# m.all <- mosaic(R1, R2, fun = mean)
# writeRaster(m.all, "NL_trim.tif", overwrite = T)


raster <- raster("NL_trim.tif")
plot(raster)

vps <- read.csv("2017vps_select.csv", head = T)
points(vps$x, vps$y, pch = "+", cex = 0.3)

stack <- brick("NL_trim.tif")
names(stack)


# make the validation points into a spatial points dataframe
crs <- crs(raster)
vps.sp <- SpatialPointsDataFrame(vps[,1:2], vps, proj4string = crs)
vps.sp
classes <- rasterize(vps.sp, raster, field = 'Class')  




