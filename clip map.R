



classmap17 <- raster("classified2017.tif")



#### cut map to watershed and cut out water
C301 <- st_read("CP_301export.shp")
C302 <- st_read("CP_302export.shp")
water <- st_read("cp")