



classmap17 <- raster("classified2017.tif")



#### cut map to watershed and cut out water
C301 <- st_read("CP_301export.shp")
C302 <- st_read("CP_302export.shp")
water <- st_read("water_vector.shp")


cols <- c("palegreen4", "aquamarine3", "lightgoldenrod1", "brown3", "burlywood4", "gray50")

plot(classmap17, col = cols)
plot(C301, add = T)
plot(C302, add = T)
plot(water, add = T, col = "blue")
