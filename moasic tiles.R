
library(sp)
library(raster)


setwd("C:/Users/eau6/Desktop/NACP_proj")



tile1 <- "composite2017tile1.tif"
tile1 <- raster(tile1)


plot(tile1, ylim = c(34,38), xlim = c(-77, 75))




tile2 <- "composite2017tile2.tif"
tile2 <- raster(tile2)
plot(tile2, add = T)

tile3 <- "composite2017tile3.tif"
tile3 <- raster(tile3)
plot(tile3)

tile4 <- "composite2017tile4.tif"
tile4 <- raster(tile4)
plot(tile4)

tile9 <- "composite2017tile9.tif"
tile9 <- raster(tile9)
plot(tile9)

m1 <- mosaic(tile1, tile2, tile3, tile4, tile9, fun = mean)
plot(m1, col = "darkgreen")


tile5 <- raster("composite2017tile5.tif")
tile6 <- raster("composite2017tile6.tif")
tile7 <- raster("composite2017tile7-0000000000-0000000000.tif")
tile7a <- raster("composite2017tile7-0000000000-0000006912.tif")
tile8 <- raster("composite2017tile8.tif")
tile10 <- raster("composite2017tile10.tif")
tile11 <- raster("composite2017tile11.tif")

m.all <- mosaic(tile1, tile2, tile3, tile4, tile9, tile5, tile6, tile7, tile7a, tile8, tile10,
                tile11, fun = mean)
plot(m.all, col = "darkgreen")
plot(m1, col = "blue", add = T)
