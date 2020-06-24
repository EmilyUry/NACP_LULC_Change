
library(sp)
library(raster)


setwd("C:/Users/eau6/Desktop/NACP_proj")



tile1 <- brick("composite2017tile1.tif")

tile2 <- brick("composite2017tile2.tif")

tile3 <- brick("composite2017tile3.tif")

tile4 <- brick("composite2017tile4.tif")

tile9 <- brick("composite2017tile9.tif")

m1 <- merge(tile1, tile2, tile3, tile4, tile9, fun = mean)

writeRaster(m1, "region1.tif", overwrite = T)





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


writeRaster(m.all, "bothwatersheds.tif")






vps <- read.csv("sample2017vps.csv", head = T)
points(vps$x, vps$y, pch = "+", cex = 0.5)




plot(m1)
