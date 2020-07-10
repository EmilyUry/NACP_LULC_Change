


setwd("C:/Users/eau6/Desktop/NACP_proj")

library(sp)
library(raster)
library(rgdal)
library(randomForest)
library(rsample)
library(sf)

map17 <- raster("classified2017_select.tif")
map10 <- raster("classified2010_select.tif")
map96 <- raster("classified1996_select.tif")

map17 <- crop(extend(map17, map96), map96) 
map10 <- crop(extend(map10, map96), map96) 


stack <- brick(map96, map10, map17)

data <- as.data.frame(stack, xy = TRUE)
head(data)
names(data) <- c("x", "y", "m96", "m10", "m17")
data[is.na(data)] <- 0
data <- data[which(data$m96 != 0),]