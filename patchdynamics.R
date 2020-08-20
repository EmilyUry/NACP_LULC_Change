
### classification of selected natural areas in H301 and H302

setwd("C:/Users/eau6/Desktop/NACP_proj") 

library(sp)
library(raster)
library(rgdal)



library(sf)
library(ggplot2)


library(landscapemetrics)
citation(package = "landscapemetrics")
library(landscapetools)

map96 <- raster("classified1996_select.tif")

map10 <- raster("classified2010_select.tif")
map10 <- crop(extend(map10, map96), map96)
map17 <- raster("classified2017_select.tif")
map17 <- crop(extend(map17, map96), map96)


cols <- c("palegreen4", "aquamarine3", "lightgoldenrod1", "brown3", "burlywood4", "gray80")
plot(map17, col = cols, legend = FALSE)

R1 <- raster("composite2017_R1.tif")
reclass1 <- c(0, 3000, 1)
reclass2 <- c(0, 3000, 2)
R1 <- reclassify(R1, reclass1 )
R2 <- raster("composite2017_R2.tif")
R2 <- reclassify(R2, reclass2 )

regions <- merge(R1, R2)
regions <- crop(extend(regions, map96), map96)
plot(regions)

stack1 <- brick(map96, map10, map17, regions)

data <- as.data.frame(stack1, xy = TRUE)
head(data)
names(data) <- c("x", "y", "m96", "m10", "m17", "region")


nrow(data[which(data$m17 == 1 | data$m17 == 2),])*0.09/100 #area in km2
nrow(data[which(data$m96 == 1 | data$m96 == 2),])*0.09/100
nrow(data[which(data$m10 == 1 | data$m10 == 2),])*0.09/100

df301 <- data[which(data$region == 1),]


map301.2017 <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value                
plot(dfr)





metrics <- calculate_lsm(map17, what = 'patch', progress = T, neighbourhood = 8)
nrow(metrics[which(metrics$class == 1 | metrics$class == 2),])
metrics$area_m2 <- metrics$value *111 *1000000




