
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

R1 <- brick("composite1996_R1.tif")
R2 <- brick("composite1996_R2.tif")
m.all <- mosaic(R1, R2, fun = mean)
writeRaster(m.all, "NL_trim_1996.tif", overwrite = T)



raster <- raster("NL_trim_1996.tif")
plot(raster)
stack <- brick("NL_trim_1996.tif")
names(stack)
names(stack) <- c("NL_trim_2010.1",  "NL_trim_2010.2",  "NL_trim_2010.3",  "NL_trim_2010.4",  "NL_trim_2010.5",
                  "NL_trim_2010.6", "NL_trim_2010.7",  "NL_trim_2010.8",  "NL_trim_2010.9",  "NL_trim_2010.10", 
                  "NL_trim_2010.11", "NL_trim_2010.12")

### must run the whole 2010 script to generate modelRF (~10 mins)


### Map prediction


names(valuetable) ## these must match names(stack)
map96 <- predict(stack, model=modelRF, na.rm=TRUE)
plot(map96, col = cols, legend = FALSE)

## if the plot looks good, write out the layer:

writeRaster(map96, filename="classified1996_select", format="GTiff", overwrite=TRUE)

