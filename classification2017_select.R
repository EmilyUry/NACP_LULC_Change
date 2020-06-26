### classification of selected natural areas in H301 and H302

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
classes <- rasterize(vps.sp, raster, field = 'class')  

# mask raster brick to training points
covmask <- mask(stack, classes)
names(classes) <- "class"
trainingbrick <- addLayer(covmask,classes)
#plot(trainingbrick)
valuetable <- getValues(trainingbrick)
#rm(trainingbrick)
#rm(classes)
valuetable <- na.omit(valuetable)       ### if this doesnt work try memory.limit(size =65176*2) or run line 42 before line 41
valuetable <- as.data.frame(valuetable) 
#head(valuetable)
valuetable$class <- factor(valuetable$class, levels = c(1:6))



##### make the RF model
set.seed(123)
valuetable$random = sample(2, nrow(valuetable), replace=TRUE, prob=c(0.8,0.2))
trainData = valuetable[valuetable$random==1,]
testData = valuetable[valuetable$random==2,]

modelRF <- randomForest(x=trainData[,c(1:12)], y=trainData$class, importance = TRUE)
modelRF

### Accuracy assessment

prediction <- predict(modelRF, newdata=testData)
#table(prediction, testData$class)
cm <- table(prediction, testData$class)
accuracy <- (sum(diag(cm)))/sum(cm)
accuracy  ## 82.1 (seed:123)


##  overall model
modelRF <- randomForest(x=valuetable[,c(1:12)], y=valuetable$class, importance = TRUE)


### Map prediction

names(stack)
names(valuetable) ## these must match
map17 <- predict(stack, model=modelRF, na.rm=TRUE)
plot(map17, col = cols, legend = FALSE)


writeRaster(map17, filename="classified2017_select", format="GTiff", overwrite=TRUE)


C301 <- st_read("CP_301export.shp")
C302 <- st_read("CP_302export.shp")

plot(C301, add = T)
plot(C302, add = T)


