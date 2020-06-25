

### classify region1

library(sp)
library(raster)
library(rgdal)
library(randomForest)
library(rsample)
library(sf)

raster <- raster("bothwatersheds.tif")
plot(raster)

vps <- read.csv("2017vpsMini.csv", head = T)

points(vps$x, vps$y, pch = "+", cex = 0.3)

stack <- brick("bothwatersheds.tif")
names(stack)




####
# make the validation points into a spatial points dataframe
crs <- crs(raster)
vps.sp <- SpatialPointsDataFrame(vps[,1:2], vps, proj4string = crs)
vps.sp
classes <- rasterize(vps.sp, raster, field = 'Class')  # mak

cols <- c("palegreen4", "aquamarine3", "lightgoldenrod1", "brown3", "burlywood4", "gray80")

# mask raster brick to training points
covmask <- mask(stack, classes)
names(classes) <- "class"
trainingbrick <- addLayer(covmask,classes)
#plot(trainingbrick)
valuetable <- getValues(trainingbrick)
rm(trainingbrick)
rm(classes)
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
#modelRF

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


writeRaster(map17, filename="classified2017", format="GTiff", overwrite=TRUE)





