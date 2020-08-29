
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


### calculate total forested wetland area:

nrow(data[which(data$m17 == 1 | data$m17 == 2),])*0.09/100 #area in km2
nrow(data[which(data$m96 == 1 | data$m96 == 2),])*0.09/100
nrow(data[which(data$m10 == 1 | data$m10 == 2),])*0.09/100


### combine evergreen and deciduous classes == 1

data$m96[which(data$m96 == 2)] <- 1
data$m10[which(data$m10 == 2)] <- 1
data$m17[which(data$m17 == 2)] <- 1

### combine non-fores classes == 3

data$m96[which(data$m96 == 4)] <- 3
data$m10[which(data$m10 == 4)] <- 3
data$m17[which(data$m17 == 4)] <- 3

data$m96[which(data$m96 == 5)] <- 3
data$m10[which(data$m10 == 5)] <- 3
data$m17[which(data$m17 == 5)] <- 3

data$m96[which(data$m96 == 6)] <- 3
data$m10[which(data$m10 == 6)] <- 3
data$m17[which(data$m17 == 6)] <- 3




nrow(data[which(data$m17 == 1),])*0.09/100
nrow(data[which(data$m10 == 1),])*0.09/100
nrow(data[which(data$m96 == 1),])*0.09/100



data$del <- ifelse(data$m96 == data$m17, 0, 1)


floss <- data[which(data$m96 == 1 & data$del == 1), ]

map196 <- data[, c(1,2,3)]
mapc96 <- rasterFromXYZ(map196)
plot(mapc96, col = c("black", "gray", "gray"))

floss.map <- floss[,c(1,2,5)]
map.floss <- rasterFromXYZ(floss.map)
plot(map.floss, col = "red", add = T)



##### cool map
library(maps)
par(mar = c(.1,.1,.1,.1))
map(database = 'state', regions = c('North Carolina', 'Virginia'), col = "oldlace", fill = TRUE, border = NA)
#this draws all PA counties since the regions argument uses partial matching
map(database = 'county', regions = 'North Carolina', col = "black", fill = FALSE, add = TRUE)
map(database = 'county', regions = 'Virginia', col = "black", fill = FALSE, add = TRUE)
plot(mapc96, col = c("black", "gray", "gray"), add = TRUE, legend = F)
plot(map.floss, col = "red", add = T, legend = F)












df301 <- data[which(data$region == 1),]
df302 <- data[which(data$region == 2),]

df301$del <- ifelse(df301$m96 == df301$m17, 1, 0)

df301.2017 <- df301[,c(1,2,5)]



map301.2017 <- rasterFromXYZ(df301.2017)  #Convert first two columns as lon-lat and third as value                
plot(map301.2017, col = cols)
cols <- c("black", "black", "gray", "gray", "gray", "gray")
plot(map301.2017, col = cols)





metrics <- calculate_lsm(map17, what = 'patch', progress = T, neighbourhood = 8)
nrow(metrics[which(metrics$class == 1 | metrics$class == 2),])

area17 <- metrics[which(metrics$metric == "area"),]

## largest = 2.452313e-06

write.csv(metrics, "patchMetrics2017.csv")


metrics10 <- calculate_lsm(map10, what = 'patch', progress = T, neighbourhood = 8)
nrow(metrics10[which(metrics10$class == 1 | metrics10$class == 2),])

area10 <- metrics10[which(metrics10$metric == "area"),]
## largest = 	3.717938e-06

write.csv(metrics10, "patchMetrics2010.csv")






metrics96 <- calculate_lsm(map96, what = 'patch', progress = T, neighbourhood = 8)
nrow(metrics96[which(metrics96$class == 1 | metrics96$class == 2),])

area96 <- metrics96[which(metrics96$metric == "area"),]
## largest 1996 = 		3.645188e-06

write.csv(metrics96, "patchMetrics1996.csv")







