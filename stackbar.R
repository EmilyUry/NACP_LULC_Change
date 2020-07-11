


setwd("C:/Users/eau6/Desktop/NACP_proj")

library(sp)
library(raster)
library(rgdal)
library(randomForest)
library(rsample)
library(sf)
library(ggplot2)

map96 <- raster("classified1996_select.tif")

map10 <- raster("classified2010_select.tif")
map10 <- crop(extend(map10, map96), map96)
map17 <- raster("classified2017_select.tif")
map17 <- crop(extend(map17, map96), map96)



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
data.na <- na.omit(data)

data1 <- data.na[which(data.na$region == 1),]
data2 <- data.na[which(data.na$region == 2),]
all96 <- table(data.na$m96)
r1.96 <- table(data1$m96)
r2.96 <- table(data2$m96)

r1.10 <- table(data1$m10)
r2.10 <- table(data2$m10)

r1.17 <- table(data1$m17)
r2.17 <- table(data2$m17)



forest <- c(r1.96[1]+r1.96[2], r1.10[1]+ r1.10[2], r1.17[1]+r1.17[2])
ghost <- c(r1.96[4], r1.10[4], r1.17[4])
shrub <- c(r1.96[5], r1.10[5], r1.17[5])
marsh <- c(r1.96[3], r1.10[3], r1.17[3])
#sand <- c(r1.96[6], r1.10[6], r1.17[6])
water <- c(0, 6667, 9746)
list <- c(forest, ghost, shrub, marsh, water)
hectares1 <- list*0.09
type <- c("forest", "forest", "forest", "ghost", "ghost", "ghost", "shrub", "shrub", "shrub",
          "marsh", "marsh", "marsh", "water", "water", "water")
year <- rep(c("1996", "2010", "2017"), 5)

r1d <- data.frame(type, year, hectares1)
r1d



forest <- c(r2.96[1]+r2.96[2], r2.10[1]+ r2.10[2], r2.17[1]+r2.17[2])
ghost <- c(r2.96[4], r2.10[4], r2.17[4])
shrub <- c(r2.96[5], r2.10[5], r2.17[5])
marsh <- c(r2.96[3], r2.10[3], r2.17[3])
#sand <- c(r2.96[6], r2.10[6], r2.17[6])
water <- c(0, 5274, 7825)
list <- c(forest, ghost, shrub, marsh, water)
hectares2 <- list*0.09

r2d <- data.frame(type, year, hectares2)
r2d



ggplot(r1d, aes(fill = type, y = hectares1, x = year)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_classic()

ggplot(r2d, aes(fill = type, y = hectares2, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic()

### facet plot 
cols <- c("palegreen4", "lightgoldenrod1", "brown3", "burlywood4", "gray80", "black")

year <- rep(c("1996", "2010", "2017"), 10)
region <- c(rep("Roanoke-Chowan", 15), rep("Neuse-Pamlico", 15))
type <- c("forest", "forest", "forest", "ghost", "ghost", "ghost", "shrub", "shrub", "shrub",
          "marsh", "marsh", "marsh", "water", "water", "water")
type <- c(type, type)
area <- c(hectares1, hectares2)
df <-  data.frame(type, year, region, area)

df

a <- ggplot(df, aes(fill = reorder(type, area), y = area, x = year)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values=c( "black","brown3","gray50", "gray80", "palegreen4")) +
  theme_classic()

a + facet_grid(cols = vars(region))


