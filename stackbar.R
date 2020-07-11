


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





R1.17 <- crop(map17, R1)
R2.17 <- crop(map17, R2)
R1.10 <- crop(map10, R1)
R2.10 <- crop(map10, R2)
R1.96 <- crop(map96, R1)
R2.96 <- crop(map96, R2)

par(mfrow = c(1,3))
plot(R1.96)
plot(R1.10)
plot(R1.17)

stack1 <- brick(R1.96, R1.10, R1.17)
data1 <- as.data.frame(stack1, xy = TRUE)
head(data1)
names(data1) <- c("x", "y", "m96", "m10", "m17")
data1[is.na(data1)] <- 0
data1 <- data1[which(data1$m96 != 0),]
data1[is.na(data1)] <- 0


stack2 <- brick(R2.96, R2.10, R2.17)
data2 <- as.data.frame(stack2, xy = TRUE)
head(data2)
names(data2) <- c("x", "y", "m96", "m10", "m17")
data2[is.na(data2)] <- 0
data2 <- data2[which(data2$m96 != 0),]

data <- data1

w96 <- nrow(data[which(data$m96 == 0),])  #water
p96 <- nrow(data[which(data$m96 == 1),])  #pine
m96 <- nrow(data[which(data$m96 == 2),])  #mixed/deciduous
h96 <- nrow(data[which(data$m96 == 3),])  #herbacious/marsh
g96 <- nrow(data[which(data$m96 == 4),])  #ghost forest
s96 <- nrow(data[which(data$m96 == 5),])  #shrub scrub
b96 <- nrow(data[which(data$m96 == 6),])  #sand/beach

w10 <- nrow(data[which(data$m10 == 0),])
p10 <- nrow(data[which(data$m10 == 1),])
m10 <- nrow(data[which(data$m10 == 2),])
h10 <- nrow(data[which(data$m10 == 3),])
g10 <- nrow(data[which(data$m10 == 4),])
s10 <- nrow(data[which(data$m10 == 5),])
b10 <- nrow(data[which(data$m10 == 6),])
w10 <- nrow(data) - p10 - m10 - h10 - g10 - s10

w17 <- nrow(data[which(data$m17 == 0),])
p17 <- nrow(data[which(data$m17 == 1),])
m17 <- nrow(data[which(data$m17 == 2),])
h17 <- nrow(data[which(data$m17 == 3),])
g17 <- nrow(data[which(data$m17 == 4),])
s17 <- nrow(data[which(data$m17 == 5),])
b17 <- nrow(data[which(data$m17 == 6),])
w17 <- nrow(data) - p17 - m17 - h17 - g17 - s17


forest <- c( p96+m96, p10+m10, p17+m17)
ghost <- c(g96, g10, g17)
shrub <- c( s96, s10, s17)
marsh <- c(m96, m10, m17)
sand <- c(b96, b10, b17)
water <- c(w96, w10, w17)
list <- c(forest, ghost, shrub, marsh, sand, water)
type <- c("forest", "forest", "forest", "ghost", "ghost", "ghost", "shrub", "shrub", "shrub",
          "marsh", "marsh", "marsh", "sand", "sand", "sand", "water", "water", "water")
year <- rep(c("1996", "2010", "2017"), 6)
r1d <- data.frame(type, year, list)
r2d <- data.frame(type, year, list)
r1d
r2d



ggplot(r1d, aes(fill = type, y = list, x = year)) +
  geom_bar(position = "stack", stat = "identity")

ggplot(r2d, aes(fill = type, y = list, x = year)) +
  geom_bar(position = "stack", stat = "identity")














R1 <- crop(extend(R1, map96), map96)
map17 <- crop(extend(map17, map96), map96) 
map10 <- crop(extend(map10, map96), map96) 


stack <- brick(map96, map10, map17, R1)

stack <- brick(map96, map10, map17)


data <- as.data.frame(stack, xy = TRUE)
head(data)
names(data) <- c("x", "y", "m96", "m10", "m17", "R1")



R1.data <- data[which(data$R1 != 0),]
R2.data <- data[which(data$R1 == 0),]




data <- R1.data
data <- R2.data

data[is.na(data)] <- 0
data <- data[which(data$m96 != 0),]

## sum all the classes
{
w96 <- nrow(data[which(data$m96 == 0),])  #water
p96 <- nrow(data[which(data$m96 == 1),])  #pine
m96 <- nrow(data[which(data$m96 == 2),])  #mixed/deciduous
h96 <- nrow(data[which(data$m96 == 3),])  #herbacious/marsh
g96 <- nrow(data[which(data$m96 == 4),])  #ghost forest
s96 <- nrow(data[which(data$m96 == 5),])  #shrub scrub
b96 <- nrow(data[which(data$m96 == 6),])  #sand/beach

w10 <- nrow(data[which(data$m10 == 0),])
p10 <- nrow(data[which(data$m10 == 1),])
m10 <- nrow(data[which(data$m10 == 2),])
h10 <- nrow(data[which(data$m10 == 3),])
g10 <- nrow(data[which(data$m10 == 4),])
s10 <- nrow(data[which(data$m10 == 5),])
b10 <- nrow(data[which(data$m10 == 6),])

w17 <- nrow(data[which(data$m17 == 0),])
p17 <- nrow(data[which(data$m17 == 1),])
m17 <- nrow(data[which(data$m17 == 2),])
h17 <- nrow(data[which(data$m17 == 3),])
g17 <- nrow(data[which(data$m17 == 4),])
s17 <- nrow(data[which(data$m17 == 5),])
b17 <- nrow(data[which(data$m17 == 6),])
}
type <- c("forest", "marsh", "ghost", "shrub", "sand", "water")
y96.total <- c(p96+m96, h96, g96, s96, b96, w96)
y10.total <- c(p10+m10, h10, g10, s10, b10, w10)
y17.total <- c(p17+m17, h17, g17, s17, b17, w17)
# y10.total <- c(1087910+1597296, 623630, 4760, 577676, 1756, 0)
# y17.total <- c(1750402+ 973871, 580608, 74766, 502431, 705, 10245)

forest <- c( p96+m96, p10+m10, p17+m17)
ghost <- c(g96, g10, g17)
shrub <- c( s96, s10, s17)
marsh <- c(m96, m10, m17)
water <- c(w96, w10, w17)
r2 <- c(forest, ghost, shrub, marsh, water)
type <- c("forest", "forest", "forest", "ghost", "ghost", "ghost", "shrub", "shrub", "shrub",
         "marsh", "marsh", "marsh", "water", "water", "water")
year <- rep(c("1996", "2010", "2017"), 5)
r1d <- data.frame(type, year, r1)
r2d <- data.frame(type, year, r2)
r1d
r2d
ggplot(r1d, aes(fill = type, y = r1, x = year)) +
  geom_bar(position = "stack", stat = "identity")

ggplot(r2d, aes(fill = type, y = r2, x = year)) +
  geom_bar(position = "stack", stat = "identity")


r1d <- data.frame(forest, ghost, shrub, marsh, water)
r1d
# 
# y10.ha <- y10.total*0.09  ## conversion to hectares
# y17.ha <- y17.total*0.09

##set <- data.frame(type, y10.ha, y17.ha)
R2.set <- data.frame(type, y96.total, y10.total, y17.total)
R2.set 
R1.set
r1

R1.set
str(R1.set)
barplot(r1d, beside = F)

