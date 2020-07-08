

## three step alluvial

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
table(data$m96)
# 
# write.csv(data, file= "three_year_data.csv")



# 
# all.data <- read.csv("three_year_data.csv", head = T)
# head(all.data)
# names(all.data) <- c("no","x", "y", "m96", "m10", "m17")

#all.data[is.na(all.data)] <- 0
#data <- all.data[which(all.data$y10 != 0),]

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

type <- c("forest", "marsh", "ghost", "shrub", "sand", "water")
y96.total <- c(p96+m96, h96, g96, s96, b96, w96)
y10.total <- c(p10+m10, h10, g10, s10, b10, w10)
y17.total <- c(p17+m17, h17, g17, s17, b17, w17)
# y10.total <- c(1087910+1597296, 623630, 4760, 577676, 1756, 0)
# y17.total <- c(1750402+ 973871, 580608, 74766, 502431, 705, 10245)
# 
# y10.ha <- y10.total*0.09  ## conversion to hectares
# y17.ha <- y17.total*0.09

##set <- data.frame(type, y10.ha, y17.ha)
set <- data.frame(type, y96.total, y10.total, y17.total)
set  #### this could be used to make a simple stacked bar plot...

library(alluvial)


data[data==0]<- NA
data[is.na(data)] <- 7 ## makes water # 7

data[data==1]<- NA
data[is.na(data)] <- 2 ## combines pine and deciduous into "forest" class

# data[which(data$m96 == 1),] <- 2  ## combines pine and deciduous into "forest" class
# data[which(data$m10 == 1),] <- 2
# data[which(data$m17 == 1),] <- 2
# 
# data[which(data$m96 == 0),] <- 7   ## makes water number 7
# data[which(data$m10 == 0),] <- 7
# data[which(data$m17 == 0),] <- 7


forest <- vector()
marsh <- vector()
ghost <- vector()
shrub <- vector()
sand <- vector()
water <- vector()

for (i in 2:7){
  FF <- data[which(data$m96 == 2 & data$m10 == 2),]
  forest[i] <- nrow(FF[which(FF$m17==i),])
  FM <- data[which(data$m96 == 2 & data$m10 == 3),]
  marsh[i] <- nrow(FM[which(FM$m17==i),])
  FG <-data[which(data$m96 == 2 & data$m10 == 4),]
  ghost[i] <- nrow(FG[which(FG$m17==i),])
  FSh <-data[which(data$m96 == 2 & data$m10 == 5),]
  shrub[i] <- nrow(FSh[which(FSh$m17==i),])
  FSa <- data[which(data$m96 == 2 & data$m10 == 6),]
  sand[i] <- nrow(FSa[which(FSa$m17==i),])
  FW <-  data[which(data$m96 == 2 & data$m10 == 7),]
  water[i] <- nrow(FW[which(FW$m17==i),])
}

Forest96 <- c(forest[2:7], marsh[2:7], ghost[2:7], shrub[2:7], sand[2:7], water[2:7])
Forest96


for (i in 2:7){
  FF <- data[which(data$m96 == 3 & data$m10 == 2),]
  forest[i] <- nrow(FF[which(FF$m17==i),])
  FM <- data[which(data$m96 == 3 & data$m10 == 3),]
  marsh[i] <- nrow(FM[which(FM$m17==i),])
  FG <-data[which(data$m96 == 3 & data$m10 == 4),]
  ghost[i] <- nrow(FG[which(FG$m17==i),])
  FSh <-data[which(data$m96 == 3 & data$m10 == 5),]
  shrub[i] <- nrow(FSh[which(FSh$m17==i),])
  FSa <- data[which(data$m96 == 3 & data$m10 == 6),]
  sand[i] <- nrow(FSa[which(FSa$m17==i),])
  FW <-  data[which(data$m96 == 3 & data$m10 == 7),]
  water[i] <- nrow(FW[which(FW$m17==i),])
}
Marsh96 <- c(forest[2:7], marsh[2:7], ghost[2:7], shrub[2:7], sand[2:7], water[2:7])
Marsh96

for (i in 2:7){
  FF <- data[which(data$m96 == 4 & data$m10 == 2),]
  forest[i] <- nrow(FF[which(FF$m17==i),])
  FM <- data[which(data$m96 == 4 & data$m10 == 3),]
  marsh[i] <- nrow(FM[which(FM$m17==i),])
  FG <-data[which(data$m96 == 4 & data$m10 == 4),]
  ghost[i] <- nrow(FG[which(FG$m17==i),])
  FSh <-data[which(data$m96 == 4 & data$m10 == 5),]
  shrub[i] <- nrow(FSh[which(FSh$m17==i),])
  FSa <- data[which(data$m96 == 4 & data$m10 == 6),]
  sand[i] <- nrow(FSa[which(FSa$m17==i),])
  FW <-  data[which(data$m96 == 4 & data$m10 == 7),]
  water[i] <- nrow(FW[which(FW$m17==i),])
}
Ghost96 <- c(forest[2:7], marsh[2:7], ghost[2:7], shrub[2:7], sand[2:7], water[2:7])
Ghost96

# for (i in 2:7){
#   FF <- data[which(data$m96 == 4 & data$m10 == 2),]
#   forest[i] <- nrow(FF[which(FF$m17==i),])
#   FM <- data[which(data$m96 == 4 & data$m10 == 3),]
#   marsh[i] <- nrow(FM[which(FM$m17==i),])
#   FG <-data[which(data$m96 == 4 & data$m10 == 4),]
#   ghost[i] <- nrow(FG[which(FG$m17==i),])
#   FSh <-data[which(data$m96 == 4 & data$m10 == 5),]
#   shrub[i] <- nrow(FSh[which(FSh$m17==i),])
#   FSa <- data[which(data$m96 == 4 & data$m10 == 6),]
#   sand[i] <- nrow(FSa[which(FSa$m17==i),])
#   FW <-  data[which(data$m96 == 4 & data$m10 == 7),]
#   water[i] <- nrow(FW[which(FW$m17==i),])
# }
# Ghost96 <- c(forest[2:7], marsh[2:7], ghost[2:7], shrub[2:7], sand[2:7], water[2:7])
# Ghost96


for (i in 2:7){
  FF <- data[which(data$m96 == 5 & data$m10 == 2),]
  forest[i] <- nrow(FF[which(FF$m17==i),])
  FM <- data[which(data$m96 == 5 & data$m10 == 3),]
  marsh[i] <- nrow(FM[which(FM$m17==i),])
  FG <-data[which(data$m96 == 5 & data$m10 == 4),]
  ghost[i] <- nrow(FG[which(FG$m17==i),])
  FSh <-data[which(data$m96 == 5 & data$m10 == 5),]
  shrub[i] <- nrow(FSh[which(FSh$m17==i),])
  FSa <- data[which(data$m96 == 5 & data$m10 == 6),]
  sand[i] <- nrow(FSa[which(FSa$m17==i),])
  FW <-  data[which(data$m96 == 5 & data$m10 == 7),]
  water[i] <- nrow(FW[which(FW$m17==i),])
}
Shrub96 <- c(forest[2:7], marsh[2:7], ghost[2:7], shrub[2:7], sand[2:7], water[2:7])
Shrub96

for (i in 2:7){
  FF <- data[which(data$m96 == 6 & data$m10 == 2),]
  forest[i] <- nrow(FF[which(FF$m17==i),])
  FM <- data[which(data$m96 == 6 & data$m10 == 3),]
  marsh[i] <- nrow(FM[which(FM$m17==i),])
  FG <-data[which(data$m96 == 6 & data$m10 == 4),]
  ghost[i] <- nrow(FG[which(FG$m17==i),])
  FSh <-data[which(data$m96 == 6 & data$m10 == 5),]
  shrub[i] <- nrow(FSh[which(FSh$m17==i),])
  FSa <- data[which(data$m96 == 6 & data$m10 == 6),]
  sand[i] <- nrow(FSa[which(FSa$m17==i),])
  FW <-  data[which(data$m96 == 6 & data$m10 == 7),]
  water[i] <- nrow(FW[which(FW$m17==i),])
}
Sand96 <- c(forest[2:7], marsh[2:7], ghost[2:7], shrub[2:7], sand[2:7], water[2:7])
Sand96




freq <- c(Forest96, Marsh96, Ghost96, Shrub96, Sand96)


Cat96 <- c(rep("Forest", 36), rep("Marsh", 36), rep("Ghost", 36),
           rep("Shrub", 36), rep("Sand", 36))
rep6 <- c(rep("Forest", 6), rep("Marsh", 6), rep("Ghost", 6),
           rep("Shrub", 6), rep("Sand", 6), rep("Water", 6))
Cat10 <- c(rep(rep6, 5))
list <- c("Forest", "Marsh", "Ghost", "Shrub", "Sand", "Water")
Cat17 <- c(rep(list, 30))

summary <- cbind(Cat96, Cat10, Cat17, freq)

dt <- as.data.frame(summary)
dt$freq <- as.numeric(dt$freq)

alluvial(dt[,1:3], freq = dt$freq)

alluvial(dt[,1:3], freq = dt$freq, hide = dt$freq <30000)




dt[dt=="Forest"] <- "aForest"  ## alphabatizes so the bars stack in the order I want them too
dt[dt=="Ghost"] <- "bGhost"
dt[dt=="Shrub"] <- "cShrub"
dt[dt=="Marsh"] <- "dMarsh"
dt[dt=="Sand"] <- NA          ## omit this class from the alluvial plot
dt <- na.omit(dt)

### options -- lots of colors
alluvial(dt[,1:3], freq = dt$freq, hide = dt$freq <13500,     #### hide, threshold for small lines -- play around with this
         col = ifelse(dt$Cat17 == "bGhost", "brown3", 
                      ifelse(dt$Cat17 == "cShrub", "burlywood4", 
                             ifelse(dt$Cat17 == "dMarsh", "lightgoldenrod", "palegreen4"))),
         border = ifelse(dt$Cat17 == "bGhost", "brown3", 
                           ifelse(dt$Cat17 == "cShrub", "burlywood4", 
                                  ifelse(dt$Cat17 == "dMarsh", "lightgoldenrod", "palegreen4"))),
         gap.width = 0.1)



### options -- lots of colors, lower threshold 
alluvial(dt[,1:3], freq = dt$freq, hide = dt$freq <5000,
         col = ifelse(dt$Cat17== "bGhost", "brown3", 
                      ifelse(dt$Cat96 == "cShrub", "burlywood4", 
                             ifelse(dt$Cat96 == "dMarsh", "lightgoldenrod", "palegreen4"))),
         border = ifelse(dt$Cat17 == "bGhost", "brown3", 
                         ifelse(dt$Cat96 == "cShrub", "burlywood4", 
                                ifelse(dt$Cat96 == "dMarsh", "darkgoldenrod", "palegreen4"))),
         gap.width = 0.1, 
         axis_labels = c("1996", "2010", "2017"), 
         cex = 1, cex.axis = 1.5)

### fewer colors
alluvial(dt[,1:3], freq = dt$freq, hide = dt$freq <10000,
         col = ifelse(dt$Cat17 == "bGhost", "brown3", "grey"),
         gap.width = 0.2)




