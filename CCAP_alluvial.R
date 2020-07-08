



## Alluvial diagram for the entire NACP

setwd("C:/Users/eau6/Desktop/NACP_proj")
library(sp)
library(raster)
library(alluvial)


map96 <- raster("cover1996Try.tif")
m01 <- raster("cover2001Try.tif")
m06 <- raster("cover2006Try.tif")
m10 <- raster("cover2010Try.tif")



map10 <- crop(extend(m10, map96), map96) 
map06 <- crop(extend(m06, map96), map96) 
map01 <- crop(extend(m01, map96), map96)

stack <- brick(map96, map01, map06, map10)

data <- as.data.frame(stack, xy = TRUE)
head(data)
names(data) <- c("x", "y", "m96", "m01", "m06", 'm10')
#table(data$m96)

### key
# 0 = Background          -> # 0 = other
# 1 = Unclassified        -> # 0 = other
# 2 = Developed, high     -> # 2 = urban
# 3 = Developed, med      -> # 2 = urban
# 4 = Developed, low      -> # 2 = urban
# 5 = Developed, open     -> # 2 = urban
# 6 = Crops               -> # 7 = ag
# 7 = Pasture             -> # 7 = ag
# 8 = Grassland           -> # 8 = other veg
# 9 = Deciduous           -> # 8 = other veg
# 10 = Evergreen          -> # 8 = other veg
# 11 = Mixed forest       -> # 8 = other veg
# 12 = Scrub/shrub        -> # 8 = other veg
# 13 = Palustrine forest  -> # 13 = palustrine
# 14 = Palustrine scrub   -> # 14 = other wetland
# 15 = Palustrine emergent-> # 14 = other wetland
# 16 = Estuarine Forest   -> # 14 = other wetland
# 17 = Esturarine scrub   -> # 14 = other wetland
# 18 = Estuarine emergent -> # 14 = other wetland
# 19 = Shore              -> # 20 = near water
# 20 = Barren             -> # 20 = near water
# 21 = Open water         -> # 21 = water
# 22 = Palustrine aquatic -> # 23 = water veg
# 23 = Estuarine aq bed   -> # 23 = water veg


### combine classes
data[data==0]<- NA
data[is.na(data)] <- 25 ## other = b = 2
data[data==1]<- NA
data[is.na(data)] <- 25
data[data==2]<- NA
data[is.na(data)] <- 26 ## urban = e = 5
data[data==3]<- NA
data[is.na(data)] <- 26
data[data==4]<- NA
data[is.na(data)] <- 26 
data[data==5]<- NA
data[is.na(data)] <- 26
data[data==6]<- NA
data[is.na(data)] <- 27 ## ag = d = 4
data[data==7]<- NA
data[is.na(data)] <- 27
data[data==8]<- NA
data[is.na(data)] <- 28 ## other veg = a = 1
data[data==9]<- NA
data[is.na(data)] <- 28
data[data==10]<- NA
data[is.na(data)] <- 28 
data[data==11]<- NA
data[is.na(data)] <- 28
data[data==12]<- NA
data[is.na(data)] <- 28 
data[data==13]<- NA
data[is.na(data)] <- 29 ## palustrine forest = c = 3
data[data==14]<- NA
data[is.na(data)] <- 30 ## other wetland = f = 6
data[data==15]<- NA
data[is.na(data)] <- 30
data[data==16]<- NA
data[is.na(data)] <- 30 
data[data==17]<- NA
data[is.na(data)] <- 30
data[data==18]<- NA
data[is.na(data)] <- 30 
data[data==19]<- NA
data[is.na(data)] <- 31 ## near water = h = 8
data[data==20]<- NA
data[is.na(data)] <- 31 
data[data==21]<- NA
data[is.na(data)] <- 32 ## water = g = 7
data[data==22]<- NA
data[is.na(data)] <- 33 ## water veg = i = 9
data[data==23]<- NA
data[is.na(data)] <- 33

### rename/reorder classes, now they are in order of total class size (decending), 
### this will be helpful for the alluv plot
data[data==25]<- NA
data[is.na(data)] <- 2 
data[data==26]<- NA
data[is.na(data)] <- 5
data[data==27]<- NA
data[is.na(data)] <- 4 
data[data==28]<- NA
data[is.na(data)] <- 1
data[data==29]<- NA
data[is.na(data)] <- 3 
data[data==30]<- NA
data[is.na(data)] <- 6
data[data==31]<- NA
data[is.na(data)] <- 8 
data[data==32]<- NA
data[is.na(data)] <- 7
data[data==33]<- NA
data[is.na(data)] <- 9

table(data$m96)

df <- data ### save this in case you f it up
data <- df
  
###Create empty vectors
aa <- vector()
ab <- vector()
ac <- vector()
ad <- vector()
ae <- vector()
af <- vector()
ag <- vector()
ah <- vector()
ai <- vector()


# writes out the list for m96 = 1
{apple <- 1
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
for (i in 1:9){
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
list1 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 2
{apple <- 2
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
list2 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 3
{apple <- 3
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list3 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 4
{apple <- 4
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list4 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 5
{apple <- 4
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list4 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 5
{apple <- 5
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list5 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 5
{apple <- 6
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list6 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 5
{apple <- 7
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list7 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 5
{apple <- 8
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list8 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

# writes out the list for m96 = 5
{apple <- 9
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 1 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.a <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 2 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 3 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 4 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 5 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 6 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 7 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 8 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  for (i in 1:9){
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 1),]
    aa[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 2),]
    ab[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 3),]
    ac[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 4),]
    ad[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 5),]
    ae[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 6),]
    af[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 7),]
    ag[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 8),]
    ah[i] <- nrow(aaa[which(aaa$m10==i),])
    aaa <- data[which(data$m96 == apple & data$m01 == 9 & data$m06 == 9),]
    ai[i] <- nrow(aaa[which(aaa$m10==i),])
  }
  apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)
  list9 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
}

mega.list <- c(list1, list2, list3, list4, list5, list6, list7, list8, list9)
freq <- mega.list  ## should be length 6561

## creates the label column for each year
c96 <- c(rep("a", 729), rep("b", 729), rep("c", 729),
           rep("d", 729), rep("e", 729), rep("f", 729), 
           rep("g", 729), rep("h", 729), rep("i", 729))

rep81 <- c(rep("a", 81), rep("b", 81), rep("c", 81),
         rep("d", 81), rep("e", 81), rep("f", 81), 
         rep("g", 81), rep("h", 81), rep("i", 81))
c01 <- c(rep(rep81, 9))

rep9 <- c(rep("a", 9), rep("b", 9), rep("c", 9),
          rep("d", 9), rep("e", 9), rep("f", 9), 
          rep("g", 9), rep("h", 9), rep("i", 9))
c06 <- c(rep(rep9, 81))

list <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")
c10 <- c(rep(list, 729))

summary <- cbind(c96, c01, c06, c10, freq)
 
write.csv(summary, file = "TRY_alluv_data.csv")

##### read in the data frame here if you want
# dt <- read.csv("Try_alluv_data.csv", head = T)

dt <- as.data.frame(summary)
dt$freq <- as.numeric(dt$freq)
head(dt)

alluvial(dt[,1:4], freq = dt$freq)

alluvial(dt[,1:4], freq = dt$freq, hide = dt$freq <5000, 
         col = "gray70", 
         border = "gray20")


### just the pixels that are changing: cp = changing pixels

cp <- dt[which(dt$c96 != dt$c10),]
alluvial(cp[,1:4], freq = cp$freq, hide = cp$freq <10000, 
         col = "gray70", 
         border = "gray20")


#### some color

alluvial(cp[,1:4], freq = cp$freq, hide = cp$freq <10000,    #### hide, threshold for small lines -- play around with this
         col = ifelse(cp$c96 == "a", "darkgreen", 
                      ifelse(cp$c96 == "c", "aquamarine3", 
                             ifelse(cp$c96 == "d", "lightgoldenrod", "burlywood4"))),
         border = ifelse(cp$c96 == "a", "darkgreen", 
                         ifelse(cp$c96 == "c", "aquamarine3", 
                                ifelse(cp$c96 == "d", "lightgoldenrod", "burlywood4"))),
         gap.width = 0.1)

### here is a reminder of the key
# a = other veg
# b = other
# c = palustrine forest
# d = ag
# e = urban
# f = other wetland
# g = water
# h = shore/barren
# i = aquatic veg





