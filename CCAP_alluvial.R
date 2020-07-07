



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

str(data)

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

data[which(data$m96 == 0),] <- 25
head(data)

for (i in 3:6){
data[which(data[,i] == 1),] <- 25 ## other = b = 2
 
#data[which(data[,i] == 2),] <- 2 
data[which(data[,i] == 3),] <- 2  ## urban = e
data[which(data[,i] == 4),] <- 2 
data[which(data[,i] == 5),] <- 2 
data[which(data[,i] == 6),] <- 7 ## ag = d
#data[which(data[,i] == 7),] <- 7
#data[which(data[,i] == 8),] <- 8 ## other veg = a
data[which(data[,i] == 9),] <- 8  
data[which(data[,i] == 10),] <- 8  
data[which(data[,i] == 11),] <- 8 
data[which(data[,i] == 12),] <- 8 
#data[which(data[,i] == 13),] <- 13  ## palustrine forest = c
#data[which(data[,i] == 14),] <- 14
data[which(data[,i] == 15),] <- 14  ## other wetland = f
data[which(data[,i] == 16),] <- 14
data[which(data[,i] == 17),] <- 14
data[which(data[,i] == 18),] <- 14
#data[which(data[,i] == 21),] <- 21  ## 21 = water = g
data[which(data[,i] == 19),] <- 20 ## near water = h
#data[which(data[,i] == 20),] <- 20
data[which(data[,i] == 22),] <- 23 ## water veg = i
}

head(data)
#table(data$m96)

## rename/reorder(1 = a = largest class)
for (i in 3:6){
  data[which(data[,i] == 2),] <- 5
  data[which(data[,i] == 8),] <- 1
  data[which(data[,i] == 13),] <- 3
  data[which(data[,i] == 7),] <- 4
  data[which(data[,i] == 14),] <- 6
  data[which(data[,i] == 23),] <- 9}
head(data)

for (i in 3:6){
  data[which(data[,i] == 0),] <- 2
  data[which(data[,i] == 21),] <- 7
  data[which(data[,i] == 21),] <- 8}
head(data)
#table(data$m96)


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


##### so this is what you have to do
##### run lines 130 through 332  **9 times through**
##### replace numbers 1 through 9 for the value of "apple" (line 130 )
##### after it runs, comment in the corresponding list from lines 336 through 344 to
##### save the returned vectors, (should be length = 81)
##### wash, rinse, repeat
##### takes a few minutes to run it through each time...


apple <- 2

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
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 2 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.b <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)


for (i in 1:9){
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 3 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.c <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)

for (i in 1:9){
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 4 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.d <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)


for (i in 1:9){
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 5 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.e <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)


for (i in 1:9){
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 6 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.f <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)


for (i in 1:9){
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 7 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.g <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)

for (i in 1:9){
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 8 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.h <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)

for (i in 1:9){
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 1),]
  aa[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 2),]
  ab[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 3),]
  ac[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 4),]
  ad[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 5),]
  ae[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 6),]
  af[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 7),]
  ag[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 8),]
  ah[i] <- nrow(aaa[which(aaa$m10==i),])
  aaa <- data[which(data$m96 == 1 & data$m01 == 9 & data$m06 == 9),]
  ai[i] <- nrow(aaa[which(aaa$m10==i),])
}
apple.i <- c(aa, ab, ac, ad, ae, af, ag, ah, ai)



# list1 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
 list2 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
# list3 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
# list4 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
# list5 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
# list6 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
# list7 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
# list8 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)
# list9 <- c(apple.a, apple.b, apple.c, apple.d, apple.e, apple.f, apple.g, apple.h, apple.i)

mega.list <- c(list1, list2, list3, list4, list5, list6, list7, list8, list9)
freq <- mega.list

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

list <- c("a", "b", "c", "d", "e", "f", "g", "h", i)
c10 <- c(rep(list, 729))

summary <- cbind(c96, c01, c06, c10, freq)
 

