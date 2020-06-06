

#' ---
#' title: "Calculating drainage density for coastal watersheds"
#' author: "Emily Ury"
#' date: "June 5, 2020"
#' output: github_document
#' ---
#'


#' This script is used to calculate drainage density for different flow types for the
#' subwatersheds of interest on the north atlantic coastal plain. 
#' The watershed data was downloaded from NHDPlus which can be accessed:
#' https://viewer.nationalmap.gov/basic/?basemap=b1&category=nhd&title=NHD%20View#startUp
#' 

#' Locally the files are found here: 
setwd("C:/Users/eau6/Desktop/NHD_data")


library(rgdal)
library(rgeos)


### Ftype key
# Artificial <- c("558", "336")  ## artificial path, canal/ditch
# Coastline <- c("566")
# Other <- c("334", "428", "420") ## connector, pipeline, underground conduit
# Natural <- c("460")



##read in all of the watersheds of interest
## this is the catchment area shapefile
catchment <- readOGR("H0303_catchment.shp")
catchment <- spTransform(catchment, proj4string(HUCs))
catchment2 <- readOGR("H0304_catchment.shp")
catchment2 <- spTransform(catchment2, proj4string(HUCs))
catchment3 <- readOGR("H0302_catchment.shp")
catchment3 <- spTransform(catchment3, proj4string(HUCs))
catchment4 <- readOGR("H0315_catchment.shp")
catchment4 <- spTransform(catchment4, proj4string(HUCs))
catchment5 <- readOGR("H0306_catchment.shp")
catchment5 <- spTransform(catchment5, proj4string(HUCs))
catchment6 <- readOGR("H0305_catchment.shp")
catchment6 <- spTransform(catchment6, proj4string(HUCs))
catchment7 <- readOGR("H0307_catchment.shp")
catchment7 <- spTransform(catchment7, proj4string(HUCs))
catchment8 <- readOGR("H0317_catchment.shp")
catchment8 <- spTransform(catchment8, proj4string(HUCs))
catchment9 <- readOGR("H0316_catchment.shp")
catchment9 <- spTransform(catchment9, proj4string(HUCs))
catchment10 <- readOGR("H1308_catchment.shp")
catchment10 <- spTransform(catchment10, proj4string(HUCs))
catchment11 <- readOGR("H0301_catchment.shp")
catchment11 <- spTransform(catchment11, proj4string(HUCs))

### these are the flowlength shapefiles (length in km is already associated with
### each vector, so there is no need to project the data)
H0303 <- readOGR("H0303_flowline.shp")
H0304 <- readOGR("H0304_flowline.shp")
H0302 <- readOGR("H0302_flowline.shp")
H0315 <- readOGR("H0315_flowline.shp")
H0306 <- readOGR("H0306_flowline.shp")
H0305 <- readOGR("H0305_flowline.shp")
H0307 <- readOGR("H0307_flowline.shp")
H0317 <- readOGR("H0317_flowline.shp")
H0316 <- readOGR("H0316_flowline.shp")
H1308 <- readOGR("H1308_flowline.shp")
H0301 <- readOGR("H0301_flowline.shp")

watershed <- c("H0303", "H0304", "H0302", "H0315", "H0306", "H0305", "H0307", "H0317", "H0316", "H1308", "H0301")

catchments <- c(catchment, catchment2, catchment3, catchment4, catchment5, catchment6, catchment7,
                catchment8, catchment9, catchment10, catchment11)
wsarea <- laply(catchments, gArea)
wsarea <- wsarea*10000 ## km squared


flowlines <- c(H0303, H0304, H0302, H0315, H0306, H0305, H0307, H0317, H0316, H1308, H0301)
dlength <- sapply(flowlines, function(x) sum(x$LengthKM))  ## unit = km
t.artificial <- sapply(flowlines, function(x) sum(x$LengthKM[x$FType == "558" |x$FType == "336"]))
t.natural <- sapply(flowlines, function(x) sum(x$LengthKM[x$FType == "460"]))
t.coast <- sapply(flowlines, function(x) sum(x$LengthKM[x$FType == "566"]))
t.other <- sapply(flowlines, function(x) sum(x$LengthKM[x$FType == "334" | x$FType == "428" | x$FType == "420"]))

summary <- data.frame()
summary <- list(watershed, wsarea, dlength, dlength/wsarea, t.artificial, t.natural, t.coast, t.other)
names(summary) <- c("Watershed", "Area", "Length", "Drainage Density", "Artificial Channels", "Natural Channels", 
                    "Coastline", "Other")

write.csv(summary, file = "drainage_density.csv")

### map of all catchments
HUCs <- readOGR("NACPHUCSexport.shp")

plot(HUCs, border = "red")
plot(catchment, col = "darkgreen", add = T)
plot(catchment2, col = "blue", add = T)
plot(catchment3, col = "black", add = T)
plot(catchment4, col = "chocolate1", add = T)
plot(catchment5, col = "orchid", add = T)
plot(catchment6, col = "slateblue2", add = T)
plot(catchment7, col = "gold", add = T)
plot(catchment8, col = "deepskyblue", add = T)
plot(catchment9, col = "yellowgreen", add = T)
plot(catchment10, col = "hotpink3", add = T)
plot(catchment11, col = "gray100", add = T)
legend("bottomright", c("0303", "0304", "0302", "0315", "0306", "0305" ,"0307", "03017", "0316", "1308", "0301"), pt.bg = c("darkgreen", "blue", "black", "chocolate1",
                                                                                                                            "orchid", "slateblue2", "gold", "deepskyblue", 
                                                                                                                            "yellowgreen", "hotpink3", "gray100"), pch = 22)
