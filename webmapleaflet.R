




##### cool map
library(maps)
par(mar = c(.1,.1,.1,.1))
map(database = 'state', regions = c('North Carolina', 'Virginia'), col = "gray97", fill = TRUE, border = NA)
#this draws all PA counties since the regions argument uses partial matching
map(database = 'county', regions = 'North Carolina', col = "black", fill = FALSE, add = TRUE)
map(database = 'county', regions = 'Virginia', col = "black", fill = FALSE, add = TRUE)
plot(mapc96, col = c("black", "gray", "gray"), add = TRUE, legend = F)
plot(map.floss, col = "red", add = T, legend = F)




# sitesmap2.R
# Map the sites data using the leaflet package.
# This creates a prettier, interactive map.
library(leaflet)
library(maps)
library(htmlwidgets) # To save the map as a web page.







# The data to map.
sites <- read.csv("http://college.holycross.edu/faculty/rlent/sites/sites.csv")
# State boundaries from the maps package. The fill option must be TRUE.
bounds <- map('state', c('North Carolina', 'Virginia'), fill=TRUE, plot=FALSE)
counties <- map(database = 'county', regions = c('North Carolina', 'Virginia'), col = "black", fill = FALSE, plot = FALSE)

# A custom icon.
icons <- awesomeIcons(
  icon = 'disc',
  iconColor = 'black',
  library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
  markerColor = 'blue',
  squareMarker = TRUE
)
# Create the Leaflet map widget and add some map layers.
# We use the pipe operator %>% to streamline the adding of
# layers to the leaflet object. The pipe operator comes from 
# the magrittr package via the dplyr package.
map <- leaflet(data = sites) %>%
  # setView(-72.14600, 43.82977, zoom = 8) %>% 
  addProviderTiles("CartoDB.Positron", group = "Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
  addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
  # Marker data are from the sites data frame. We need the ~ symbols
  # to indicate the columns of the data frame.
  addMarkers(~lon_dd, ~lat_dd, label = ~locality, group = "Sites") %>% 
  # addAwesomeMarkers(~lon_dd, ~lat_dd, label = ~locality, group = "Sites", icon=icons) %>%
  addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
  addPolygons(data=counties, group="Counties", weight=1, fillOpacity = 0) %>%
  
  addScaleBar(position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("Map", "Satellite", "Relief"),
    overlayGroups = c("Sites", "States"),
    options = layersControlOptions(collapsed = FALSE)
  )
invisible(print(map))

# Save the interactive map to an HTML page.
saveWidget(map, file="sitesmap2.html", selfcontained=TRUE)
# Uncomment the following line to save the interactive map to a static image file.
# mapshot(map, file="sitesmap2.png")
