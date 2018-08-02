# Locations in Cambodia
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started February 24, 2018
# Last updated February 24, 2018

###############################################################################
# INITIALIZE AND LOAD DATA
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")                                # Load custom functions
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "gridExtra",
                 "xlsx", "Amelia", "ltm", "googleVis",
                 "rgdal", "maps", "mapdata", "maptools",
                 "scales", "mapproj"))
load(file = "iDE_Oct2017.Rdata")
# names(data)

###############################################################################
# PLOT LOCATIONS
###############################################################################
map = gvisMap(data = data.frame(name = data$Dist,
                                latlong = paste(data$DistLat,
                                                data$DistLong, sep = ":")),
              locationvar = "latlong",
              tipvar = "name", chartid = "1")
plot(map)

map()
map(wrap = c(0,360), fill = TRUE, col = 2) # pacific-centered map of the world
map(wrap = c(0, 360, NA), fill = TRUE, col = 2) # idem, without Antarctica
map('usa')	# national boundaries
map('county', 'new jersey')	# county map of New Jersey
map('state', region = c('new york', 'new jersey', 'penn'))	# map of three states
map("state", ".*dakota", myborder = 0)	# map of the dakotas
map.axes()				# show the effect of myborder = 0
if(require(mapproj))
  map('state', proj = 'bonne', param = 45)	# Bonne equal-area projection of states

# names of the San Juan islands in Washington state
map('county', 'washington,san', names = TRUE, plot = FALSE)

# national boundaries in one linetype, states in another
# (figure 5 in the reference)
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)

# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
if(require(mapproj)) {	# mapproj is used for  projection="polyconic"
  # color US county map by 2009 unemployment rate
  # match counties to map using FIPS county codes
  # Based on J's solution to the "Choropleth Challenge"
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  
  # load data
  # unemp includes data for some counties not on the "lower 48 states" county
  # map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
  #  cities
  data(unemp)
  data(county.fips)
  
  # define color buckets
  colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
  unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
  leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
  
  # align data with map definitions by (partial) matching state,county
  # names, which include multiple polygons for some counties
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                      county.fips$polyname)]
  colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]
  
  # draw map
  map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
      lty = 0, projection = "polyconic")
  map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
      projection="polyconic")
  title("unemployment by county, 2009")
  legend("topright", leg.txt, horiz = TRUE, fill = colors)
  
  # Choropleth Challenge example, based on J's solution, see:
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  # To see the faint county boundaries, use RGui menu:  File/SaveAs/PDF
}


cambodia = readShapePoly("data/cambodia_administrative.shp")   #layer of data for species range




map(database = "world", regions = ".")
samps <- read.csv("FieldSamples.csv")   #my data for sampling sites, contains a column of "lat" and a column of "lon" with GPS points in decimal degrees
map("worldHires","Canada", xlim=c(-140,-110),ylim=c(48,64), col="gray90", fill=TRUE)  #plot the region of Canada I want
map("worldHires","usa", xlim=c(-140,-110),ylim=c(48,64), col="gray95", fill=TRUE, add=TRUE)  #add the adjacent parts of the US; can't forget my homeland
plot(pcontorta, add=TRUE, xlim=c(-140,-110),ylim=c(48,64), col=alpha("darkgreen", 0.6), border=FALSE)  #plot the species range
10
points(samps$lon, samps$lat, pch=19, col="red", cex=0.5)  #plot my sample sites