# List of packages for session
.packages = c("ggplot2", "plyr", "rms")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)


require(rgdal)
require(sf)
require(dplyr)


setwd("C:/Users/ftp/Desktop/")
gadm <-  sf::read_sf("gadm36.shp", crs = 4326)
gadm2 <- readOGR("gadm36.shp")
grouped <- gadm %>% 
  group_by(GID_0)


for (i in 2010:2015){
  print(paste("The year is", i))
}




shape <- readOGR(dsn = ".", layer = "SHAPEFILE")
shape <- read_sf(dsn = ".", layer = "SHAPEFILE")

libs <- c("rgdal", "maptools", "gridExtra", "rgeos", "raster")
lapply(libs, require, character.only = TRUE)

LA <- readOGR(dsn = ".", layer = "infuse_dist_lyr_2011_clipped")
IDs <- gadm[sample(nrow(gadm), 100000), ]
IDs <- gadm$ID_1 


gadm_union <- unionSpatialPolygons(gadm, gadm$)

str(gadm)















