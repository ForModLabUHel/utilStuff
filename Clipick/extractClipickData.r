# this is needed to run python inside R
library(reticulate)

library(plyr)
library(tidyverse)
library(sf)
library(raster)

source("extractCoords/extractCoords.r")

# specify here the path of python in your computer
# the code works with python 2 but not with python 3
use_python("C:/Python27") 

source_python("Clipick/clipick.py")

# getWeatherData takes the following arguments in this order 
# (location is always needed, all the others are optional): 
#
# Longitude (in degrees) 
# Latitude (in degrees) 
# StartYear = 1951 
# StartMonth = 1 
# StartDay = 1 
# EndYear = 2100 
# EndMonth = 12 
# EndDay = 31


# site = name of the site, shape = shape object, res = resolution in m
# sY = start year, sM = start month, sD = start day, eY = end year, eM = end month, eD = end day
getWD <- function(site, shape, res, sY=1951, sM=1, sD=1, eY=2100, eM=12, eD=31) {
  # here we store the weather data
  wDs <- list()
  
  myRaster <- toRaster(site, shape, res)
  
  grid_points <- rasterToPoints(myRaster)

  # this is the number of coordinate points
  nDFs <- nrow(grid_points)

  for (i in 1:nDFs) {
    lon <- grid_points[i,1]
    lat <- grid_points[i,2]
  
    # getting the weather data from clipick
    result <- getWeatherData(lon, lat, sY, sM, sD, eY, eM, eD)
  
    # weather datas as data.frame
    wD <- data.frame(t(sapply(result,c)), row.names = TRUE)

    wDs[[i]] <- wD
  }
  return(wDs)
}


# this function makes the raster with climIDs
# uniqueWDs = unique weather data frames
# wDs = all weather data frames
# site = site name 
# shape = shape object of site
# res = resolution in m
climRaster <- function(uniqueWDs, wDs, site, shape, res) {
  
  myRaster <- toRaster(site, shape, res)
  
  grid_points <- rasterToPoints(myRaster)
  
  # this is the number of coordinate points
  nDFs <- nrow(grid_points)
  
  nIDs <- length(uniqueWDs) ##this is the number of unique dataframes
  dfIDs <- numeric(nDFs) #create a vector with the dataframes' IDs
  
  #loop to assign the uniqueIDs to each dataframe
  for(i in 1:nIDs){
    IDx <- which(wDs %in% uniqueWDs[i])
    dfIDs[IDx] <- i
  }

  # set up data for the raster
  x <- grid_points[,1]
  y <- grid_points[,2]
  climID <- dfIDs
  rData <- data.frame(x, y, climID)

  rData_sf <- st_as_sf(rData, coords=c("x", "y")) %>%
  st_set_crs(4326)

  climID_sf <- rData_sf[, "climID"]
  climID_sp <- as(climID_sf, "Spatial")
  gridded(climID_sp) = TRUE

  climID_raster <- raster(climID_sp)
  
  return(climID_raster)
}
