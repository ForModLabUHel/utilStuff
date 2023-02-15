# this is needed to run python inside R
library(reticulate)

library(plyr)
library(tidyverse)
library(sf)
library(raster)

source("https://raw.githubusercontent.com/ForModLabUHel/utilStuff/master/extractCoords/extractCoords.r")

# specify here the path of python in your computer
# the code works with python 2 but not with python 3
use_python("C:/Python27")

source_python("https://raw.githubusercontent.com/ForModLabUHel/utilStuff/master/Clipick/clipick.py")

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


# site = name of the site, shapeRast = shape object or Raster, res = resolution in m
# sY = start year, sM = start month, sD = start day, eY = end year, eM = end month, eD = end day
getWD <- function(site, shapeRast, res, sY=1951, sM=1, sD=1, eY=2100, eM=12, eD=31) {
  # here we store the weather data
  wDs <- list()
  if(class(shapeRast)=="RasterLayer"){
    myRaster <- shapeRast
  }else{
    myRaster <- toRaster(site, shape, res)
  }
  grid_points <- rasterToPoints(myRaster)
  
  # this is the number of coordinate points
  nDFs <- nrow(grid_points)

  for (i in 1:nDFs) {
    lon <- grid_points[i,1]
    lat <- grid_points[i,2]
    
    # getting the weather data from clipick
    result <- getWeatherData(lon, lat, sY, sM, sD, eY, eM, eD)
    
    # weather datas as data.table
    wD <- data.table(t(sapply(result,c)))
    
    # fix the column names
    names(wD) <- as.character(wD[1,])
    wD <- wD[-1,]
    
    # this is the number of the day
    wD$rday <- 1:nrow(wD)
    
    wDs[[i]] <- wD
  }
  
  return(wDs)
}

# coordPlots = coordinates of plots; must have columns long and lat
# sY = start year, sM = start month, sD = start day, eY = end year, eM = end month, eD = end day
getWDpoints <- function(coordPlots, sY=1951, sM=1, sD=1, eY=2100, eM=12, eD=31) {
  # here we store the weather data
  wDs <- list()
  
  # this is the number of coordinate points
  nDFs <- nrow(coordPlots)
  
  for (i in 1:nDFs) {
    lon <- coordPlots$long[i]
    lat <- coordPlots$lat[i]
    
    # getting the weather data from clipick
    result <- getWeatherData(lon, lat, sY, sM, sD, eY, eM, eD)
    
    # weather datas as data.table
    wD <- data.table(t(sapply(result,c)))
    
    # fix the column names
    names(wD) <- as.character(wD[1,])
    wD <- wD[-1,]
    
    # this is the number of the day
    wD$rday <- 1:nrow(wD)
    
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

# this function returns site id + climID
# uniqueWDs = unique weather data frames
# wDs = all weather data frames
# ids = site id's in the same order 
climIDlist <- function(uniqueWDs, wDs, ids) {
  # this is the number of coordinate points
  nDFs <- length(ids)
  
  nIDs <- length(uniqueWDs) ##this is the number of unique dataframes
  dfIDs <- numeric(nDFs) #create a vector with the dataframes' IDs
  
  #loop to assign the uniqueIDs to each dataframe
  for(i in 1:nIDs){
    IDx <- which(wDs %in% uniqueWDs[i])
    dfIDs[IDx] <- i
  }
  
  climIDs <- cbind(ids, dfIDs)
  
  colnames(climIDs) <- c("id", "climID")
  
  return(climIDs)
}

# creates weather input for Prebas and saves it as .rdata file
# wDs is list of weather data as it comes from clipick
# ! here you already need to know the climID's and have removed the duplicates!
prebasWeather <- function(wDs) {
  
  nclim <- length(wDs)
  
  nDays <- nrow(wDs[[1]])
  
  # fix the data
  for(i in 1:nclim) {
    
    # set date column 
    wDs[[i]]$date <- paste0(wDs[[i]]$Year, "-", wDs[[i]]$Month, "-", wDs[[i]]$Day)
    
    # count mean temperature
    wDs[[i]]$TAir <- (as.double(wDs[[i]]$tasmax)+as.double(wDs[[i]]$tasmin))/2
    
    # VPD
    wDs[[i]]$SVP <- 610.7*10^(7.5*wDs[[i]]$TAir/(237.3+wDs[[i]]$TAir))
    wDs[[i]]$VPD <- wDs[[i]]$SVP*(1-(as.double(wDs[[i]]$hursmax)+as.double(wDs[[i]]$hursmin))/2/100)/1000
    
    # PAR in right unit
    wDs[[i]]$PAR <- as.double(wDs[[i]]$rss)*0.44*4.968
    
    # Precip 
    wDs[[i]]$Precip <- as.double(wDs[[i]]$pr)
    
  }
  
  # create matrices for weather data for nDays
  PAR<-matrix(NA,nrow = nclim, ncol=nDays)
  TAir<-matrix(NA,nrow = nclim,ncol=nDays)
  Precip<-matrix(NA,nrow = nclim,ncol=nDays)
  VPD<-matrix(NA,nrow = nclim,ncol=nDays)
  CO2<-matrix(380,nrow = nclim,ncol=nDays)
  
  # fill the weather matrices
  for(i in 1:nclim) {
    PAR[i,] <- wDs[[i]]$PAR
    TAir[i,] <- wDs[[i]]$TAir
    Precip[i,] <- wDs[[i]]$Precip
    VPD[i,] <- wDs[[i]]$VPD
  }
  
  save(TAir, Precip, PAR, VPD, CO2, file="outputs/weather.rdata")
  
}
