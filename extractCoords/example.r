source("extractCoords.r")
source("extractClipickData.r")


# read in the polygon shape files 
aoi_ALTRI <- st_read("inputs/ALTRI.shp")
aoi_FDRP <- st_read("inputs/FDRP.shp")
aoi_FDSM <- st_read("inputs/FDSM.shp")
aoi_FVS <- st_read("inputs/FVS.shp")
aoi_INCDS <- st_read("inputs/INCDS.shp")
aoi_PAYCO <- st_read("inputs/PAYCO.shp")
aoi_UNAC <- st_read("inputs/UNAC.shp")


#### Extract coordinates at 10KMgrid
myRaster <- toRaster("ALTRI",aoi_ALTRI,10000)
grid_points <- rasterToPoints(myRaster)

# extract the Altri weather data from clipick from 1.1.2000-2.1.2000 at 10 km grid
wDs <- getWD("ALTRI", aoi_ALTRI, 10000, 2000, 1, 1, 2000, 1, 2)

# Find the unique dataframes
# weather data for the site is here in order of climIDs!
weather_data <- unique(wDs)

# raster of climID:s is here
climID_raster <- climRaster(weather_data, wDs, "ALTRI", aoi_ALTRI, 80000)
