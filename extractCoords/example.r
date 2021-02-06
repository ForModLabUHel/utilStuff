source("extractCoords/extractCoords.r")
source("Clipick/extractClipickData.r")


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
wDs <- getWD("ALTRI", aoi_ALTRI, 100000, 2000, 1, 1, 2000, 2, 2)


# Find the unique data tables
# weather data for the site is here 
weather_data <- unique(wDs)

# raster of climID:s is here
climID_raster <- climRaster(weather_data, wDs, "ALTRI", aoi_ALTRI, 10000)

# weather data as one data.table, climID as "id"
weather_data_table <- rbindlist(weather_data, idcol="id")

