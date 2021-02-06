#### TO USE THIS SCRIPT: Check filepaths in this file and also in extractClipickData.r file.
## Script produces weather data for area defined with a shape file including a polygon of aoi.
## Produced data (climID raster file and weather database) are in a suitable form to be used 
## as an input climate data for PREBAS model. 


source("filePath/extractCoords.r")
source("filePath/extractClipickData.r")


# Read in the polygon shape file/files 
aoi_sitename <- st_read("filePath/extent.shp")


# Extract coordinates at 10KM grid
myRaster <- toRaster("sitename",aoi_sitename,10000)
grid_points <- rasterToPoints(myRaster)


# Extract weather data from clipick from 1.1.2015-31.12.2025 at 10KM grid. 
wDs <- getWD("sitename", aoi_sitename, 10000, 2015, 1, 1, 2025, 12, 31)


# Find the unique data tables.
# Weather data for the site is here
wDs_unique <- unique(wDs)



#### RASTER FILE EXTRACTION
# raster of climID:s is here
climID_raster <- climRaster(wDs_unique, wDs, "sitename",aoi_sitename, 10000)

# Reproject the climID raster to match satellite data files
epsg32636 = "+proj=utm +zone=36 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
climID_raster_final <- projectRaster(climID_raster,
                                       crs = epsg32636, method = "ngb")

# Save the raster as tif-file
writeRaster(climID_raster_final,filename = "filePath/climID.tif",overwrite=T)



#### MODIFYING OUTPUT DATABASE
# Weather data as one data.table, climID as "id"
weather_temp <- rbindlist(wDs_unique, idcol="id")

# Remove leap days
weather_data <- weather_temp[Day != 29 | Month != 2]

# Adding rday column (rday = order number of each day/roew in the whole time period)
weather_data <- weather_data[,rday:=1:length(Day),by=id]

# Covert needed variables to numeric
weather_data <- weather_data[, tasmax:=as.numeric(tasmax)]
weather_data <- weather_data[, tasmin:=as.numeric(tasmin)]
weather_data <- weather_data[, hursmax:=as.numeric(hursmax)]
weather_data <- weather_data[, hursmin:=as.numeric(hursmin)]
weather_data <- weather_data[, rss:=as.numeric(rss)]
weather_data <- weather_data[, pr:=as.numeric(pr)]



#### CONVERT DATA STRUCTURE SO THAT VARIABLES AND UNITS MATCH REQUIREMENTS OF PREBAS MODEL
# This is not necessary: Add DOY (day of the year) column. rep(1:x, y*z): x = days in a year or number of days extracted if time period less than a year,
# y = number of years in the time period, z = number of sites aka. points in the grid.
# weather_data <- weather_data[,DOY:=rep(1:365,8*30)]

# Convert radiation (rss, MJ/m2) to PAR (µmol/J or mol/MJ)
weather_data <- weather_data[,PAR:=rss*0.44*4.56]

# Calculate estimation of the average temperature with maximum and minimum temperature. 
# Note: result is a rather rough estimate of the average daily temperature.
weather_data <- weather_data[,TAir:=(tasmax+tasmin)/2]

# Calculate water-vapor deficit with relative humidity (hursmax, hursmin):
# First calculate the saturation vapor pressure (SVP)
weather_data <- weather_data[,SVP:=610.7*10^(7.5*TAir/(237.3+TAir))]

# Then calculate water-vapor deficit (VPD, unit kPa)
weather_data <- weather_data[,VPD:=SVP*(1-( hursmax+ hursmin)/2/100)/1000]

# Change name  of pr column to Precip
weather_data <- weather_data[,Precip:= pr]

# Add column with CO2 level value
weather_data <- weather_data[,CO2:=380]

# Clean unnecessary columns and organize columns in desired order
dat <- subset(weather_data, select=c(id, rday, PAR, TAir, VPD, Precip, CO2))



# Save the datatable as database
save(dat,file="filePath/clipickWD_sitename.RData")

