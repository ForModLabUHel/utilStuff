library(data.table)
library(sf)
library(raster)
library(rgdal)
library(rgeos)
aoi_ALTRI <- readOGR("AOIs/ALTRI.shp")  ###Note for reading the files I'm using a different function readOGR instead of st_read
aoi_FDRP <- readOGR("AOIs/FDRP.shp")
aoi_FDSM <- readOGR("AOIs/FDSM.shp")
aoi_FVS <- readOGR("AOIs/FVS.shp")
aoi_INCDS <- readOGR("AOIs/INCDS.shp")
aoi_PAYCO <- readOGR("AOIs/PAYCO.shp")
aoi_UNAC <- readOGR("AOIs/UNAC.shp")



# This need to be changed accordingly to the example below.
extractEXt <- function(site,shape){
  coordsTab <- data.table(as.matrix(extent(shape)))
  coordsTab$coord <- c("x","y")
  coordsTab$site <- site
  return(coordsTab)
}

###this needs to be changed
# coordsTab <- data.table()
# coordsTab <- rbind(coordsTab,extractEXt("ALTRI",aoi_ALTRI))
# coordsTab <- rbind(coordsTab,extractEXt("FDRP", aoi_FDRP))
# coordsTab <- rbind(coordsTab,extractEXt("FDSM", aoi_FDSM))
# coordsTab <- rbind(coordsTab,extractEXt("FVS", aoi_FVS))
# coordsTab <- rbind(coordsTab,extractEXt("INCDS", aoi_INCDS))
# coordsTab <- rbind(coordsTab,extractEXt("PAYCO", aoi_PAYCO))
# coordsTab <- rbind(coordsTab,extractEXt("UNAC", aoi_UNAC))



# The procidure could be the following.

### project the shapefile to EPSG:21037 (UTM 37S) to deal with meters rather than degrees.
shape_utm <- spTransform(aoi_ALTRI, CRS("+init=epsg:21037"))

# consider the extent of the polygon and create a raster
test_utm_rst <- raster(extent(shape_utm), crs = projection(shape_utm))
res(test_utm_rst) <- 10000 ###set the resolution to 10km
values(test_utm_rst) <- 1 ####assign some values to the pixels 

### project the raster to the original shapefile CRS
rastCRSor <- projectRaster(test_utm_rst,           
                           crs = crs(aoi_ALTRI))
# the values of the pixels are modified by the projectRaster. look at ?projectRaster "method" in the help Arguments
###what you need to do now is to set the raster values to 1 when are in the shapeFile (see ?crop  function in raster package ) and to NA or 0 when outside.


# Some plot to test
plot(shape_utm)
plot(test_utm_rst,add=T)


plot(aoi_ALTRI)
plot(rastCRSor, add=T)
 