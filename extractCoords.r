library(data.table)
library(sf)
library(raster)
aoi_ALTRI <- st_read("AOIs/ALTRI.shp")
aoi_FDRP <- st_read("AOIs/FDRP.shp")
aoi_FDSM <- st_read("AOIs/FDSM.shp")
aoi_FVS <- st_read("AOIs/FVS.shp")
aoi_INCDS <- st_read("AOIs/INCDS.shp")
aoi_PAYCO <- st_read("AOIs/PAYCO.shp")
aoi_UNAC <- st_read("AOIs/UNAC.shp")

extractEXt <- function(site,shape){
  coordsTab <- data.table(as.matrix(extent(shape)))
  coordsTab$coord <- c("x","y")
  coordsTab$site <- site
  return(coordsTab)
}


coordsTab <- data.table()

coordsTab <- rbind(coordsTab,extractEXt("ALTRI",aoi_ALTRI))
coordsTab <- rbind(coordsTab,extractEXt("FDRP", aoi_FDRP))
coordsTab <- rbind(coordsTab,extractEXt("FDSM", aoi_FDSM))
coordsTab <- rbind(coordsTab,extractEXt("FVS", aoi_FVS))
coordsTab <- rbind(coordsTab,extractEXt("INCDS", aoi_INCDS))
coordsTab <- rbind(coordsTab,extractEXt("PAYCO", aoi_PAYCO))
coordsTab <- rbind(coordsTab,extractEXt("UNAC", aoi_UNAC))


