source("extractCoords.r")


# read in the polygon shape files 
aoi_ALTRI <- st_read("inputs/ALTRI.shp")
aoi_FDRP <- st_read("inputs/FDRP.shp")
aoi_FDSM <- st_read("inputs/FDSM.shp")
aoi_FVS <- st_read("inputs/FVS.shp")
aoi_INCDS <- st_read("inputs/INCDS.shp")
aoi_PAYCO <- st_read("inputs/PAYCO.shp")
aoi_UNAC <- st_read("inputs/UNAC.shp")


#### Extract coordinates at 10KMgrid
gridPoints("ALTRI",aoi_ALTRI)
gridPoints("FDRP",aoi_FDRP)
