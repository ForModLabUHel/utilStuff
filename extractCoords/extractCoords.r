library(data.table)
library(sf)
library(raster)
library(dplyr)
# library(lwgeom)
# test

# this is the function that extracts the extreme point coordinates
# "site" is the name of the site, "shape" is the shape file
extractEXt <- function(site,shape){
  coordsTab <- data.table(as.matrix(extent(shape)))
  coordsTab$coord <- c("x","y")
  coordsTab$site <- site
  return(coordsTab)
}



# this function converts the shape object to about 10 x 10 km raster 
# and that to points and writes the coordinates of the points to a .txt file
gridPoints <- function(site, shape) {
  
  test_ext <- extractEXt(site, shape)
  
  # south-west coordinates
  sw <- st_point(c(test_ext$min[1], test_ext$min[2])) %>%
    st_sfc() %>%
    st_set_crs(4326)
  
  # north-west coordinates
  nw <- st_point(c(test_ext$min[1], test_ext$max[2])) %>%
    st_sfc() %>%
    st_set_crs(4326)
  
  # south-east coordinates
  se <- st_point(c(test_ext$max[1], test_ext$min[2])) %>%
    st_sfc() %>%
    st_set_crs(4326)
  
  # raster height and width, 10 km grid
  raster_width <- round(st_distance(sw, se)/10000)
  raster_height <- round(st_distance(sw, nw)/10000)
  
  # converts shapefile to a raster
  r <- raster(as(shape, "Spatial"), ncols = raster_width, nrows = raster_height)
  rr <- rasterize(as(shape, "Spatial"), r, getCover = FALSE)
  
  # converts the raster to spatial points
  grid_points <- rasterToPoints(rr)
  
  # writes the coordinates of the points to txt-file
  namestring <- paste("outputs/gridPoints_", site, ".txt", sep = "")
  write.table(grid_points[,1:2], namestring, row.names = FALSE)
}



