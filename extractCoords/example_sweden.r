library(mapview)

source("extractCoords/extractCoords.r")
source("Clipick/extractClipickData.r")

#########


load("inputs/coordPlots.rdata")








sY=1993
sM=1 
sD=1 
eY=1993 
eM=3 
eD=1




# take 100 sites 
test <- coordPlots[1:100,]

# get weather data for all test sites
wDs_test <- getWDpoints(test, 2000, 1, 1, 2000, 1, 2)

# take unique weather datas
weather_data <- unique(wDs_test)

# here's siteID + climID
test_climID <- climIDlist(weather_data, wDs_test, test$id)

# vector of climIDs
climIDs <- unique(test_climID[,2])

# vector of first positions of each clim IDs
climIDpos <- match(climIDs, test_climID[,2])

# get the weather data once for each climID only
wDs <- getWDpoints(test[climIDpos,], 2000, 1, 1, 2000, 2, 1)

# number of climIDs
nclim <- length(climIDs)


#####-------- HERE I TRY TO GET RID OF THE PLACES WITHOUT DATA-----------

# find the sites where there is no weather data
nodata <- NULL

for(i in 1:length(wDs)) {
  if(is.null(wDs[[i]]$Day[1])) {
    nodata <- c(nodata, i)
  }
}

# let's save the sites with no data
#save(nodata, file="nodata.rdata")

plots_sf <- st_as_sf(coordPlots, coords=c("long", "lat")) %>%
  st_set_crs(4326)

load("nodata.rdata")

# these are the sites with no weather data
mapview(plots_sf[nodata,])

# check if the problematic sites are something we simulate
load("siteIDs.rdata")
#yes they are
plots_sf[nodata,]$id %in% siteInfoX$siteID

misPlots <- coordPlots[nodata,]

misPlots2 <- coordPlots[nodata,]


for (i in 1:nrow(misPlots2)) {
  if(misPlots2[i]$long<13) {
    misPlots2[i]$long <- misPlots2[i]$long+0.22
  } else misPlots2[i]$long <- misPlots2[i]$long-0.22
}

misPlots_sf <- st_as_sf(misPlots2, coords=c("long", "lat")) %>%
  st_set_crs(4326)

mapview(misPlots_sf)

wDs_mis <- getWDpoints(misPlots2, 2000, 1, 1, 2000, 2, 1)

# here's one of each location
mapview(misPlots_sf[c(1,2,5,6,10,11,16),])
mapview(misPlots_sf)

# here I check if moving the coordinates fixes the problem
mapview(misPlots_sf[1,]) # long +0.22
mapview(misPlots_sf[2,]) # lat +0.22 
mapview(misPlots_sf[5,]) # long = 18.69
mapview(misPlots_sf[6,]) # long -0.22
mapview(misPlots_sf[10,]) # ???
mapview(misPlots_sf[11,]) # long +0.22
mapview(misPlots_sf[16,]) # long -0.22


mapview(misPlots_sf[c(5,10),]) # ???

misPlots2[10,2] <- 18.03
misPlots2[10,3] <- 58.98

# make a data.frame for the fixed coordinates
newCoords <- as.data.frame(misPlots[c(1,2,5,6,10,11,16), 2:3])

newCoords$nLong <- newCoords$long
newCoords$nLat <- newCoords$lat

newCoords[1,3] <- newCoords[1,1]+0.22
newCoords[2,4] <- newCoords[2,2]+0.22
newCoords[3,3] <- 18.69
newCoords[4,3] <- newCoords[4,1]-0.22
newCoords[5,3:4] <- c(18.03,58.98)
newCoords[6,3] <- newCoords[6,1]+0.22
newCoords[7,3] <- newCoords[7,1]-0.22


# fix all the coordinates
for(i in 1:nrow(misPlots2)) {
  index <- match(misPlots2[i,2], newCoords[,1])
  misPlots2[i, 2:3] <- newCoords[index,3:4]
}


# put the fixed coordinates back together with the other coordinates
fixedPlots <- as.data.frame(coordPlots)
for (i in 1:nrow(misPlots2)) {
  index <- match(misPlots2[i]$id, fixedPlots$id)
  fixedPlots[index,] <- misPlots2[i,]
}


#####------------------ NODATA SECTION ENDS HERE ------------------



# get weather data for all sites for 2 months
wDs_testset <- getWDpoints(fixedPlots, 2010, 1, 1, 2010, 3, 1)

# take unique weather datas
weather_data <- unique(wDs_testset)

# here's siteID + climID
climID <- climIDlist(weather_data, wDs_testset, fixedPlots$id)

# vector of climIDs
climIDs <- unique(climID[,2])

# vector of first positions of each clim IDs
climIDpos <- match(climIDs, climID[,2])

# get the weather data once for each climID only
wDs <- getWDpoints(fixedPlots[climIDpos,])

# number of climIDs
nclim <- length(climIDs)





# put the weather data in prebas-format
prebasWeather(wDs)

load("outputs/weather.rdata")

# 0. selvitä identtiset koordinaatit?
# 1. hae data 2 kk tms
# 2. selvitä mistä puuttuu dataa ja muuta koordinaatteja
# 3. selvitä climID:t
# 4. hae data vain climID:ille 150 vuodeksi
# 1, 2, 5, 6, 10, 11, 16

# 5. muuta data oikeaan muotoon




