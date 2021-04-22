
# load plot coordinates
load("fixedPlots.rdata")

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

# starting and ending dates for the data extraction
sY=1951
sM=1 
sD=1 
eY=2100 
eM=12 
eD=31

# get the weather data once for each climID only
wDs <- getWDpoints(fixedPlots[climIDpos,], sY, sM, sD, eY, eM, eD)


# put the weather data in prebas-format
prebasWeather(wDs)

# save the siteID's + climID's
save(climID, file="outputs/climID.rdata")





