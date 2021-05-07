rm(list = ls()) # clear the workspace

# Packages: Update below packages if eorrors occur------------------------------
library(MODISTools)
library(rlang)
library(pushoverr)
library(memoise)

# Listing MODIS products / bands / dates ---------------------------------------
products <- mt_products()
View(products)

# Daily GPP
# MOD17A2H:MODIS/Terra Gross Primary Productivity (GPP)
# 8-Day L4 Global 500 m SIN Grid
bands <- mt_bands(product = "MOD17A2H")
View(bands)

dates <- mt_dates(product = "MOD17A2H", lat = 62, lon = 24)
head(dates)
tail(dates)

# Downloading MODIS time series -------------------------------------------
# download the MODIS daily GPP data for Hyytiala

Hyy_GPP <- mt_subset(product = "MOD17A2H",
                     lat = 61.8474,
                     lon =  24.2948,
                     band = "Gpp_500m",
                     start = "2004-01-01",
                     end = "2004-12-30",
                     km_lr = 0,# km left-right to sample (rounded to the nearest integer)
                     km_ab = 0,# km above-below to sample (rounded to the nearest integer)
                     site_name = "FI-Hyy",
                     internal = T, #should the data be returned as an internal data structure
                     progress = T #show download progress
) 

Hyy_Psn_QC <- mt_subset(product = "MOD17A2H",
                        lat = 61.8474,
                        lon =  24.2948,
                        band = "Psn_QC_500m",
                        start = "2004-01-01",
                        end = "2004-12-30",
                        km_lr = 0,# km left-right to sample (rounded to the nearest integer)
                        km_ab = 0,# km above-below to sample (rounded to the nearest integer)
                        site_name = "FI-Hyy",
                        internal = T, #should the data be returned as an internal data structure
                        progress = T #show download progress
) 
lat_lon <- sin_to_ll(Hyy_GPP$xllcorner, Hyy_GPP$yllcorner)
Hyy_GPP$lat<-lat_lon[,1]
Hyy_GPP$log<-lat_lon[,2]
head(Hyy_GPP$scale)
scale<-as.numeric(Hyy_GPP$scale)
Hyy_GPP$DailyGPP<-Hyy_GPP$value*scale/8*1000 ## Unit: from kgC/m^2 in 8 days to gC/m2/day
write.csv(Hyy_GPP,'Hyy_GPP.csv')

# Plot --------------------------------------------------------------------
library(ggplot2)
pldata<-Hyy_GPP

ggplot(pldata, aes(x=as.Date(calendar_date), y = DailyGPP)) + 
  geom_point()+
  geom_line(colour = "#2D708EFF") + # draw the line for the mean value
  ylab(expression(paste('GPP(gC/', m^2,'/d)'))) + # add a y-axis label
  xlab("Date")   # add a x-axis label

# An example batch download data frame ------------------------------------
# download the MODIS daily GPP data for Hyytiala and Sodankyla
# create data frame with a site_name, lat and lon column
# holding the respective names of sites and their location
df <- data.frame("site_name" = c('Hyy','Sod'), stringsAsFactors = FALSE)
df$lat <- c(61.8474, 67.3624)
df$lon <- c(24.2948, 26.6386)
# an example batch download data frame
MultiSites <- mt_batch_subset(df = df,
                              product = "MOD17A2H",
                              band = "Gpp_500m",
                              km_lr = 0,
                              km_ab = 0,
                              start = "2004-01-01",
                              end = "2004-12-30",
                              internal = TRUE,
                              ncores = 2 # number of cores to use while downloading in parallel (auto will select the all cpu cores - 1 or 10)
)
write.csv(MultiSites,'MultiSites.csv')

# convert the coordinates
lat_lon <- sin_to_ll(MultiSites$xllcorner, MultiSites$yllcorner)

#### Additional instructions:

# When a large selection of locations is needed you might benefit from using 
# the batch download function mt_batch_subset(), which provides a wrapper around
# the mt_subset() function in order to speed up large download batches. 

# MOD17A2H ----------------------------------------------------------------
# The MOD17A2H Version 6 Gross Primary Productivity (GPP) product is a cumulative
# 8-day composite of values with 500 meter (m) pixel size based on the radiation use
# efficiency concept that can be potentially used as inputs to data models to calculate
# terrestrial energy, carbon, water cycle processes, and biogeochemistry of vegetation.
# The data product includes information about GPP and Net Photosynthesis (PSN).
# The PSN band values are the GPP less the Maintenance Respiration (MR). The data
# product also contains a PSN Quality Control (QC) layer. The quality layer contains
# quality information for both the GPP and the PSN.