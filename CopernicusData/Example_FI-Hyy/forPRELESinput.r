rm(list = ls())
library(imputeTS)
library(lubridate)

data<-read.csv('data.csv') #data from CDS
data$ymd<-substr(data$time,1,10) 

data$SW<-na_interpolation(data$rsds,option='linear') #linear interpolation for shortwave redation
data<-data[which(!is.na(data$tas)),] #remove the NA rows

# The shortwave radiation (SW, W /m2) can be converted to PAR (mol/m2/day) 
# External to the earth’s atmosphere, the ratio of PAR to total solar radiation is 0.44. 
# Then we can use 4.56 (µmol/J or mol/MJ) to convert the unit as PRELES requests. 
data$PAR <- data$SW*0.44*4.56/1e6*60*60*24 

# Temperature in the atmosphere. It has units of Kelvin (K). 
# Temperature measured in kelvin can be converted to degrees Celsius (°C) by subtracting 273.15.
data$TAir<-data$tas-273.15

# Precipetation from kg m-2 s-1 or mm s-1 to mm day-1
data$Precip<-data$pr*60*60*24

##' Convert specific humidity to relative humidity
##' from Bolton 1980 The computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @author David LeBauer in Stack
qair2rh <- function(qair, temp, press = 1013.25){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

SVP <-  610.7*10^(7.5*data$TAir/(237.3+data$TAir))
RH<-qair2rh(data$huss,data$TAir)
data$VPD <- SVP*(1-RH)/1000

dataPreles<-data.frame(Lat=data$lat,Lon=data$lon,Date=data$ymd,DoY=yday(data$ymd),
                       PAR=data$PAR,TAir=data$TAir,VPD=data$VPD,Precip=data$Precip)
write.csv(dataPreles,'dataPreles.csv')
