# Global Gridded Surfaces of Selected Soil Characteristics (IGBP-DIS)
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=569
# Field Capacity and Wilting Point
library(RSAGA)
sites<-read.csv('Locations.csv')
sites$FC<-NA
sites$WT<-NA
data.fc<-read.ascii.grid("fieldcap.dat", return.header = TRUE)
x.fc<-data.fc$data
data.wp<-read.ascii.grid("wiltpont.dat", return.header = TRUE)
x.wp<-data.wp$data
for (i in 1:nrow(sites)) {
  a<-ceiling((sites$Long+180)/0.0833333)[i]
  b<-ceiling((sites$Lat+56.5)/0.083333)[i]
  sites$FC[i]<-x.fc[1686-b,a]
  sites$WT[i]<-x.wp[1686-b,a]
}
summary(sites)
write.csv(sites,file = 'FCWP.csv')
