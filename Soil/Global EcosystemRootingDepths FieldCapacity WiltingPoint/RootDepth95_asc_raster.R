# 95ecosys_rootdepth_1d.asc: Mean 95% ecosystem rooting depth in meters. This 
# is an estimation of the rooting depth that contains 95% of all roots. "1d" implies 
# a 1-degree spatial resolution (lat/long).
# The dataset is from https://daac.ornl.gov/ISLSCP_II/guides/ecosystem_roots_1deg.html
library(raster)
x<-raster('95ecosys_rootdepth_1d.asc', gz = FALSE)
sites<-read.csv('Locations.csv')
sites$rootdepth<-NA
for (i in 1:nrow(sites)) {
  if (!is.na(x[ceiling(90-sites$Lat)[i],ceiling(sites$Long+180)[i]])) {
    sites$rootdepth[i]<-x[ceiling(90-sites$Lat)[i],ceiling(sites$Long+180)[i]]
  }
  else if  (!is.na(x[ceiling(90-sites$Lat)[i],round(sites$Long+180)[i]+1])){
    sites$rootdepth[i]<-x[ceiling(90-sites$Lat)[i],round(sites$Long+180)[i]+1]
  }
  else if  (!is.na(x[round(90-sites$Lat)[i]+1,ceiling(sites$Long+180)[i]])){
    sites$rootdepth[i]<-x[round(90-sites$Lat)[i]+1,ceiling(sites$Long+180)[i]]
  }
  else if  (!is.na(x[round(90-sites$Lat)[i],round(sites$Long+180)[i]-1])){
    sites$rootdepth[i]<-x[round(90-sites$Lat[i]),round(sites$Long+180)[i]-1]
  }
  else if  (!is.na(x[round(90-sites$Lat)[i]-1,round(sites$Long+180)[i]])){
    sites$rootdepth[i]<-x[round(90-sites$Lat[i])-1,round(sites$Long+180)[i]]
  }
  else if  (!is.na(x[round(90-sites$Lat)[i]+1,round(sites$Long+180)[i]+1])){
    sites$rootdepth[i]<-x[round(90-sites$Lat[i])+1,round(sites$Long+180)[i]+1]
  }
}
summary(sites)
write.csv(sites,file = 'rooting_depth.csv')
