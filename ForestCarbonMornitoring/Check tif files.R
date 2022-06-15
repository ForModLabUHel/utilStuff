# set working directory to the source file location
# devtools::install_github('https://github.com/ForModLabUHel/Rprebasso')
library(raster)
library(data.table)
library(Rprebasso)
library(ggplot2)

# covert tif/raster to data.table
convert<-function(nameX){
  rasterX<-raster(nameX)
  tableX<-data.table(rasterToPoints(rasterX))
}

# read the inputs of initial states
Gtable<-convert('FCM_IRE_G_10M_1CHS_8BITS.tif')
Htable<-convert('FCM_IRE_H_10M_1CHS_16BITS.tif')
Dtable<-convert('FCM_IRE_D_10M_1CHS_8BITS.tif') 
pPtable<-convert('FCM_IRE_P_pine_10M_1CHS_8BITS.tif')
pSPtable<-convert('FCM_IRE_P_spruce_10M_1CHS_8BITS.tif')
pLAtable<-convert('FCM_IRE_P_larch_10M_1CHS_8BITS.tif')
STtable<-convert('FCM_IRE_SITE_10M_1CHS_8BITS.tif')
SVItable<-convert('FCM_IRE_SVI_10M_1CHS_16BITS.tif')

# rename the columns of data.table
setnames(Gtable,c('x','y','G'))
setnames(Htable,c('x','y','H'))
setnames(Dtable,c('x','y','D'))
setnames(pPtable,c('x','y','pP'))
setnames(pSPtable,c('x','y','pSP'))
setnames(pLAtable,c('x','y','pLA'))
setnames(STtable,c('x','y','ST'))
setnames(SVItable,c('x','y','SVI'))

# remove the non-forested area
Gtable<-Gtable[G<200]
Htable<-Htable[H<2000]
Dtable<-Dtable[D<200]
pPtable<-pPtable[pP<200]
pSPtable<-pSPtable[pSP<200]
pLAtable<-pLAtable[pLA<200]
STtable<-STtable[ST<200]
SVItable<-SVItable[SVI<2000]

# Merge the tables into one table 
alldata<-Reduce(merge,list(Htable,Gtable,Dtable,pPtable,pSPtable,pLAtable,STtable,SVItable))
rm(Htable,Gtable,Dtable,pPtable,pSPtable,pLAtable,STtable,SVItable)

alldata$H<-alldata$H/10 # convert the unit from dm to m
alldata$pB<-100-(alldata$pP+alldata$pSP+alldata$pLA) # proportion of Broad-leaf 
alldata[pB<0,pB:=0]
alldata[,no:=c(1:.N)] # number of the sites
alldata[,nLayers:= sum(c(pP,pSP,pLA,pB)!=0),by=no] # nupmber of the layers
alldata[,nSpecies:=nLayers] # number of species
summary(alldata)

# select some sites for test runs
set.seed(1234)
nSites<-5000
random.sites <-sample(x = 1:nrow(alldata), size = nSites, replace = F)
sites<-alldata[random.sites,]

siteInfo<- data.frame(siteID=c(1:nSites),
                      # climID=sample(x=1:nrow(PAR), size = nSites, replace = T ), # random
                      climID=1:nSites,
                      siteType=sites$ST,
                      SWinit=rep(200,nSites),
                      CWinit=rep(0,nSites),
                      SOGinit=rep(0,nSites),
                      Sinit =rep(10,nSites),
                      nLayers =sites$nLayers,
                      nSpecies=sites$nSpecies,
                      Dsoil=rep(413,nSites),
                      FC=rep(0.450,nSites),
                      WP=rep(0.118,nSites) )

maxNlayers<-max(sites$nLayers)
multiInitVar <- array(0,dim=c(nSites,7,maxNlayers))
multiInitVar[,6,]<- rep(NA,nSites) #Hbc Height of crown base
multiInitVar[,7,]<- rep(NA,nSites) #Asap Sapwood area at crown base
for (i in 1:nSites) {
  pr<-c(sites$pP[i],sites$pLA[i],sites$pSP[i],sites$pB[i])
  species<-c(1,1,2,3) # parameter vector for pine, larch, spruce, and broad-leaf.
  order.species<-order(pr,decreasing = T)
  nlayer<-sites$nLayers[i]
  multiInitVar[i,1,1:nlayer]<-  species[order.species][1:nlayer]#species
  multiInitVar[i,2,1:nlayer]<-  10 #Age
  multiInitVar[i,3,1:nlayer]<-  sites$H[i] #H
  multiInitVar[i,4,1:nlayer]<-  sites$D[i] #DB
  multiInitVar[i,5,1:nlayer]<- sites$G[i]*pr[order.species][1:nlayer]/100#BAH
}

# Climate inputs for the sampling sites
nyear<-10
load('Climate.rdata')
PAR.2 = matrix(data=PAR,nrow = nSites,ncol = nyear*365,byrow = T)
TAir.2 = matrix(data=TAir,nrow = nSites,ncol = nyear*365,byrow = T)
VPD.2 = matrix(data=VPD,nrow = nSites,ncol = nyear*365,byrow = T)
Precip.2 = matrix(data=Precip,nrow = nSites,ncol = nyear*365,byrow = T)
CO2.2 = matrix(data=CO2,nrow = nSites,ncol = nyear*365,byrow = T)

# run PREBAS
initPrebas <- InitMultiSite(nYearsMS = rep(nyear,nSites),
                            siteInfo=as.matrix(siteInfo),
                            pCROBAS
                            = pCROB,
                            pPRELES=pPREL,
                            defaultThin=0.,
                            ClCut = 0.,
                            multiInitVar = multiInitVar,
                            PAR = PAR.2,
                            TAir= TAir.2,
                            VPD= VPD.2,
                            Precip= Precip.2,
                            CO2= CO2.2)
output<- multiPrebas(initPrebas)
multOut<-output$multiOut

pldata<-data.frame(
  CAI=multOut[,1,43,1,1]+multOut[,1,43,2,1]+multOut[,1,43,3,1]+multOut[,1,43,4,1],
  SVI=sites$SVI
)

pl<-ggplot(pldata, aes(CAI, SVI)) + 
  geom_point(shape=1)+
  geom_abline(slope=1,intercept=0,color='darkred'  )+
  coord_fixed()+
  xlim(min(pldata),max(pldata))+
  ylim(min(pldata),max(pldata))
ggExtra::ggMarginal(pl, type = "histogram")
