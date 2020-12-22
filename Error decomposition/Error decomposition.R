### error discomposion
## for the old growth data: with out thinning strategy
MSEdec <- function(var,obs,sim,method){
  if(method==1 | method=="KobSal"){
    var =var
    xbar = mean(sim)
    ybar = mean(obs)
    x_i = sim
    y_i = obs
    n = length(sim)
    mse = sum((x_i-y_i)^2)/n
    sb = (xbar-ybar)^2   ##bias of the simulation from the measurement 
    sd_s = sqrt((sum((x_i-xbar)^2))/n) ##standard deviation of the simulation
    sd_m = sqrt((sum((y_i-ybar)^2))/n) ##standard deviation of the measurement
    r =((sum((x_i-xbar)*(y_i-ybar)))/n)/(sd_m*sd_s)  ##correlation coefficient between the simulation and measurement
    sdsd = (sd_s-sd_m)^2  ##the difference in the magnitude of fluctuation between the simulation and measurement
    lcs = 2*sd_s*sd_m*(1-r)    ##lack of positive correlation weighted by the standard deviations
    return(list(var=var,sb=sb,sdsd=sdsd,lc=lcs,mse=mse))
  }
  if(method==2 | method=="GauchAl"){
    xbar = mean(sim)
    ybar = mean(obs)
    x_i = sim
    y_i = obs
    n = length(sim)
    mse = sum((x_i-y_i)^2)/n
    model <-lm(y_i~x_i)
    s_l = model$coef[2]
    r_2 =summary(model)$adj.r.squared
    sb = (xbar-ybar)^2
    sd_s = sqrt((sum((x_i-xbar)^2))/n) 
    sd_m = sqrt((sum((y_i-ybar)^2))/n) 
    nu = (1-s_l)^2*(sum((x_i-xbar)^2/n))
    lc = (1-r_2)*(sum((y_i-ybar)^2/n))
    return(list(var=var,sb=sb,nu=nu,lc=lc,mse=mse))
  }
}

read.csv("outdata0.csv")
resultV1<- MSEdec("V",allData0[which(allData0$var=="V")]$obs,allData0[which(allData0$var=="V")]$sim,method = 1)
resultV2<- MSEdec("V",allData0[which(allData0$var=="V")]$obs,allData0[which(allData0$var=="V")]$sim,method = 2)
resultB1<- MSEdec("B",allData0[which(allData0$var=="B")]$obs,allData0[which(allData0$var=="B")]$sim,method = 1)
resultB2<- MSEdec("B",allData0[which(allData0$var=="B")]$obs,allData0[which(allData0$var=="B")]$sim,method = 2)
resultH1<- MSEdec("H",allData0[which(allData0$var=="H")]$obs,allData0[which(allData0$var=="H")]$sim,method = 1)
resultH2<- MSEdec("H",allData0[which(allData0$var=="H")]$obs,allData0[which(allData0$var=="H")]$sim,method = 2)
resultHc1<- MSEdec("Hc",allData0[which(allData0$var=="Hc")]$obs,allData0[which(allData0$var=="Hc")]$sim,method = 1)
resultHc2<- MSEdec("Hc",allData0[which(allData0$var=="Hc")]$obs,allData0[which(allData0$var=="Hc")]$sim,method = 2)
resultD1<- MSEdec("D",allData0[which(allData0$var=="D")]$obs,allData0[which(allData0$var=="D")]$sim,method = 1)
resultD2<- MSEdec("D",allData0[which(allData0$var=="D")]$obs,allData0[which(allData0$var=="D")]$sim,method = 2)
resultN1<- MSEdec("N",allData0[which(allData0$var=="N")]$obs,allData0[which(allData0$var=="N")]$sim,method = 1)
resultN2<- MSEdec("N",allData0[which(allData0$var=="N")]$obs,allData0[which(allData0$var=="N")]$sim,method = 2)

method_Korb <- Map(c,resultV1,resultB1,resultH1,resultHc1,resultD1,resultN1)
method_Korb <- data.frame(matrix(unlist(method_Korb), nrow=length(method_Korb), byrow=T))
row.names(method_Korb)<-c("Var","sb","sdsd","lc","mse")
method_Gauchal <- Map(c,resultV2,resultB2,resultH2,resultHc2,resultD2,resultN2)
method_Gauchal <- data.frame(matrix(unlist(method_Gauchal), nrow=length(method_Gauchal), byrow=T))
row.names(method_Gauchal)<-c("Var","sb","nu","lc","mse")
