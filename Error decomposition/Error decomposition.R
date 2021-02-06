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

