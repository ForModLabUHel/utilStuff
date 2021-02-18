pre_CUE<-function(PAR){

  Names<-c("c", "r.f", "r.r", "r.w", "sigma.c",
           "WRT.Wf",  "Ww.Wf",   "rho.M",   "c.R", "s.r", 
           "s.R", "s.H", "r.R", "c.H", "r.H",
           "h.M", "ksi")    
  
  c <- PAR[,which(Names == 'c')]
  r.f <- PAR[,which(Names == 'r.f')]
  r.r <- PAR[,which(Names == 'r.r')]
  r.w <- PAR[,which(Names == 'r.w')]
  sigma.c <- PAR[,which(Names == 'sigma.c')]
  WRT.Wf <- PAR[,which(Names == 'WRT.Wf')]
  Ww.Wf <- PAR[,which(Names == 'Ww.Wf')]
  rho.M <- PAR[,which(Names == 'rho.M')]
  c.R <- PAR[,which(Names == 'c.R')]
  s.r <- PAR[,which(Names == 's.r')]
  s.R <- PAR[,which(Names == 's.R')]
  s.H <- PAR[,which(Names == 's.H')]
  r.R <- PAR[,which(Names == 'r.R')]
  c.H <- PAR[,which(Names == 'c.H')]
  r.H <- PAR[,which(Names == 'r.H')]
  h.M <- PAR[,which(Names == 'h.M')]
  ksi <- PAR[,which(Names == 'ksi')]
  
  r.x<- r.r+rho.M*((c.R-c)*s.r+c*(s.R-s.r)+(1+c.H)*h.M*s.H+r.R+h.M*r.H+ksi)
  cue<- 1/(1+c)*(1-(r.f+(r.x)/(1+rho.M)*WRT.Wf+r.w*Ww.Wf)/sigma.c)
  
  cue
  
}

