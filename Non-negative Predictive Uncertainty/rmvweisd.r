library(mvtnorm)
#Function 1: random generation function of multivariate Weibull distribution
rmvweisd <- function(n, shape=1, decay=1, corr=diag(length(shape)))
{
  ## extract parameters, do sanity checks, deal with univariate case
  
  if(!is.matrix(corr) || !isSymmetric(corr))
    stop("'corr' must be a symmetric matrix")
  D = ncol(corr)
  
  Ds = length(shape)
  if(Ds > D)
    warning("'shape' longer than width of 'corr', truncating to fit")
  if(Ds != D)
    shape = rep(shape, length.out=D)
  
  Dd = length(decay)
  if(Dd > D)
    warning("'decay' longer than width of 'corr', truncating to fit")
  if(Dd != D)
    decay = rep(decay, length.out=D)
  
  if(D == 1) rweisd(n, shape, decay)
  
  ## generate standard multivariate normal matrix, convert to CDF
  Z = rmvnorm(n, mean=rep(0,dim(corr)[1]), sigma = corr)
  cdf = pnorm(Z)
  
  ## convert to Weibull (WeiSD), return
  sapply(1:D, function(d) qweisd(cdf[,d], shape[d], decay[d]))
}

# Here we use rmvnorm function from mvtnorm package directly
# #Function 2: random generation function of multivariate normal distribtution
# rmvnormal <- function(n, mean=NULL, cov=NULL)
# {
# 	## munge parameters PRN and deal with the simplest univariate case
# 
# 	if(is.null(mean))
# 		if(is.null(cov))
# 			return(rnorm(n))
# 		else
# 			mean = rep(0, nrow(cov))
# 	else if (is.null(cov))
# 		cov = diag(length(mean))
# 
# 	## gather statistics, do sanity checks
# 
# 	D = length(mean)
# 	if (D != nrow(cov) || D != ncol(cov))
# 		stop("length of mean must equal nrow and ncol of cov")
# 
# 	E = eigen(cov, symmetric=TRUE)
# 	if (any(E$val < 0)){
# 	  stop("Numerically negative definite covariance matrix")
# 	  warning("Numerically negative definite covariance matrix")}
# 
# 	## generate values and return
# 	mean.term = mean
# 	covariance.term = E$vec %*% (t(E$vec) * sqrt(E$val))
# 	independent.term = matrix(rnorm(n*D), nrow=D)
# 
# 	drop(t(mean.term + (covariance.term %*% independent.term)))
# }

#Function 3: quantile function of Weibull distribution
qweisd <- function(p, shape, decay, lower.tail=TRUE, log.p=FALSE)
  qweibull(p, shape=shape, scale=decay^(-1/shape),
           lower.tail=lower.tail, log.p=log.p)


