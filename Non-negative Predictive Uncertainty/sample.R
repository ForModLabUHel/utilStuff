library(FAdist)
library(mvtnorm)
# library(MASS)
source('Fweibull_Arithmetic Mean and Variance.R') # Moment-based parameter recovery method for location,scale,shape parameters
# source('Fweibull_Quadratic Mean and Variance.R')
# source('distributions.R') ## lcmix & mvweisd
source('rmvweisd.R') # Multivariate Weibull using shape and decay parameters


# Moment-based parameter recovery for the parameters in Weibull pdf
abc<-data.frame(rbind(Fweibull(2,10),Fweibull(5,2),Fweibull(10,3)))

hist(rweibull3(1000,
          thres=0,
          scale=abc$b[2],
          shape=abc$c[2]),nclass =30) 

shd<-data.frame(shape=abc$c,decay=abc$b^(-abc$c))

## For details see: https://rdrr.io/rforge/lcmix/man/mvweisd.html
set.seed(123)
s <- shd$shape
d <- shd$decay

##	the correlation matrix
rho <- matrix(c(1, 0.3, 0.5,
                0.3, 1, -0.6,
                0.5,-0.6,1),
                  ncol=3)
## FYI. Normal copular method use a multivariate normal distribution with marginal means of 0 and
## marginal variances of 1.Thus the covariance matrix is the same as the correlation matrix in this context.

x <- rmvweisd(10000, s, d, rho)

## FYI. A commen error here is 'Numerically negative definite covariance matrix' or
## 'Not positive definite'. That is an algebraic statement that some of the variables are
# linear combinations of one another. The covariance matrix is not positive definite because 
## it is singular. That means that at least one of those variables can be expressed as a linear combination of the others. 
## You do not need all the variables as the value of at least one can be determined from a subset of the others.
# library(MASS) # In MASS, treated as an error
# y<-mvrnorm(n=1000,mu=rep(0,ncol(rho)),Sigma = rho)
# library(mvtnorm) # In mvtnorm, treated as a warning
# y<-rmvnorm(n=1000,mean=rep(0,ncol(rho)),sigma = rho)

plot(x[,1],x[,2])
plot(x[,3],x[,2])
mean(x[,1])
mean(x[,2])
mean(x[,3])
hist(x[,1],nclass = 30)
hist(x[,2],nclass = 30)
hist(x[,3],nclass = 30)



