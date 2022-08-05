
# load library MASS
library(CCA)
library(MASS)
library(devtools)

source_url('https://raw.github.com/hadley/stringr/master/R/c.r')


# Generate Data (start)

##function to convert correlation matrix to variance-cov maatrix
# R = correlation matrix
# S = vector of standard deviations
cor2cov <- function(R, S) {
  sweep(sweep(R, 1, S, "*"), 2, S, "*")
}

R <- matrix(c(1,0.8,0.8,1),2,2)
S <- c(10,1) 
varCovMat <- cor2cov(R,S)

set.seed(12389)
sample_size <- 600000                                       
sample_meanvector <- c(10, 5, 7, 9, 20)                                   
sample_covariance_matrix <- matrix(c(5, 4, 3, 2, 1, 4, 5, 4, 3, 2,
                                     3, 4, 5, 4, 3, 2, 3, 4, 5, 4, 1, 
                                     2, 3, 4, 5), ncol = 5)

# create multivariate normal distribution
sample_distribution <- mvrnorm(n = sample_size,
                               mu = sample_meanvector, 
                               Sigma = sample_covariance_matrix)

sample2 <- mvrnorm(n = sample_size,
                   mu = c(0.5,3), 
                   Sigma = varCovMat)

# print top of distribution
head(sample_distribution)
ciao=sort(rnorm(sample_size,0,0.1))
ciao2=sort(rnorm(sample_size,4,1))

inputs <- cbind(sample_distribution[,1:2],
                runif(sample_size,0,2),
                ciao2,
                sample_distribution[,3],
                sample2[,1],
                ciao)
outputs <- cbind(sample_distribution[,4:5],
                 sample2[,2],
                 ciao2 + rnorm(sample_size),
                 ciao*2)

# Generate Data (end)

#### calculate the redundancy indeces using the RdInd_calc function
RdIn <- RdInd_calc(inputs, outputs)


##extract results and make some plot
gg <- melt(data.table(t(RdIn),input=paste0("in",1:nFactors)),
           id.vars = "input",variable.name = "output",
           value.name = "RdInd")

ggplot(data=gg,aes(x=input,y=RdInd)) + facet_wrap(~output) + 
  geom_bar(stat='identity')+scale_fill_brewer(palette="Greens")

