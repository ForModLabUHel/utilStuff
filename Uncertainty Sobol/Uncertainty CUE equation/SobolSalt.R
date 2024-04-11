rm(list = ls())
library(sensitivity)
library(ggplot2)
library(ggrepel)
## Settings for Drawing figures
theme_Tian <- function() {
  theme_bw()+
    theme(panel.grid = element_blank()) + # no grid lines
    theme(strip.background = element_rect(fill = "grey95", colour = "grey30"))+
    theme(axis.title = element_text(colour = 'black',size=8,face = 'plain'))+
    theme(axis.text =  element_text(colour = 'black',size=8,face = 'plain'))+
    theme(legend.text = element_text(colour = 'black',size = 8,face = 'plain'))+
    theme(legend.title = element_text(colour = 'black',size=8,face = 'plain'))+
    theme(strip.text = element_text(colour = 'black',size=8,face = 'plain'))
}
theme_set(theme_Tian())

## load the model
source('pre_CUE function.R')

## Ranges of parameters
prior<-read.csv('CUEprior.csv')

## Uncertainty information is often obtained using Monte Carlo technique. 
XX<-t(matrix(runif(nrow(prior)*1e4,min = prior$Min,max = prior$Max),ncol = 1e4))
pl.cue<-data.frame(cue=pre_CUE(XX))

## Histogram overlaid with kernel density curve
ggplot(pl.cue, aes(x=cue)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="burlywood2")+  # Overlay with transparent density plot
  xlab('CUE')

ggsave(
  filename = "uncertainty.jpg",
  width = 15,
  height =10,
  units = "cm",
  dpi = 1000
)

## Using the Variance-based sensitivity analysis, Sobol indices, to partition the Monte Carlo uncertainty. 
nsample<-50000
## This example assumes the parameters follow independent uniform distributions, which is the simplest case.
X1<-t(matrix(runif(nrow(prior)*nsample,min = prior$Min,max = prior$Max),ncol = nsample))
X2<-t(matrix(runif(nrow(prior)*nsample,min = prior$Min,max = prior$Max),ncol = nsample))
colnames(X1)<-prior$Name
colnames(X2)<-prior$Name
## A converged result will need large number of samples and replicates.
sobol.cue <- sobolSalt(model = pre_CUE, X1, X2, scheme="A", nboot = 20000)

sobol.S<-as.data.frame(sobol.cue$S)
sobol.S
sum(sobol.S[,1]) #The sum of the first-order indices should be less than 1 or equal to 1.
sobol.T<-as.data.frame(sobol.cue$T)
sobol.T
sum(sobol.T[,1]) #The sum of the total sensitivity indices should be larger than 1 or equal to 1.

write.csv(sobol.S,file = 'Sobol first order effect.csv')
write.csv(sobol.T,file = 'Sobol total effect.csv')

## Drawing doughnut plots
data0 <- data.frame(category=prior$Name,count=sobol.S[,1])
data<-data0[order(data0$count),]
data[nrow(data),1]<-c(' Interactions')
data[nrow(data),2]<-1-sum(data$count)

data$count[which(data$count<0)]<-0
data$fraction <- data$count / sum(data$count) # Compute percentages
data$ymax <- cumsum(data$fraction) # Compute the cumulative percentages (top of each rectangle)
data$ymin <- c(0, head(data$ymax, n=-1)) # Compute the bottom of each rectangle
data$labelPosition <- (data$ymax + data$ymin) / 2 # Compute label position
data$label <- paste0(data$category, ": ", round(data$count, digits=2)) # Names of each label
# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text_repel(x=2.5, aes(y=labelPosition, label=label, color=category), size=3)+
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  guides(fill=guide_legend(title="Parameters"),color=guide_legend(title="Parameters"))+
  theme_void()
  # theme(legend.position = "none")
ggsave(
  filename = "uncertainty partition.jpg",
  width = 25,
  height =15,
  units = "cm",
  dpi = 1000
)

plot(sobol.cue,choice=1)
ggplot(sobol.cue, choice=1)
