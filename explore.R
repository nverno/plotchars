## Create some useful graphics to show plot level characteristics for Moosilauke plots
library(plyr)
library(ggplot2)
library(grid)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")
## Ignore plots 1:3 and data collected for dead trees in 86/87
pp <- subset(pp, !yrmort %in% c(1986, 1987) & pplot > 3)

######################################################################################
####
###    Basal areas
##

## Total basal area by year / plot
bas <- ddply(pp, .(pplot, time), .fun = function(x) {
    x <- droplevels(x)
ggplot(pp, aes(
