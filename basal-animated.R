## Plot basal area distributions, animated across years
library(animation)
library(ggplot2)
library(plyr)
library(grid)

## data
canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")
## Ignore plots 1:3 and data collected for dead trees in 86/87
pp <- subset(pp, !yrmort %in% c(1986, 1987) & pplot > 3)

## choose plot layout
tst <- subset(pp, pplot == 4)
symbols(x = tst$bqudx, y = tst$bqudy, circles = tst$ba, inches = 1/3, ann = F,
        bg = "steelblue2", fg = NULL)

## Create basal area animation for all permanent plots, xy-locations
dir.create("~/work/plotchars/plot-ba-xy-animate")
for (pplot in c(4:27)) {
    tst <- pp[pp$pplot == pplot,]
    ## simple plot basal area animation
    png(file = "~/work/plotchars/plot-ba-xy-animate/%02d.png", width = 600, height = 600)
    if (unique(tst$pplot) < 16) {
        yrs <- c(86, 98, 10)
    } else { yrs <- c(87, 98, 10) }
    for (yr in yrs) {
        samp <- tst[tst$time == yr,]
        samp$spec <- as.numeric(samp$spec)
        symbols(x = samp$bqudx, y = samp$bqudy, circles = samp$ba, inches = 1/3, ann = F,
                bg = "steelblue2", fg = NULL, xlim = c(0, 11),  ylim = c(0, 11), col = as.numeric(samp$spec))
    }
    dev.off()
    setwd("~/work/plotchars/plot-ba-xy-animate")
    shell(cmd = '"convert -delay 80 *.png example.gif"')
    file.rename("example.gif", paste0("plot",pplot,".gif"))
    file.remove(list.files(path = "~/work/plotchars/plot-ba-xy-animate/", pattern=".png"))
}

## Create basal area animation for all permanent plots, histograms
dir.create("~/work/plotchars/plot-ba-hist-animate")
for (pplot in c(4:27)) {
    tst <- pp[pp$pplot == pplot & !is.na(pp$dbh) & pp$dbh >= 5,]
    maxba <- max(tst$ba, na.rm = T)
    ## simple plot basal area animation
    png(file = "~/work/plotchars/plot-ba-hist-animate/%02d.png", width = 600, height = 600)
    if (is.na(unique(tst$pplot)))
        print(pplot)
    if (unique(tst$pplot) < 16) {
        yrs <- c(86, 98, 10)
    } else { yrs <- c(87, 98, 10) }
    for (yr in yrs) {
        samp <- tst[tst$time == yr,]
        hist(samp$ba, main = paste0("plot ",pplot,", year ",yr), xlim = c(0, maxba))
    }
    dev.off()
    setwd("~/work/plotchars/plot-ba-hist-animate")
    shell(cmd = '"convert -delay 80 *.png example.gif"')
    file.rename("example.gif", paste0("plot",pplot,".gif"))
    file.remove(list.files(path = "~/work/plotchars/plot-ba-hist-animate/", pattern=".png"))
}

## GRAPHS
dir.create("~/work/plotchars/plot-ba-graphs")

## Max ba per plot
samp <- pp[!is.na(pp$dbh) & pp$dbh >= 5,]
samp[samp$time == 10,]$time <- 100
samp$pplot <- as.factor(samp$pplot)
maxbas <- ddply(samp, .(pplot,time), .fun = function(x) {
    x <- droplevels(x)
    data.frame(maxba = max(x$ba))
})
ggplot(maxbas, aes(time, maxba, group = pplot, col = pplot)) +
    geom_point() + geom_path(arrow = arrow()) + ylab("max basal area in plot")
ggsave("~/work/plotchars/plot-ba-graphs/max-ba.png")

## Summed ba per plot
samp <- pp[!is.na(pp$dbh) & pp$dbh >= 5,]
samp[samp$time == 10,]$time <- 100
samp$pplot <- as.factor(samp$pplot)
plotbas <- ddply(samp, .(pplot,time), .fun = function(x) {
    x <- droplevels(x)
    data.frame(sumba = sum(x$ba))
})
ggplot(plotbas, aes(time, sumba, group = pplot, col = pplot)) +
    geom_point() + geom_path(arrow = arrow()) + ylab("total basal area in plot")
ggsave("~/work/plotchars/plot-ba-graphs/total-ba.png")

## which plots show decrease in max ba?
## 9, 15, 24 are the only plots that showed decreased in the maximum ba
mbas <- reshape(maxbas, idvar = "pplot", v.names = c("maxba"), timevar = "time", direction = "wide")
decreased <- unique(droplevels(mbas[mbas$maxba.100 < mbas$maxba.86 | mbas$maxba.100 < mbas$maxba.87, ])$pplot)

## which plots show decrease in total ba
## 18, 19, 20, 21, 24 are the only plots that showed decreased in the total ba
tbas <- reshape(plotbas, idvar = "pplot", v.names = c("sumba"), timevar = "time", direction = "wide")
decreased <- unique(droplevels(tbas[tbas$sumba.100 < tbas$sumba.86 | tbas$sumba.100 < tbas$sumba.87, ])$pplot)
