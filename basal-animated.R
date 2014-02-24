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

symbols(x=dfx$ev1, y=dfx$ev2, circles=dfx$ev3, inches=1/3, ann=F, bg="steelblue2", fg=NULL)

## set delay between frames for replay
ani.options(interval = 0.05)

## colors
col.range <- heat.colors(15)

dir.create("~/work/plotchars/plot-ba-animate")

## simple plot basal area animation
png(file = "~/work/plotchars/plot-ba-animate/%02d.png", width = 600, height = 600)
for (yr in c(86, 98, 10)) {
    samp <- tst[tst$time == yr,]
    samp$spec <- as.numeric(samp$spec)
    symbols(x = samp$bqudx, y = samp$bqudy, circles = samp$ba, inches = 1/3, ann = F,
            bg = "steelblue2", fg = NULL, xlim = c(0, 11),  ylim = c(0, 11), col = as.numeric(samp$spec))
}
dev.off()

setwd("~/work/plotchars/plot-ba-animate")
shell(cmd = '"convert -delay 80 *.png example_4.gif"')

# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))
