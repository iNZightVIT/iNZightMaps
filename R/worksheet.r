data.1 <- read.csv("C:/Users/jason/Desktop/inzight/iNZightVIT/data/QuakesNZ2000.csv", header = TRUE)
library(RgoogleMaps)
library(grid)
setwd('C:/Users/jason/Documents/GitHub/iNZightMaps/R')
source('stupid_loading_method.r')
source('C:/Users/jason/Documents/GitHub/iNZightMaps/R/inzmap.R')
source('C:/Users/jason/Documents/GitHub/iNZightMaps/R/support.fun.R')

iNZightPlot(Longitude, Latitude, data = data.1, plottype = "map", xlab="", ylab="")
