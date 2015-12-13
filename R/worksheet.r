data.1 <- read.csv("C:/Users/jason/Desktop/inzight/iNZightVIT/data/QuakesNZ2000.csv", header = TRUE)
library(RgoogleMaps)
library(grid)
source('C:/Users/jason/Desktop/inzight/iNZightMaps-dev-integrateplots/R/stupid_loading_method.r')
source('C:/Users/jason/Desktop/inzight/iNZightMaps-dev-integrateplots/R/inzmap.R')
source('C:/Users/jason/Desktop/inzight/iNZightMaps-dev-integrateplots/R/support.fun.R')

iNZightPlot(Longitude, Latitude, data = data.1, plottype = "map", xlab="", ylab="")
iNZightPlot(colby = Felt,g1 = Month,Month, Month, data = data.1, plottype = "map", xlab="", ylab="")

