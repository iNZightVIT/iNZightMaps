data.1 <- read.csv("C:/Users/jason/Desktop/inzight/iNZightVIT/data/QuakesNZ2000.csv", header = TRUE)

data.1$Latitude = data.1$Latitude - 2
devtools::load_all("C:/Users/jason/Desktop/inzight/iNZightPlots-dev-plotmethods/R/iNZightPlots", export_all = FALSE)
devtools::load_all("C:/Users/jason/Desktop/inzight/iNZightMaps-dev-integrateplots/R/iNZightMaps",export_all = FALSE)
library(RgoogleMaps)
library(grid)
source('C:/Users/jason/Desktop/inzight/stupid_loading_method.r')

source('C:/Users/jason/Desktop/inzight/needed.r')


source('C:/Users/jason/Desktop/inzight/needed.r')
iNZightPlot(Longitude, Latitude, data = data.1, plottype = "map", xlab="", ylab="")
