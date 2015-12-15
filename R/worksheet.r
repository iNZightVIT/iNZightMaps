data.1 <- read.csv("C:/Users/jason/Desktop/inzight/iNZightVIT/data/QuakesNZ2000.csv", header = TRUE)
library(RgoogleMaps)
library(grid)
setwd('C:/Users/jason/Documents/GitHub/iNZightMaps/R')
source('stupid_loading_method.r')
source('C:/Users/jason/Documents/GitHub/iNZightMaps/R/inzmap.R')
source('C:/Users/jason/Documents/GitHub/iNZightMaps/R/support.fun.R')

iNZightPlot(Latitude, Longitude, data = data.1, plottype = "map", xlab="", ylab="")


library(devtools)
install_github("iNZightVIT/iNZightPlots@dev-plotmethods")
install_github("iNZightVIT/iNZightMaps@dev-integrateplots")

library(iNZightMaps)
data <- whatever
obj <- iNZightMap(...)
plot(obj)


0. get roxygen2 installed and working -- yippee!
1. complete documentation as much as possible
2. test as much as you can (data sets, arguments, col, size etc)
3. shapes!