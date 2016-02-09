library(maptools)
library(grid)
library(RColorBrewer)
library(iNZightPlots)
library(RgoogleMaps)
library(countrycode)
library(stringr)
library(iNZightMaps)






data("nzquakes")
data.2 = nzquakes

system.time(iNZightPlot(Longitude,Latitude,data = data.2,colby = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap')))
ClickOnZoom()


data("nzquakes")
iNZightPlot(Longitude,Latitude,data = nzquakes,colby = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap'))
ClickOnZoom(ratio = 2)
