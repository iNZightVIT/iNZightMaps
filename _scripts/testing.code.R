library(maptools)
library(grid)
library(RColorBrewer)
library(iNZightPlots)
library(RgoogleMaps)
library(countrycode)
library(stringr)
library(iNZightMaps)



data.2 = read.csv('C:/Users/yeamin/Desktop/GeoNet_CMT_solutions.csv')
data.2$Latitude = data.2$Latitude + 180

iNZightPlot(Longitude,Latitude,data = data.2,colby =dp,g1 = str,
            plottype = 'map',plot.features = list(maptype = 'roadmap'))



data("nzquakes")
data.2 = nzquakes

system.time(iNZightPlot(Longitude,Latitude,data = data.2,colby = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap')))
ClickOnZoom(ratio = 1/2)

