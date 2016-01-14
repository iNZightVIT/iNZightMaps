library(maptools)
library(grid)
library(RColorBrewer)
library(iNZightPlots)
library(iNZightMaps)
library(grid)

data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world/TM_WORLD_BORDERS-0.3.shp'

shp <- readShapePoly(location)
shapeobj <- shape.extract(shp)

data.t = data.trans(data.1$Exports,transform = 'linear')
data.o = order.match(data.t,shp[[5]],data.1$Country)
color = col.fun(data.o,each = shapeobj$each,display = 'hue')
shape.obj = shape.object(color,shapeobj$latlon)

system.time(iNZightPlot(Exports, Country, data = data.1, plottype = 'map',
            plot.features =
              list(maptype = "shape", shape.obj = shape.obj)))


