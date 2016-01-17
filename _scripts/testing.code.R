library(maptools)
library(grid)
library(RColorBrewer)
library(iNZightPlots)
library(iNZightMaps)

data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
location = 'C:/Users/yeamin/Desktop/world/ne_110m_admin_0_countries.shp'
data("nzquakes")
shp <- readShapeSpatial(location)
shapeobj <- shape.extract(shp)

data.t = data.trans(data.1$Exports,transform = 'linear')
data.o = order.match(data.t,shp[[4]],data.1$Country)
color = col.fun(data.o,color.index = shapeobj$col.index,display = 'hue',col = 'blue',offset = 0.2)
shape.obj = color.bind(color,shapeobj)


system.time(iNZightPlot(Exports, Country, data = data.1, plottype = 'map',
            plot.features =
              list(maptype = "shape", shape.obj = shape.obj)))


obj = iNZightMap(~Latitude,~Longitude,data = nzquakes)
plot.inzightmap(obj,opacity = 'Day',type = 'roadmap')

data.2 = read.csv('C:/Users/yeamin/Desktop/GeoNet_CMT_solutions.csv')

iNZightPlot(Longitude,Latitude,data = data.2,colby = rake,
                        plottype = 'map',plot.features = list(maptype = 'roadmap'))



data.2 = read.csv('C:/Users/yeamin/Desktop/GeoNet_CMT_solutions.csv')
data("nzquakes")
data.2 = nzquakes



fakedata <- data.2
fakedata$Longitude= -fakedata$Longitude  # i.e., opposite sign

iNZightPlot(Longitude,Latitude,data = fakedata,
            plottype = 'map',plot.features = list(maptype = 'roadmap'))

range(lon.rescale(data.2$Longitude),na.rm = TRUE)

lat.range = c(-50.51416 -32.05253)
lon.range=c(197.9872,-197.9872+360)
size.1 = c(640,640)
MaxZoom(lat.range, lon.range, size.1)

162.0304 197.9872