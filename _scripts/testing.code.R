library(maptools)
library(grid)
library(RColorBrewer)
library(iNZightPlots)
library(RgoogleMaps)
library(countrycode)
library(stringr)


data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
location = 'C:/Users/yeamin/Desktop/world/ne_110m_admin_0_countries.shp'
shp <- readShapeSpatial(location)
shape.obj <- shape.extract(shp)


data.t = data.trans(data.1$Exports,transform = 'linear')
data.o = order.match(shp[[4]],data.1$Country)
color = col.fun(data.o,
                color.index = shape.obj$col.index,
                display = 'hue',col = 'red',offset = 0)
shapeobj = color.bind(color,shape.obj)
plot.inzshapemap(shapeobj)






data.2 = read.csv('C:/Users/yeamin/Desktop/GeoNet_CMT_solutions.csv')
data.2$Latitude = data.2$Latitude + 180

system.time(iNZightPlot(Longitude,Latitude,data = data.2,
            plottype = 'map',plot.features = list(maptype = 'roadmap')))



iNZightPlot(Longitude,Latitude,data = data.2,colby =dp,g1 = str,
            plottype = 'map',plot.features = list(maptype = 'roadmap'))



data("nzquakes")
data.2 = nzquakes
