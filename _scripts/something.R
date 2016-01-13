library(maptools)
library(grid)
library(RColorBrewer)
location = 'C:/Users/yeamin/Desktop/QuakesNZ2000.csv'

library(iNZightMaps)
location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world/TM_WORLD_BORDERS-0.3.shp'
data("nzquakes")
data = nzquakes
obj.1 = iNZightMap(~Latitude,~Longitude,data = data)
plot.inzightmap(obj.1,shp.name = location,type = 'shape')


names(ob$opts$plot.features)
data.1 = read.csv('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/Gapminder-2008.csv',skip = 1)
obj = iNZightMap(~Region,~Country,data = data.1)
plot.inzightmap(obj,shp.name = location,type = 'shape')


shp = readShapePoly(location)
ex.data = shape.extract(shp)


x = data.1$Imports
x.percent = data.trans(x)

match.data = region.match(x.percent,shp[[5]],data.1$Country)
col.fun(match.data,each = ex.data$each,display = 'heat',col = 'yellow')
