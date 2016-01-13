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







data.1 = read.csv('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/Gapminder-2008.csv',skip = 1)
plot.inzightmap(obj.1,shp.name = location,type = 'shape',with.data = data.1,region = 'Country',subset.by = 'Imports',transform = 'log',display = 'heat')


names(ob$opts$plot.features)

offset = 0
fill.float <<- ifelse(is.na(orderd.data) == TRUE,1,orderd.data * (1 - offset) + (offset))


ob$opts$plot.features$offset
