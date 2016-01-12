library(maptools)
library(grid)
library(RColorBrewer)
location = 'C:/Users/yeamin/Desktop/QuakesNZ2000.csv'

library(iNZightMaps)
location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world/TM_WORLD_BORDERS-0.3.shp'



names(ob$opts$plot.features)
data.1 = read.csv('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/Gapminder-2008.csv',skip = 1)
obj = iNZightMap(~Region,~Country,data = data.1)
plot.inzightmap(obj,shp.name = location,type = 'shape')




ob$opts$plot.features$colby
