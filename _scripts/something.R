library(maptools)
library(grid)
library(RColorBrewer)
location = 'C:/Users/yeamin/Desktop/QuakesNZ2000.csv'

library(iNZightMaps)
location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world/TM_WORLD_BORDERS-0.3.shp'
shade.map(location,region = 'Country',colby = 'Exports',
          transform = 'linear', display = 'heat',offset = 0,na.fill = 'gray',col = 'red')

data("nzquakes")
data = nzquakes
obj = iNZightMap(~Latitude,~Longitude,data = data)
plot.inzightmap(obj,plot.shp = TRUE,shp.name = location)
