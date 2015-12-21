library(grid)
library(raster)
setwd('C:/Users/jason/Documents/GitHub/iNZightMaps/data')
source('C:/Users/jason/Desktop/shade.fun.r')
system.time(shade.map('sha/TASUB2016_HD_Clipped.shp'))

library(shapefiles)
a = read.shp('au/map.shp')
data = convert.to.simple(a)
data.xy = data[,2:3]
data.plot = split(data.xy, as.factor(data[,1]))
xlim = range(data.xy[,1])
ylim = range(data.xy[,2])
plot(0,0,type = 'n', xlim =xlim,ylim = ylim)
mapply(polygon,data.plot)


polygon(data.xy)