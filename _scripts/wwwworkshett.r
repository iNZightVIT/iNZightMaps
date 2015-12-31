library(grid)
library(maptools)
setwd('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/world')
data = read.csv('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/Gapminder-2008.csv',skip = 1)
source('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/R/shade.fun.r')
system.time(shade.map('TM_WORLD_BORDERS-0.3.shp',colby = 'Populationgrowth',data = data))



shp.name = 'TM_WORLD_BORDERS-0.3.shp'
shade.map(shp.name,colby = 'Populationgrowth',data = data, method = 'n')


