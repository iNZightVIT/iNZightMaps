library(grid)
library(maptools)
library(RColorBrewer)
setwd('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/world')
data = read.csv('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/Gapminder-2008.csv',skip = 1)
source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')
system.time(shade.map('TM_WORLD_BORDERS-0.3.shp',colby = 'Populationgrowth',data = data,col = 'Yellow'))



shp.name = 'TM_WORLD_BORDERS-0.3.shp'
source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')
system.time(shade.map(shp.name,colby = 'IncomePerPerson',data = data, method = 'heat',na.fill = 'White',offset = 0))

source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')
shp.name = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/au/map.shp'
system.time(shade.map(shp.name,colby = 'IncomePerPerson', method = 'n',na.fill = 'White',offset =0.5))


shp.name = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/Africa/map.shp'
system.time(shade.map(shp.name,colby = 'IncomePerPerson', method = 'n',na.fill = 'White',offset =0.5))