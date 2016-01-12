###loading
library(grid)
library(maptools)
library(RColorBrewer)
setwd('C:/Users/yeamin/Documents/GitHub/iNZightMaps/data')
source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')

###data switch
##data.1

##data.2
shp.name = 'world/TM_WORLD_BORDERS-0.3.shp'
ss = readShapePoly(shp.name)
data = ss@data



######################################data use within shp file################################################
###a quick check for accuracy
shade.map('world/TM_WORLD_BORDERS-0.3.shp',data = data,region = 'NAME',colby = 'AREA',
		transform = 'linear', display = 'hue',offset = 0,na.fill = 'gray')
shade.map('world/TM_WORLD_BORDERS-0.3.shp',data = data,region = 'NAME',colby = 'LON',
		transform = 'normal', display = 'hue',offset = 0,na.fill = 'gray')



###time
system.time(
shade.map('world/TM_WORLD_BORDERS-0.3.shp',data = data,region = 'NAME',colby = 'LON',
		transform = 'normal', display = 'hue',offset = 0,na.fill = 'gray')
)

Rprof()
shade.map('world/TM_WORLD_BORDERS-0.3.shp',data = data,region = 'NAME',colby = 'LON',
		transform = 'normal', display = 'hue',offset = 0,na.fill = 'gray')

Rprof(NULL)

prof <- summaryRprof()
prof$by.self
######################################data use within shp file################################################



###an other data set
data.1 = read.csv('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/Gapminder-2008.csv',skip = 1)


source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')
shade.map('world/TM_WORLD_BORDERS-0.3.shp',data = data.1,region = 'Country',colby = 'EnergyUsePerPerson',
		transform = 'linear', display = 'cm.colors',offset = 0,na.fill = 'gray',col = 'green')

shade.map('world/TM_WORLD_BORDERS-0.3.shp',data = data.1,region = 'Country',colby = 'Imports',
		transform = 'normal', display = 'hue',offset = 0,na.fill = 'gray',col = 'red')

shade.map('world/TM_WORLD_BORDERS-0.3.shp',data = data.1,region = 'Country',colby = 'Exports',
		transform = 'linear', display = 'heat',offset = 0,na.fill = 'gray',col = 'red')



###
shade.map('NZ/map.shp',region = 'Country',colby = 'Exports',
		transform = 'linear', display = 'heat',offset = 0,na.fill = 'gray',col = 'red')
shade.map('AU/map.shp',region = 'Country',colby = 'Exports',
		transform = 'linear', display = 'heat',offset = 0,na.fill = 'gray',col = 'red')


