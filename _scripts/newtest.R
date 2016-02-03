library(maptools)
library(grid)
library(RColorBrewer)
library(iNZightPlots)
library(RgoogleMaps)
library(countrycode)
library(stringr)
library(iNZightMaps)


ll = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
data.2 = data.1[data.1$Country %in% c('Algeria','Angola','Burkina Faso','Cameroon',
                                      'Chad','Egypt','Libya','Mauritania','Niger',
                                      'Nigeria','Sudan'),]


data.3 = data.1[data.1$Country %in% c('New Zealand','Australia','Japan'),]
data.4 = data.1[data.1$Country %in% c('Albania', 'Andorra', 'Austria', 'Belarus', 'Belgium', 
                                      'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 
                                      'Cyprus','Czech Republic', 
                                      'Estonia', 'Finland'),]
dataIn = data.4
obj <- iNZightShapeMap(ll, data.region = 'Country', data = dataIn)
var = c('BodyMassIndex_F','ChildrenPerWoman','Populationtotal','Populationdensity')
bar.obj = bar.coor(var = var, data = dataIn, x = x, y = y, xmax = xmax, ymax = ymax)
system.time(plot(obj, variable = ~Imports,
                 region = ~Country,
                 data = dataIn,
                 col.fun = 'e',
                 col = 'red',
                 transform = 'linear',
                 na.fill = '#C0C0C0',
                 col.offset = 0,
                 full.map = F,
                 extend.ratio = 1,
                 name = 'bar'))



obj <- iNZightShapeMap(ll, data.region = 'Country', data = data.4)
iNZightPlot(CO2Emissions, Country, data = data.4, plottype = 'shapemap',#g1 = Leap.Year,
            plot.features =list(
              shape.object = obj,
              transform = "power", 
              col.offset = 0, 
              col = "blue",
              col.method = 'hue',
              na.fill = 'White',
              full.map = T,
              extend.ratio = 1,
              name = TRUE
            )
)

obj <- iNZightShapeMap(ll, data.region = 'Country', data = data.3)
iNZightPlot(ChildrenPerWoman, Country, data = data.3, plottype = 'shapemap',g1 = Leap.Year,
            plot.features =list(
              shape.object = obj,
              transform = "linear", 
              col.offset =0.2, 
              col = "blue",
              col.method = 'rainbow',
              na.fill = 'White',
              full.map = T,
              extend.ratio = 2)
)




##au
ll = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/New Zealand.rds'
data.1 = read.csv('C:/Users/yeamin/Desktop/nz.csv')
obj <- iNZightShapeMap(ll, data.region = 'area', data = data.1)
plot(obj, variable = ~population,g1 = year,
                 region = ~area,
                 data = data.1,
                 col.fun = 'hue',
                 transform = 'power',
                 na.fill = 'white',
                 col = 'blue',
                 offset = 0,
                 full.map =F,
                 extend.ratio = 1)

iNZightPlot(population, area, data = data.1, plottype = 'shapemap',g1 = year,
            plot.features =list(
              shape.object = obj,
              transform = "linear", 
              col.offset =0.2, 
              col = "blue",
              col.method = 'r',
              na.fill = 'White',
              full.map = F,
              extend.ratio = 3)
)

