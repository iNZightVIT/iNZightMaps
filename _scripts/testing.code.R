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


data.3 = data.1[data.1$Country %in% c('New Zealand','Australia'),]
data.4 = data.1[data.1$Country %in% c('Albania', 'Andorra', 'Austria', 'Belarus', 'Belgium', 
                                      'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 
                                      'Cyprus','Czech Republic', 
                                      'Estonia', 'Finland'),]
dataIn = data.1
obj <- iNZightShapeMap(ll, data.region = 'Country', data = dataIn)
system.time(plot(obj, variable = ~ChildrenPerWoman,
                 region = ~Country,
                 data = dataIn,
                 col.fun = 'hue',
                 transform = 'log',
                 na.fill = '#7A853D',
                 col.offset = 0,
                 full.map = T,
                 extend.ratio = 2,
                 col = 'purple',
                 name =T))