
## packages 
library(maptools)
library(grid)
library(RColorBrewer)
library(iNZightPlots)
library(RgoogleMaps)
library(countrycode)
library(stringr)
library(iNZightMaps)

## shape file(rds)
l1 = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
l2 = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/rds/AuCity.rds'
l3 = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/rds/Africa.rds'
l4 = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/rds/SouthAmerica.rds'
l6 = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/NewZealand.rds'

## data set
data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
data.2 = data.1[data.1$Country %in% c('Algeria','Angola','Burkina Faso','Cameroon',
                                      'Chad','Egypt','Libya','Mauritania','Niger',
                                      'Nigeria','Sudan'),]


data.3 = data.1[data.1$Country %in% c('New Zealand','Australia','Japan'),]
data.4 = data.1[data.1$Country %in% c('Albania', 'Andorra', 'Austria', 'Belarus', 'Belgium', 
                                      'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 
                                      'Cyprus','Czech Republic'),]
