
ll = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world110m.rds'
data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder.csv')
data.2 = data.1[data.1$Country %in% c('Algeria','Angola','Burkina Faso','Cameroon',
                                      'Chad','Egypt','Libya','Mauritania','Niger',
                                      'Nigeria','Sudan'),]


data.3 = data.1[data.1$Country %in% c('Australia','New Zealand'),]
data.4 = data.1[data.1$Country %in% c('Albania', 'Andorra', 'Austria', 'Belarus', 'Belgium', 
                                      'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 
                                      'Cyprus','Czech Republic', 
                                      'Estonia', 'Finland'),]

dataIn = data.3
obj <- iNZightShapeMap(ll, data.region = 'Country', data = dataIn)
system.time(plot(obj, variable = ~CO2Emissions,
                 region = ~Country,
                 data = dataIn,
                 col.fun = 'heat',
                 transform = 'log',
                 na.fill = 'white',
                 offset = 0.2,
                 full.map = F,
                 extend.ratio =1))



obj <- iNZightShapeMap(ll, data.region = 'Country', data = data.2)
iNZightPlot(CO2Emissions, Country, data = data.2, plottype = 'shapemap',g1 = Leap.Year,
            plot.features =list(
              shape.object = obj,
              transform = "linear", 
              col.offset = 0, 
              col = "blue",
              col.method = 'hue',
              na.fill = 'White',
              full.map = F,
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
              full.map = F,
              extend.ratio = 2)
)

