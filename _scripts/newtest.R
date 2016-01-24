
ll = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder.csv')
data.2 = data.1[data.1$Country %in% c('Algeria','Angola','Burkina Faso','Cameroon',
                                      'Chad','Egypt','Libya','Mauritania','Niger',
                                      'Nigeria','Sudan'),]


data.3 = data.1[data.1$Country %in% c('India','Japan','New Zealand'),]
obj <- iNZightShapeMap(ll, data.region = 'Country', data = data.2)


plot(obj, variable = ~CO2Emissions,
     g1 = Year,
     region = ~Country,
     data = data.2,
     col.fun = 'heat',
     transform = 'linear',
     offset = 0.2,
     full.map =F)

iNZightPlot(CO2Emissions, Country, data = data.2, plottype = 'shapemap',g1 = Year,
            plot.features =list(
              shape.object = obj,
              transform = "linear", 
              col.offset = 0.2, 
              col = "blue",
              col.method = 'heat',
              na.fill = 'White',
              full.map = T)
)

