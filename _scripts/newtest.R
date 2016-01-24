
ll = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
data.2 = data.1[data.1$Country %in% c('Algeria','Angola','Burkina Faso','Cameroon',
                                      'Chad','Egypt','Libya','Mauritania','Niger',
                                      'Nigeria','Sudan'),]


data.3 = data.1[data.1$Country %in% c('New Zealand','China'),]
obj <- iNZightShapeMap(ll, data.region = 'Country', data = data.3)


plot(obj, variable = ~CO2Emissions,
     region = ~Country,
     data = data.3,
     col.method = 'heat',
     transform = 'linear',
     full.map =F)

ee = obj$each
length(rep(1:length(ee),ee))

data.1 = read.csv('C:/Users/yeamin/Desktop/bank.csv',skip = 4)
s.set = data.1$Country.Name %in% c('China','Japan')
ss.set = data.1$Year %in% c(1996,2000,2004,2008)

data.2 = data.1[s.set,]
data.3 = data.1[ss.set & s.set,]
obj <- iNZightShapeMap(ll, data.region = 'Country.Name', data = data.1)
iNZightPlot(X1960, Country.Name, data = data.1, plottype = 'shapemap',
            plot.features =list(
              shape.object = obj,
              transform = "log", 
              col.offset = 0, 
              col = "blue",
              col.method = 'heat',
              na.fill = 'gray',
              full.map = F)
            )
data.3$Imports



obj <- iNZightShapeMap(ll, data.region = 'Country', data = data.2)
iNZightPlot(CO2Emissions, Country, data = data.2, plottype = 'shapemap',g1 = Year,
            plot.features =list(
              shape.object = obj,
              transform = "normal", 
              col.offset = 0, 
              col = "red",
              col.method = 'hue',
              na.fill = 'gray',
              full.map = F)
)


obj$col.index[136]
