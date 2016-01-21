
ll = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/shpdata/world.rds'
data.2 = data.1[data.1$Country %in% c('China','Japan'),]
obj <- iNZightShapeMap(ll, region = 'Country', data = data.2)


plot(obj, variable = ~CO2Emissions,
     region = ~Country,
     data = data.2,
     col.method = 'blue',
     transform = 'log',
     full.map = FALSE)



data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)

data.1 = data.1[data.1$Country %in% c('China','Japan'),]
obj <- iNZightShapeMap(ll, region = 'Country', data = data.1)
iNZightPlot(ChildrenPerWoman, Country, data = data.1, plottype = 'shapemap',
            plot.features =list(
              shape.object = obj,
              transform = "linear", 
              col.offset = 0.2, 
              col = "red",
              col.method = 'hue',
              na.fill = 'gray')
            )

iNZightPlot(CO2Emissions, Country, data = data.1, plottype = 'shapemap',
            plot.features =list(
              shape.object = obj,
              transform = "normal", 
              col.offset = 0, 
              col = "red",
              col.method = 'hue',
              na.fill = 'gray')
)
