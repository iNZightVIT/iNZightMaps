
ll = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/shpdata/world.rds'
file_ext(ll)
obj <- iNZightShapeMap(ll, region = 'Country', data = data.1)


plot(obj, variable = ~ChildrenPerWoman, region = ~Country,data = data.1)




iNZightPlot(EnergyUsePerPerson, Country, data = data.1, plottype = 'shapemap',
            plot.features =list(
              shape.object = obj,
              transform = "linear", 
              col.offset = 0.2, 
              col = "red",
              col.method = 'bi.polar',
              na.fill = 'gray')
            )

aa = 'woifjewiojfioewjfoiewjfiowejfewiof.csv'
grepl('.CSV',aa)
