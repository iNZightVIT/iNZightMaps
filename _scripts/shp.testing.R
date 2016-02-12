dataIn = data.1
obj = iNZightShapeMap(l1, data.region = 'Country', data = dataIn)
var = c('BodyMassIndex_F','Cellphones','CO2Emissions',
        'EnergyUsePerPerson','Imports','Inflation','Populationtotal')
bar.obj = bar.coor(obj = obj,var = var, data = dataIn, xmax = 1, ymax = 3)
system.time(plot(obj, variable = ~CO2Emissions,
                 col.fun = 'hue',
                 col = 'red',
                 transform = 'linear',
                 na.fill = '#C0C0C0',
                 col.offset = 0,
                 full.map = T,
                 extend.ratio = 1,
                 name = 'b'))
sClickOnZoom(ratio = 0.1)
srezoom(zoom = 0.8)


data("nzquakes")
system.time(iNZightPlot(Longitude,Latitude,data = nzquakes,colby = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap')))
ClickOnZoom(0.1)
rezoom(zoom =0.9)
