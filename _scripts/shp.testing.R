dataIn = data.1
obj = iNZightShapeMap(l1, data.region = 'Country', data = dataIn,variable = ~ChildrenPerWoman)
var = c('BodyMassIndex_F','Cellphones','CO2Emissions',
        'EnergyUsePerPerson','Imports','Inflation','Populationtotal')
bar.obj = bar.coor(obj = obj,var = var, data = dataIn, xmax = 1, ymax = 3)
system.time(plot(obj, variable = ~ChildrenPerWoman,
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

obj = iNZightShapeMap(l1, data.region = 'Country', data = dataIn,variable = ~ChildrenPerWoman)
iNZightPlot(ChildrenPerWoman, Country, data = dataIn, plottype = 'shapemap',g1 = Region,
            plot.features =list(
              shape.object = obj,
              transform = "normal", 
              col.offset = 0, 
              col = "blue",
              col.method = 'hue',
              na.fill = 'gray',
              full.map = F,
              extend.ratio = 1,
              name = 'v'
            )
)




obj = iNZightShapeMap(l1, data.region = 'Country', data = dataIn,variable = ~ChildrenPerWoman)


