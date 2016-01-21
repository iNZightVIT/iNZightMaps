
data = read.csv('C:/Users/yeamin/Desktop/testingdata.csv')
shp.name = 'C:/Users/yeamin/Desktop/world/ne_110m_admin_0_countries.shp'
shp = readShapeSpatial(shp.name)
shp.obj = shape.extract(shp,column.index = 4)



grid.newpage()
data.t = data.trans(data.1$CO2Emissions,transform = 'normal')
data.o = order.match(shp[[4]],data.1$Country)
color = col.fun(data.t[data.o],
                color.index = shape.obj$col.index,
                display = 'hue',col = 'red',offset = 0)

ind = (shp.obj$region %in% data.1$Country )
index.1 = rep(ind,shp.obj$col.index)
a = rep(index.1,shp.obj$each)
latlon.nz = shp.obj$latlon[a,]
each.nz = shp.obj$each[index.1]
colindex.nz = shp.obj$col.index[ind]
xlim = range(latlon.nz[,1])
ylim = range(latlon.nz[,2])
colors = color[index.1]
wh = win.ratio()

vp = viewport(0.5,0.5,width = wh[1], height = wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim)
pushViewport(vp)
grid.polygon(latlon.nz[,1], latlon.nz[,2], default.units = "native", id.length = each.nz,
             gp = gpar(col = 'black',fill = colors))

