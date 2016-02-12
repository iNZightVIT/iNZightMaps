data("nzquakes")
system.time(iNZightPlot(Longitude,Latitude,data = nzquakes,colby = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap')))
ClickOnZoom(0.1)
rezoom(zoom =0.9)



data("nzquakes")
iNZightPlot(Longitude,Latitude,data = nzquakes,g1 = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap'))
