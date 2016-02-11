
data.1 = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
data.2 = data.1[data.1$Country %in% c('Algeria','Angola','Burkina Faso','Cameroon',
                                      'Chad','Egypt','Libya','Mauritania','Niger',
                                      'Nigeria','Sudan'),]


data.3 = data.1[data.1$Country %in% c('New Zealand','Australia','Japan'),]
data.4 = data.1[data.1$Country %in% c('Albania', 'Andorra', 'Austria', 'Belarus', 'Belgium', 
                                      'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 
                                      'Cyprus','Czech Republic'),]




data("nzquakes")
system.time(iNZightPlot(Longitude,Latitude,data = nzquakes,colby = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap')))
ClickOnZoom(0.1)
rezoom(zoom =0.9)



data("nzquakes")
iNZightPlot(Longitude,Latitude,data = nzquakes,g1 = Depth,
                        plottype = 'map',plot.features = list(maptype = 'roadmap'))
