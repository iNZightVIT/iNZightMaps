## source package
## data = Gapminder data
## location
location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world/TM_WORLD_BORDERS-0.3.shp'
data = data.1

## mess with the output of this function so it gives a dataframe: [x | y | id | country]
shapeobj <- shape.extract(shp <- readShapePoly(location))
                          
                          
myshape <- data.frame(x = shapeobj$latlon[, 1],
                      y = shapeobj$latlon[, 2],
                      id = as.numeric(rownames(shapeobj$latlon)))
color = 'yellow'


iNZightPlot(ChildrenPerWoman, Country, data = data, plottype = "map",
            plot.features =
                list(maptype = "shape", shape.obj = myshape, temp.col = color))


## once shape.extract modified, soemthing like:
iNZightPlot(ChildrenPerWoman, Country, data = data, plottype = "map",
            plot.features =
                list(maptype = "shape", shape.obj = shapeobj, temp.col = COLOURS))


### COLOURS:
## the necessary colours (for each ID)











### then ... 
## colour function modified to take values in [0,1] -> hex colour

