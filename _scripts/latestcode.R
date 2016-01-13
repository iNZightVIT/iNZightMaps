## source package
## data = Gapminder data
## location
location <- ""


## mess with the output of this function so it gives a dataframe: [x | y | id | country]
shapeobj <- shape.extract(shp <- readShapePoly(location),
                          "ChildrenPerWoman", "linear", "hue", data = data, na.fill = "pink",
                                               offset = 0,col = "green4", region = "Country")
                          
myshape <- data.frame(x = shapeobj$polygon[, 1],
                      y = shapeobj$polygon[, 2],
                      id = as.numeric(rownames(shapeobj$polygon)))



iNZightPlot(ChildrenPerWoman, Country, data = data, plottype = "map",
            plot.features =
                list(maptype = "shape", shape.obj = myshape, temp.col = shapeobj$color))


## once shape.extract modified, soemthing like:
iNZightPlot(ChildrenPerWoman, Country, data = data, plottype = "map",
            plot.features =
                list(maptype = "shape", shape.obj = shapeobj, temp.col = COLOURS))


### COLOURS:
## the necessary colours (for each ID)











### then ... 
## colour function modified to take values in [0,1] -> hex colour

