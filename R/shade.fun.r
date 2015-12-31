shade.map = function(shp,data,colby = 'Populationgrowth',method = 'l',na.fill = 'White')
{	
	shape = readShapePoly(shp)
	bbox = shape@bbox
	xlim = bbox[1,]
	ylim = bbox[2,]
	shade.obj = shape.extract(shp = shape,colby = colby, method = method,data = data,na.fill = na.fill)
	shade.data = shade.obj$polygon
	shade.id = rownames(shade.data)
	col = shade.obj$color
	vp = viewport(0.5,0.5,name = 'VP:PLOTlayout',xscale = xlim, yscale = ylim)
	pushViewport(vp)
	grid.polygon(shade.data[,1],shade.data[,2],default.units = "native", id = shade.id,
						gp = 
							gpar(col = 'black',
							fill  = col))
}



shape.extract = function(shp,colby,method,data,na.fill)
{
	polygon.data = list()
	j = 0
	col = 0
	poly.rep = 0
	poly.out = length(shp@polygons)
	index = 0
	for(i in 1:poly.out)
	{
		poly.in = length(shp@polygons[[i]]@Polygons)
		poly.rep[i] = poly.in
		for(ii in 1:poly.in)
		{
			j = j + 1
			polygon.data[j] = list(shp@polygons[[i]]@Polygons[[ii]]@coords)
			index[j] = dim(polygon.data[[j]])[1]
		}
		
	}
	print(paste('num of polygon:', j))
	poly.index = rep(1:j,index)
	latlon = do.call(rbind,polygon.data)
	rownames(latlon) = poly.index
	col = col.fun(shp,colby = colby, method = method, each = poly.rep,data = data,na.fill = na.fill)
	shape.obj = list(polygon = latlon, color = col)
	shape.obj
	
	
}


col.fun = function(shp,data,colby, method ,each,na.fill)
{
	a = data[,colby]
	if(method =='l')
	{	
		b = a - min(a,na.rm = TRUE)
		percent.data = b/diff(range(b,na.rm = TRUE))
		order = match(shp[[5]],data$Country)
		orderd.data = percent.data[order]
		offset = 0.5
		impossible.number = '0.234959823475923487509234875'
		orderd.data.1 = ifelse(is.na(orderd.data) == TRUE, impossible.number,orderd.data * 0.5 + 0.5)
		color.each = ifelse(orderd.data.1 == impossible.number, na.fill, rgb(orderd.data.1,0.5,orderd.data.1))
		color.out = rep(color.each,each)
	}
	if(method == 'r')
	{
		order = match(shp[[5]],data$Country)
		impossible.number = '0.234959823475923487509234875'
		r = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		g = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		b = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		color.each = ifelse(r == impossible.number, na.fill, rgb(r,g,b))
		color.out = rep(color.each,each)		
	}
	if(method == 'n')
	{
		r = runif(length(shp@polygons))
		g = runif(length(shp@polygons))
		b = runif(length(shp@polygons))
		color.each = rgb(r,g,b)
		color.out = rep(color.each,each)		
	}
	if(method == '')
	color.out
}

