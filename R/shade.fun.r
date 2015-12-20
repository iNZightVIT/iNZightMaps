shade.map = function(shp)
{	
	shape <<- shapefile(shp)
	bbox = shape@bbox
	xlim = bbox[1,]
	ylim = bbox[2,]
	shade.obj <<- shape.extract(shape)
	vp = viewport(0.5,0.5,name = 'VP:PLOTlayout',xscale = xlim, yscale = ylim)
	pushViewport(vp)
	invisible(mapply(gird.map.polygon,shade.obj$data,shade.obj$col.out)
			)
}


gird.map.polygon = function(data,col)
{	
	data.1 = data
	grid.polygon(data.1[,1],data.1[,2],default.units = "native",
						gp = 
							gpar(col = 'black',
							fill  = col))
}


shape.extract = function(shape)
{
	data = list()
	j = 0
	col = 0
	poly.rep = 0
	poly.out = length(shape@polygons)
	for(i in 1:poly.out)
	{
		poly.in = length(shape@polygons[[i]]@Polygons)
		poly.rep[i] = poly.in
		for(ii in 1:poly.in)
		{
			j = j + 1
			data[j] = list(shape@polygons[[i]]@Polygons[[ii]]@coords)
		}
		
	}
	print(paste('num of polygon:', j))
	h = runif(poly.out)
	s = runif(poly.out)
	v = runif(poly.out)
	col.fun = hsv(h,s,v)
	col.out = rep(col.fun,poly.rep)
	col.out
	list(data = data, col.out = col.out)

	
}


