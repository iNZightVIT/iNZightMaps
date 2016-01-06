shade.map = function(shp,data,colby = 'Populationgrowth',method = 'l',na.fill = 'White',offset = 0.5,col)
{	
	###
	grid.newpage()
	###

	shape = readShapePoly(shp)
	bbox = shape@bbox
	xlim = bbox[1,]
	ylim = bbox[2,]
	shade.obj = shape.extract(shp = shape,colby = colby, method = method,data = data,na.fill = na.fill,offset = offset,col = col)
	shade.data = shade.obj$polygon
	shade.id = rownames(shade.data)
	col = shade.obj$color
	ratio = diff(xlim)/diff(ylim)
	win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
	win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
	#ratio = ifelse(win.width > win.height, )
	vp = viewport(0.5,0.5,name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim,
		width = unit(1,'snpc'),height = unit(1/ratio,'snpc'))
	pushViewport(vp)
	grid.polygon(shade.data[,1],shade.data[,2],default.units = "native", id = shade.id,
						gp = 
							gpar(col = 'black',
							fill  = col))
}



shape.extract = function(shp,colby,method,data,na.fill,offset,col)
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
	col = col.fun(shp,colby = colby, method = method, each = poly.rep,data = data,na.fill = na.fill,offset = offset,col = col)
	shape.obj = list(polygon = latlon, color = col)
	shape.obj
	
	
}


col.fun = function(shp,data,colby,method,each,na.fill,offset,col)
{
	if(!missing(data))
	{
		a = data[,colby]
		b = a - min(a,na.rm = TRUE)
		percent.data = b/diff(range(b,na.rm = TRUE))
		order = match(shp[[5]],data$Country)
		orderd.data = percent.data[order]
		impossible.number = '0.234959823475923487509234875'
		orderd.col.trans = ifelse(is.na(orderd.data) == TRUE, impossible.number,orderd.data * (1 - offset) + (offset))
	}else{
		orderd.col.trans = each
		impossible.number = '0.234959823475923487509234875'
		###fore the method if data is missing
		method = 'n'
	}
	##keep adding method for choose color
	if(method =='l')
	{	
		fill = rgb(1,0,0,orderd.col.trans)
	}
	if(method == 'r')
	{
		r = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		g = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		b = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		fill = rgb(r,g,b)	
	}
	if(method == 'n')
	{
		r = runif(length(shp@polygons))
		g = runif(length(shp@polygons))
		b = runif(length(shp@polygons))
		na.fill = rgb(r,g,b)
		fill = rgb(r,g,b)	
	}
	if(method == 'hue')
	{	
		fill = hcl(0,c = as.numeric(orderd.col.trans)*100, l = 60)
	}
	if(method == 'ff')
	{
		ab = expand.grid(a = as.numeric(orderd.col.trans)*360,b = 100)
		Lab = cbind(L =50, ab)
		srgb = convertColor(Lab, from = "Lab", to = "sRGB")
		fill = rgb(srgb[, 1], srgb[, 2], srgb[, 3])
	}
	if(method == 'heat')
	{
		a = round(as.numeric(orderd.col.trans) * length(each))
		fill = heat.colors(length(each))[-a]
	}
	if(method == 'rainbow')
	{
		a = round(as.numeric(orderd.col.trans) * length(each))
		fill = rainbow(length(each))[-a]
	}
	if(method == 'terrain.colors')
	{
		a = round(as.numeric(orderd.col.trans) * length(each))
		fill = terrain.colors(length(each))[-a]		
	}
	if(method == 'topo.colors')
	{
		a = round(as.numeric(orderd.col.trans) * length(each))
		fill = topo.colors(length(each))[-a]			
	}
	if(method == 'cm.colors')
	{
		a = round(as.numeric(orderd.col.trans) * length(each))
		fill = cm.colors(length(each))[-a]				
	}
	
	color.each = ifelse(orderd.col.trans== impossible.number, na.fill, fill)
	color.out = rep(color.each,each)	
	color.out
}

