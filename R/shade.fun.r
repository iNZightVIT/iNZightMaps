shade.map = function(shp,data,colby = 'Populationgrowth',method = 'linear',display = 'heat',na.fill = 'White',offset = 0 ,col)
{	
	###
	grid.newpage()
	###

	shape = readShapePoly(shp)
	bbox = shape@bbox
	xlim = bbox[1,]
	ylim = bbox[2,]
	shade.obj = shape.extract(shp = shape,colby = colby, method = method,data = data,display = display,
								na.fill = na.fill,offset = offset,col = col)
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



shape.extract = function(shp,colby,method,display,data,na.fill,offset,col)
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
	col = col.fun(shp,colby = colby, method = method,display = display,
			each = poly.rep,data = data,na.fill = na.fill,offset = offset,col = col)
	shape.obj = list(polygon = latlon, color = col)
	shape.obj
	
	
}

##col not include in the function yet
col.fun = function(shp,data,colby,method,display,each,na.fill,offset,col)
{
	##if data is missing then just fill the area randomly ie, display = 'n'
	if(missing(data))
	{		
		orderd.col.trans = each
		impossible.number = 0.091823021983
		###fore the method if data is missing
		display = 'n'
		warning('the data is missing! use the default display method')
	}
	else{
		##transform method
		a = data[,colby]
		b = a - min(a,na.rm = TRUE)
		if(method =='linear')
		{	
			b = b
		}
		if(method =='log')
		{
			b = log(b + 1)
			b = b - min(b,na.rm = TRUE)
		}
		if(method == 'sqrt')
		{
			b = sqrt(b)
		}
		if(method == 'exp')
		{
			b = exp(b)
		}
		if(method == 'power')
		{
			b = b^2
		}
		if(method == 'normal')
		{
			x <<- (b - mean(b,na.rm = TRUE))/sd(b,na.rm = TRUE)
			b = dnorm(x,0,1)
		}
		percent.data = b/diff(range(b,na.rm = TRUE))
		order = match(shp[[5]],data$Country)
		orderd.data <<- percent.data[order]
		impossible.number = 0.091823021983
	}
	
	print(offset)
	bio.color = c('bi.polar')
	###the color can not be offset if it is bio-color
	if(display %in% bio.color)
	{
		orderd.col.trans <<- ifelse(is.na(orderd.data) == TRUE, impossible.number,orderd.data)
	}else
	{
		orderd.col.trans <<- ifelse(is.na(orderd.data) == TRUE, impossible.number,orderd.data * (1 - offset) + (offset))
	}
	
	###display method
	if(display == 'hue')
	{
		fill = rgb(1,0,0,orderd.col.trans)
	}
	if(display == 'hcl')
	{	
		fill = hcl(as.numeric(orderd.col.trans)*100,l = 85)
	}
	if(display == 'ff')
	{
		ab = expand.grid(a = as.numeric(orderd.col.trans)*360,b = 100)
		Lab = cbind(L =50, ab)
		srgb = convertColor(Lab, from = "Lab", to = "sRGB")
		fill = rgb(srgb[, 1], srgb[, 2], srgb[, 3])
	}
	if(display == 'heat')
	{
		over.col = heat.colors(length(each) * 100)
		orderd.col = over.col[length(over.col):1]
		id = round(orderd.col.trans * length(each) * 100)
		fill = orderd.col[id]
	}
	if(display == 'rainbow')
	{
		over.col = rainbow(length(each) * 100)
		orderd.col = over.col[length(over.col):1]
		id = round(orderd.col.trans * length(each) * 100)
		fill = orderd.col[id]
	}
	if(display == 'terrin.colors')
	{
		over.col = terrin.colors(length(each) * 100)
		orderd.col = over.col[length(over.col):1]
		id = round(orderd.col.trans * length(each) * 100)
		fill = orderd.col[id]	
	}
	if(display == 'topo.colors')
	{
		over.col = topo.colors(length(each) * 100)
		orderd.col = over.col[length(over.col):1]
		id = round(orderd.col.trans * length(each) * 100)
		fill = orderd.col[id]			
	}
	if(display == 'cm.colors')
	{
		over.col = cm.colors(length(each) * 100)
		orderd.col = over.col[length(over.col):1]
		id = round(orderd.col.trans * length(each) * 100)
		fill = orderd.col[id]				
	}
	if(display == 'bi.polar')
	{
		col.center = mean(orderd.col.trans)
		fill = ''
		re.scale= 1 / max(abs(orderd.col.trans - col.center))
		alpha = ifelse(orderd.col.trans >= col.center,
						(orderd.col.trans- col.center) * re.scale,
						(col.center - orderd.col.trans) * re.scale
					   )
		fill = ifelse(orderd.col.trans >= col.center,
						 rgb(1,0,0,alpha = alpha),
						 rgb(0,0,1,alpha = alpha)
					  )
	}
	if(display == 'gray')
	{
		fill = gray(round(1 - as.numeric(orderd.col.trans),5))
	}
	
	###fill the color randomly notinclude the missing area
	if(display == 'r')
	{
		r = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		g = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		b = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
		fill = rgb(r,g,b)	
	}
	###fill the color randomly does include the missing area
	if(display == 'n')
	{
		r = runif(length(shp@polygons))
		g = runif(length(shp@polygons))
		b = runif(length(shp@polygons))
		na.fill = rgb(r,g,b)
		fill = rgb(r,g,b)	
	}
	
	
	color.each = ifelse(orderd.col.trans== impossible.number, na.fill, fill)
	color.out = rep(color.each,each)	
	color.out
}

function(){
e = rep(FALSE,length(m.split))
for(i in 1:length(m.split)){
	for(j in 1:length(s.split)){
		e[i] = any(c(any(m.split[[i]] %in% s.split[[j]]),e[i]))
		
	}
}
missing.name[e]
}
