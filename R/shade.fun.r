shape.extract = function(shp,colby,transform,display,data,na.fill,offset,col,region)
{
    polygon.data = list()
    j = 0
    color = 0
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
    color = col.fun(shp,colby = colby, transform = transform,display = display,
            each = poly.rep,data = data,na.fill = na.fill,offset = offset,col = col,region = region)
    shape.obj = list(polygon = latlon, color = color)
    shape.obj
	polygon.data = list()
	j = 0
	color = 0
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
	color = col.fun(shp,colby = colby, transform = transform,display = display,
			each = poly.rep,data = data,na.fill = na.fill,offset = offset,col = col,region = region)
	shape.obj = list(polygon = latlon, color = color)
	shape.obj


}

col.fun = function(shp,data,colby,transform,display,each,na.fill,offset,col,region)
{
    ##if data is missing then just fill the area randomly ie, display = 'n'
    if(missing(data) || is.null(data))
    {		
        fill.float = each
        impossible.number = 0.091823021983
        ###fore the transform if data is missing
        display = 'n'
        #warning('the data is missing! use the default display transform')
    }
    else{
        ##transform transform
        #orderd.data <<- percent.data[order]

        a = data[,colby]
        b = a - min(a,na.rm = TRUE)
        if(transform =='linear')
        {	
            b = b
        }
        if(transform =='log')
        {
            b = log(b + 1)
        }
        if(transform == 'sqrt')
        {
            b = sqrt(b)
        }
        if(transform == 'exp')
        {
            b = exp(b)
        }
        if(transform == 'power')
        {
            b = b^2
        }
        if(transform == 'normal')
        {
            x = (a - mean(a,na.rm = TRUE))/sd(a,na.rm = TRUE)
            b = dnorm(x,0,1)
            b = b - min(b,na.rm = TRUE)
        }
        if(transform == 'unknow~')
        {
            
        }
        percent.data = b/diff(range(b,na.rm = TRUE))
        order = match(shp[[5]],data[,region])
        orderd.data <<- percent.data[order]
        impossible.number = 0.091823021983
        
        bio.color = c('bi.polar','cm.colors	')

        if(display %in% bio.color)
        {
            fill.float <<- ifelse(is.na(orderd.data) == TRUE, impossible.number,orderd.data)
        }else
        {
            fill.float <<- ifelse(is.na(orderd.data) == TRUE, impossible.number,orderd.data * (1 - offset) + (offset))
        }
    }
    ###the color can not be offset if it is bio-color


    ###display transform
    if(display == 'hue')
    {
        char.col.trans = col2rgb(col)/255
        fill.col =rgb(char.col.trans[1],char.col.trans[2],char.col.trans[3],fill.float)
    }
    if(display == 'hcl')
    {	
        fill.col = hcl(as.numeric(fill.float)*100,l = 85)
    }
    if(display == 'ff')
    {
        ab = expand.grid(a = as.numeric(fill.float)*360,b = 100)
        Lab = cbind(L =50, ab)
        srgb = convertColor(Lab, from = "Lab", to = "sRGB")
        fill.col = rgb(srgb[, 1], srgb[, 2], srgb[, 3])
    }
    if(display == 'heat')
    {
        over.col = heat.colors(length(each) * 100)
        orderd.col = over.col[length(over.col):1]
        id = round(fill.float * length(each) * 100)
        fill.col = orderd.col[id]
    }
    if(display == 'rainbow')
    {
        over.col = rainbow(length(each) * 100)
        orderd.col = over.col
        id = round(fill.float * length(each) * 100)
        fill.col = orderd.col[id]
    }
    if(display == 'terrain.colors')
    {
        over.col = terrain.colors(length(each) * 100)
        orderd.col = over.col
        id = round(fill.float * length(each) * 100)
        fill.col = orderd.col[id]	
    }
    if(display == 'topo.colors')
    {
        over.col = topo.colors(length(each) * 100)
        orderd.col = over.col
        id = round(fill.float * length(each) * 100)
        fill.col = orderd.col[id]
    }
    if(display == 'cm.colors')
    {
        over.col = cm.colors(length(each) * 100)
        orderd.col = over.col
        id = round(fill.float * length(each) * 100)
        fill.col = orderd.col[id]				
    }
    if(display == 'bi.polar')
    {
        col.center = mean(fill.float)
        fill = ''
        re.scale= 1 / max(abs(fill.float - col.center))
        alpha = ifelse(fill.float >= col.center,
                        (fill.float- col.center) * re.scale,
                        (col.center - fill.float) * re.scale
                       )
        fill.col = ifelse(fill.float >= col.center,
                         rgb(1,0,0,alpha = alpha),
                         rgb(0,0,1,alpha = alpha)
                      )
    }
    if(display == 'gray')
    {
        fill.col = gray(round(1 - as.numeric(fill.float),5))
    }

    ###fill the color randomly notinclude the missing area
    if(display == 'r')
    {
        r = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
        g = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
        b = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
        fill.col = rgb(r,g,b)	
    }
    ###fill the color randomly does include the missing area
    if(display == 'n')
    {
        r = runif(length(shp@polygons))
        g = runif(length(shp@polygons))
        b = runif(length(shp@polygons))
        na.fill = rgb(r,g,b)
        fill.col = rgb(r,g,b)	
    }


    color.each = ifelse(fill.float== impossible.number, na.fill, fill.col)
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
