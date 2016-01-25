##' extract the information from a SpatialPolygonsDataFrame object and 
##'
##' the function will also returns a global object which called global.objects
##' @title extract and create a shape object
##' @param shp a SpatialPolygonsDataFrame object, see \link{readShapeSpatial}
##' @return a shape object
##' @author Jason Wen
##' @import maptools
##' @export
shape.extract = function(shp,column.index = 2)
{

    polygon.data = list()
    j = 0
    col.index = 0
    poly.out = length(shp@polygons)
    index = 0
    for(i in 1:poly.out)
    {
        poly.in = length(shp@polygons[[i]]@Polygons)
        col.index[i] = poly.in
        for(ii in 1:poly.in)
        {
            j = j + 1
            polygon.data[j] = list(shp@polygons[[i]]@Polygons[[ii]]@coords)
            index[j] = dim(polygon.data[[j]])[1]
        }
        
    }
    latlon = do.call(rbind,polygon.data)
    latlon.data = data.frame(latlon = latlon)
	colnames(latlon.data) = c('lon.x','lat.y')
	region = shp[[column.index]]	
    obj = list(latlon = latlon.data,each = index,region = region,col.index = col.index)
    class(obj) <- c("shape.object")
	obj
}

##' Transform the data into the range of [0,1]
##'
##' 
##' @title data transformation
##' @param x a numeric value or vector. 
##' @param transform the method for transformation, can be linear,log,sqrt,exp,power and normal
##' @return a transformed numeric vector
##' @author Jason Wen
##' @export
data.trans = function(x,transform = 'linear')
{
    a = x
    b = a - min(a,na.rm = TRUE)
    transform.option = c('linear','log','sqrt','exp','power','normal')
    impossible.number = 0.091823021983
    bio.color = c('bi.polar')
    if(!(transform %in% transform.option))
    {
        stop("Invaild transform method,please select one method from:
            'linear','log','sqrt','exp','power','normal'")
    }
    
    
    switch(transform,
        linear = {b = b},
        log = {b = log(b + 1)},
        sqrt = {b = sqrt(b)},
        exp = {b = exp(b)},
        power = {b = b^2},
        normal = 
        {
            x = (a - mean(a,na.rm = TRUE))/sd(a,na.rm = TRUE)
            b = dnorm(x,0,1)
            b = b - min(b,na.rm = TRUE)
        },
    )
    
    percent.data = b/diff(range(b,na.rm = TRUE))
    percent.data
}


##' draw a map by passing an iNZightPlot object
##'
##' the function will also returns a global object which called global.objects
##' @title Plot an iNZight Map
##' @param shp.region a character vector
##' @param data.region a character vector
##' @return an integer vector 
##' @author Jason Wen
order.match = function(shp.region,data.region)
{
    order = match(shp.region,data.region)
    orderd.data = order
    na.data = shp.region[is.na(orderd.data)]
    print(paste('number of unmatch region:',length(na.data)))
    orderd.data
}



##' Choose a type of color that helps read the map nicely.
##' @title Color Specification
##' @param data a numeric vector that should lie on the range of [0,1]
##' @param color.index an integer vector
##' @param display a character value, the method for display colors. It should be one value of "hcl","hue","heat","rainbow","terrain","topo","cm","gray","r","n"
##' @param na.fill a character value, fill the unmatch region/country by the color.
##' @param offset a numeric value within the range of [0,1] 
##' @param col the color for fill the match region/country, it only needs to be specify if display = 'hue'
##' @return an color vector
##' @author Jason Wen
##' @import RColorBrewer
col.fun = function(data,color.index,
                    display = 'hue',na.fill = 'white',offset = 0,col = 'red')
{
    display.option = c('hcl','hue','heat','cm','rainbow','terrain','topo','cm','bi.polar','r','n')
    impossible.number = 0.091823021983
    bio.color = c('bi.polar')
    if(!(display %in% display.option))
    {
        stop("display method not found,please select one method from:
            'hcl','hue','heat','cm','rainbow','terrain','topo','cm','bi.polar','r','n' ")
    }
    
    data = round(data,2)
    if(display %in% bio.color)
    {
        fill.float = ifelse(is.na(data) == TRUE, impossible.number,data,gray)
    }else
    {
        fill.float = ifelse(is.na(data) == TRUE, impossible.number,data * (1 - offset) + (offset))
    }
    ###the color can not be offset if it is bio-color
    ###display transform
    switch(display,
    
        hcl = 
		{
			fill.col = hcl(as.numeric(fill.float)*100,l = 85)
		},
        
        hue = 
        {
            char.col.trans = col2rgb(col)/255
            fill.col =rgb(char.col.trans[1],char.col.trans[2],char.col.trans[3],fill.float)
        },
        
        heat = 
        {
            over.col = heat.colors(length(color.index) * 100 + 1)
            orderd.col = over.col[length(over.col):1]
            id = fill.float * length(color.index) * 100 + 1
            fill.col = orderd.col[id]
        },
        
        rainbow = 
        {
            over.col = rainbow(length(color.index) * 100 + 1)
            orderd.col = over.col
            id = fill.float * length(color.index) * 100 + 1
            fill.col = orderd.col[id]            
        },
        
        terrain = 
        {
            over.col = terrain.colors(length(color.index) * 100 + 1)
            orderd.col = over.col
            id = fill.float * length(color.index) * 100 + 1
            fill.col = orderd.col[id]	            
        },
        
        topo = 
        {
            over.col = topo.colors(length(color.index) * 100 + 1)
            orderd.col = over.col
            id = fill.float * length(color.index) * 100 + 1
            fill.col = orderd.col[id]
        },
        
        cm = 
        {
            over.col = cm.colors(length(color.index) * 100 + 1)
            orderd.col = over.col
            id = fill.float * length(color.index) * 100 + 1
            fill.col = orderd.col[id]
            print(fill.col)
        },
        
        bi.polar = 
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
        },
        
        gray =  
        {
            fill.col = gray(round(1 - as.numeric(fill.float),5))
        },
        
        r = 
        {
            r = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
            g = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
            b = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
            fill.col = rgb(r,g,b)	
        },
        
        n = 
        {
            r = runif(length(shp@polygons))
            g = runif(length(shp@polygons))
            b = runif(length(shp@polygons))
            na.fill = rgb(r,g,b)
            fill.col = rgb(r,g,b)	
        }
        
    )
    color.each = ifelse(fill.float== impossible.number, na.fill, fill.col)
    color.out = rep(color.each,color.index)	
    color.out
}



col.missing = function(shape.obj)
{
	region = shape.obj$region
	col.index = shape.obj$col.index
	length = length(rep(region,col.index))
	r = runif(length)
	g = runif(length)
	b = runif(length)
	color.out = rgb(r,g,b)
	color.out
}


##' bind the color and a shape object together
##'
##' the function will also returns a global object which called global.objects
##' @title Plot an iNZight Map
##' @param color a numeric vector see\link{rgb}
##' @param obj an shape obj
##' @return a list that contain shape object and colors for filling
##' @author Jason Wen
color.bind = function(color,obj)
{
    with.color = list(obj = obj, color = color)
    with.color
}

getShapeFile = function()
{
    
}


##' compute the ratio of the current width and height of the window.
##' @title compute the ratio of the window
##' @return a vector of length of 2 that specify the current width and height 
##' @author Jason Wen
##' @export
win.ratio = function()
{
    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
    ##compute the ratio
    xlim = current.viewport()$xscale
    ylim = current.viewport()$yscale
    ratio.map = (diff(xlim)/diff(ylim))
    ratio.win = win.width/win.height
    if(ratio.map < ratio.win)
    {
        h = unit(1,'npc')
        w = unit(ratio.map/ratio.win, 'npc')
    }else{
        w = unit(1,'npc')
        h = unit(ratio.win/ratio.map, 'npc')
    }
    c(w,h)
}


re.scale = function(x,ratio)
{
  mid = mean(x)
  l = diff(x)/2
  l.r = l * ratio
  c(mid - l.r, mid + l.r)
  
}