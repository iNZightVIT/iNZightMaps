##' extract the information from a SpatialPolygonsDataFrame object.
##'
##' the function will also returns a global object which called global.objects.
##' @title extract and create a shape object
##' @param shp a SpatialPolygonsDataFrame object, see \link{readShapeSpatial}.
##' @return a shape object.
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

##' Transform the data into the range of [0,1].
##'
##' 
##' @title data transformation
##' @param x a numeric value or vector. 
##' @param transform the method for transformation, can be linear,log,sqrt,exp,power and normal.
##' @return a transformed numeric vector.
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

##' draw a map by passing an iNZightPlot object.
##'
##' the function will also returns a global object which called global.objects.
##' @title Plot an iNZight Map
##' @param shp.region a character vector.
##' @param data.region a character vector.
##' @return an integer vector .
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
##' @param data a numeric vector that should lie on the range of [0,1].
##' @param color.index an integer vector from inzshapemap object.
##' @param display a character value, the method for display colors. It should be choosen by one of the following display method: "hcl","hue","heat","rainbow","terrain","topo","cm","gray","r","n".
##' @param na.fill a character value that sepecify the color that use to fill the unmatch region/country.
##' @param offset a numeric value within the range of [0,1] .
##' @param col the color for fill the match region/country, it only needs to be specify if display = 'hue'.
##' @return A color vector.
##' @author Jason Wen
##' @import RColorBrewer
##' @details hcl,HCL Color Specification, whith c = 35 and l = 85 see \link{hcl}. hue, when display = 'hue', then the 'col' arg need to be specified. The alpha will depend on the data, see \link{rgb}. rainbow,terrain,topo,cm are the method from \link{RColorBrewer}. r,n , the color filled randomly expect n will fill the entire map even the region is unmatch.
col.fun = function(data,color.index,
                    display = 'hue',na.fill = 'white',offset = 0,col = 'red')
{
    display.option = c('hcl','hue','heat','cm','rainbow','terrain','topo','cm','bi.polar','r','n','gray')
    impossible.number = 0.091823021983
    bio.color = c('bi.polar')
    if(!(display %in% display.option))
    {
        stop("display method not found,please select one method from:
            'hcl','hue','heat','cm','rainbow','terrain','topo','cm','bi.polar','r','n','grat' ")
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
            r = ifelse(fill.float == impossible.number, impossible.number,runif(fill.float))
            g = ifelse(fill.float == impossible.number, impossible.number,runif(fill.float))
            b = ifelse(fill.float == impossible.number, impossible.number,runif(fill.float))
            fill.col = rgb(r,g,b)	
        },
        
        n = 
        {
            r = runif(length(fill.float))
            g = runif(length(fill.float))
            b = runif(length(fill.float))
            na.fill = rgb(r,g,b)
            fill.col = rgb(r,g,b)	
        }
        
    )
    color.each = ifelse(fill.float== impossible.number, na.fill, fill.col)
    color.out = rep(color.each,color.index)	
    color.out
}

##' compute the ratio of the current width and height of the window.
##' @title compute the ratio of the window.
##' @return a vector of length of 2 that specify the current width and height .
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
    c(w = w,h = h)
}

##' rearrange the limit by given a ratio.
##' @title rearrange the limit
##' @param x a vector of length 2.
##' @param ratio a numeric value.
##' @return a vector of length of 2, re-sized by the ratio.
##' @author Jason Wen
##' @export
re.scale = function(x,ratio)
{
  mid = mean(x)
  l = diff(x)/2
  l.r = l * ratio
  c(mid - l.r, mid + l.r)
  
}

##' is the latitude and longitude within the limit?
##' @title identify the which set of latitude and longitude are within the limit.
##' @param x a numeric matrix with n*2 dimension.
##' @param lim a numeric vectro with length 4.
##' @return a logical vector of length of n, indicates which row in x is within in the limit.
##' @author Jason Wen
##' @export
within = function(x,lim)
{
    (x[,1] > lim[1] & x[,1] < lim[2]) & 
    (x[,2] > lim[3] & x[,2] < lim[4])
}

##' Calculate the inner limit.
##' @title inner limit
##' @details the limit is computed by calculate the limit of the given region.
##' @param obj an inzshapemap object.
##' @param d.region a character vector that sepecify the region or country.
##' @return a numeric vector with length 4, the inner limit/bbox of the map.
##' @author Jason Wen
##' @export
innerLim = function(obj,d.region)
{
    latlon = obj$latlon
    each = obj$each
    col.index = obj$col.index
    region = obj$region
    
    logic = region %in% d.region
    polygon.index.fill = rep(logic,col.index)  
    each.sub = each[rep(logic,col.index)]
    latlon.sub = latlon[rep(rep(logic,col.index),each),]
    lim.sub = c(range(latlon.sub[,1],na.rm = TRUE),range(latlon.sub[,2],na.rm = TRUE))
    lim.sub
}

##' Calculate the outer limit.
##' @title outer limit
##' @details the limit is computed by calculate the limit of the given inner limit. If there is a region been cut by the inner limit, then the outer limit will extend up to the limit of the region.
##' @param obj an inzshapemap object.
##' @param lim a numeric vector of length 4.
##' @param ignore.region a character value or vector, sepecify which regions are been ignored when calculate the limit.
##' @return a numeric vector with length 4, the outer limit/bbox of the map.
##' @author Jason Wen
##' @export
outerLim = function(obj,lim,ignore.region = c('Russia','Antarctica'))
{    
    latlon = obj$latlon
    each = obj$each
    col.index = obj$col.index
    region = obj$region
    
    with = within(latlon,lim)
    id = rep(1:length(each),each)
    each.index = as.numeric(rownames(table(id[with])))
    log.pre = id %in% each.index
    log.pre[rep(rep((region %in% ignore.region),col.index),each)] = FALSE
    latlon.sub = latlon[log.pre,]

    lim.sub = c(range(latlon.sub[,1]),range(latlon.sub[,2]))
    lim.sub
    
    
}


##' Subset by limit.
##' @title subset by limit
##' @param obj an inzshapemap object.
##' @param lim a numeric vector of length 4.
##' @return an inzshapemap object.
##' @author Jason Wen
##' @export
subByLim = function(obj,lim)
{
    latlon = obj$latlon
    each = obj$each
    col.index = obj$col.index
    col = obj$col
    co = col
    region = obj$region
    
    
    with = within(latlon,lim)
    id = rep(1:length(each),each)
    each.index = as.numeric(rownames(table(id[with])))
    each.sub = each[each.index]
    latlon.sub = latlon[id %in% each.index,]
    lim.sub = c(range(latlon.sub[,1]),range(latlon.sub[,2]))
    region.id = rep(rep(region,col.index),each)
    with.sub = within(latlon,lim.sub)
    
    
    latlon.out = latlon[id %in% id[with.sub],]
    each.sub.index = as.numeric(rownames(table(id[with.sub])))
    each.out = each[each.sub.index]
    
    ## some regions have multiple length
    e = rep(rep(1:length(region),col.index),each)
    e.index = as.numeric(rownames(table(e[with.sub])))
    
    ## out
    region.out = region[e.index]
    col.index.out = col.index[e.index]
    col.out = col[each.sub.index]
    lim.out = lim.sub
    
    obj = list(latlon = latlon.out,each = each.out,
             col.index = col.index.out, region = region.out,
             xylim = lim.out,col = col.out)
    obj
}
