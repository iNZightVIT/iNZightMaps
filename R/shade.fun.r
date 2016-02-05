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
    cents = coordinates(shp)
    area = sapply(slot(shp, "polygons"), slot, "area")
    max.area = tapply(area,region,max)
    i.region = region[area %in% max.area]
    center = cents[area %in% max.area,]
    center.region = data.frame(lon.x = center[,1], lat.y = center[,2], i.region = i.region)
    obj = list(latlon = latlon.data,center.region = center.region,  ## data frame
                each = index,region = region,col.index = col.index) ## list
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
    if(length(a) > 1)
        percent.data
        else
            1

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
##' @param display a character value, the method for display colors. It should be choosen by one of the following display method: "hcl","hue","heat","rainbow","terrain","topo","cm","gray","r","n","e".
##' @param na.fill a character value that sepecify the color that use to fill the unmatch region/country.
##' @param offset a numeric value within the range of [0,1] .
##' @param col the color for fill the match region/country, it only needs to be specify if display = 'hue'.
##' @return A color vector.
##' @author Jason Wen
##' @import RColorBrewer
##' @details hcl,HCL Color Specification, whith c = 35 and l = 85 see \link{hcl}. hue, when display = 'hue', then the 'col' arg need to be specified. The alpha will depend on the data, see \link{rgb}. rainbow,terrain,topo,cm are the method from \link{RColorBrewer}. r,n , the color filled randomly expect n will fill the entire map even the region is unmatch. e, equal color.
col.fun = function(data,color.index,
                    display = 'hue',na.fill = '#F4A460',offset = 0,col = 'red')
{
    display.option = c('hcl','hue','heat','cm','rainbow','terrain','topo','cm','bi.polar','r','n','gray','e')
    impossible.number = 0.091823021983
    bio.color = c('bi.polar')
    if(!(display %in% display.option))
    {
        stop("display method not found,please select one method from:
            'hcl','hue','heat','cm','rainbow','terrain','topo','cm','bi.polar','r','n','grat','e' ")
    }
    
    data = round(data,2)
    if(display %in% bio.color)
    {
        fill.float = ifelse(is.na(data) == TRUE, impossible.number,data)
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
        },
        e = 
        {
            char.col.trans = col2rgb(col)/255
            fill.col =rgb(char.col.trans[1],char.col.trans[2],char.col.trans[3],1)
        }
        
    )
    color.each = ifelse(fill.float== impossible.number, na.fill, fill.col)
    color.out = rep(color.each,color.index)	
    color.out
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
lim.inside = function(x,lim)
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
    
    with = lim.inside(latlon,lim)
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
    aa <<- obj
    
    
    with = lim.inside(latlon,lim)
    id = rep(1:length(each),each)
    each.index = as.numeric(rownames(table(id[with])))
    each.sub = each[each.index]
    latlon.sub = latlon[id %in% each.index,]
    lim.sub = c(range(latlon.sub[,1]),range(latlon.sub[,2]))
    region.id = rep(rep(region,col.index),each)
    with.sub = lim.inside(latlon,lim.sub)
    
    
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
             xylim = lim.out,col = col.out,center.region = obj$center.region)
    obj
}



##' Calaudate the bbox of a country
##'
##' @title Calaudate the bbox of a country
##' @param obj the iNZight Shape Map Object
##' @param name the name of the country
##' @return a 2*2 numeric matrix
##' @author Jason
##' @import countrycode
##' @export
name.match = function(shp.region,data.region)
{
    s = countrycode(shp.region, "country.name", "iso3c")
    d = countrycode(data.region, "country.name", "iso3c")
    is.na(s)
    is.na(d)
    if(all(is.na(s)))
    {
        d = data.region
        s = shp.region
    }
    n = nchar(as.character(data.region))
    if(all(length(n) == 2))
    {
        d = countrycode(data.region, "iso2c", "iso3c")
    }
    list(d = d, s = s)
}

##' compute and return the xlim and ylim within aspect ratio
##'
##' @title win.ratio
##' @param xlim a numeric vector of length 2
##' @param ylim a numeric vector of length 2
##' @return a numeric vector of length 4, the first two componets specified the new xlim and the last two componets specified the new ylim.
##' @author Jason
##' @import countrycode
##' @export
win.ratio = function(xlim,ylim)
{
    x = diff(xlim)
    y = diff(ylim) 
    
    w = convertWidth(current.viewport()$width, "mm", TRUE)
    h = convertHeight(current.viewport()$height, "mm", TRUE)
    
    if(h/w < y/x)
    {
        x.tmp = y/(h/w)
        x.r = x.tmp/x
        xlim = re.scale(xlim,x.r)

    }else
    {
        y.tmp = (h/w) * x
        y.r = y.tmp/y
        ylim = re.scale(ylim,y.r)

    }
    lim = c(xlim,ylim)
    lim
}



##' Calaudate the bbox of a country
##'
##' @title Calaudate the bbox of a country
##' @param obj the iNZight Shape Map Object
##' @param name the name of the country
##' @vector logical value, if it is TRUE then return a vector of length 4 otherwise return an 2*2 matrix
##' @return a 2*2 numeric matrix or a vector of length 4
##' @author Jason
##' @export
region.bbox = function(obj,name,vector = FALSE)
{
    latlon = obj$latlon
    each =obj$each
    region = obj$region
    col.index = obj$col.index

    region.ind = rep(rep(region,col.index),each)
    nn = region.ind %in% name
    lat.lim = range(latlon[nn,1])
    lon.lim = range(latlon[nn,2])
    bbox = c(lat.lim, lon.lim)
    if(vector == FALSE)
    {
        dim(bbox) = c(2,2)
        colnames(bbox) = c('lat','lon')
    }
    bbox
}



bar.coor = function(obj,var,data,xmax = 1,ymax = 18,bar.col = c('#E0FFFF','#FAFAD2','#FFA07A','#C71585'))
{
    region = obj$region
    col.index = obj$col.index
    each = obj$each

    a = rep(rep(1:length(region),col.index),each)
    b = rep(rep(region %in% data$Country,col.index),each)

    s.region = rownames(table(as.character(region[as.numeric(rownames(table(a[b])))])))
    region.lim = do.call(rbind,lapply(s.region,region.bbox,obj = obj,vector = TRUE))
    ind = match(s.region,obj$center.region$i.region)
    x = obj$center.region$lon.x[ind]
    y = obj$center.region$lat.y[ind]


    l = apply(region.lim,1,function(x)c(diff(x[1:2]),diff(x[3:4])))
    plot.num = as.character(length(var))
    bar.weight.tot = as.numeric(plot.num) * xmax * 0.85
    s.length.x = ifelse(l[1,] > bar.weight.tot,xmax,l[1,]/as.numeric(plot.num))/2
    

    switch(plot.num,
       '1' = 
       {
            xl = x - s.length.x
            xr = x + s.length.x
       },
       '2' = {
            xl.1 = x - 2 * s.length.x
            xr.1 = x
            xl.2 = x
            xr.2 = x + 2 * s.length.x
            xl = c(xl.1,xl.2)
            xr = c(xr.1,xr.2)
         
       },
       '3' = {   
            xl.1 = x - 3 * s.length.x
            xr.1 = x - 1 * s.length.x
            xl.2 = x - 1 * s.length.x
            xr.2 = x + 1 * s.length.x
            xl.3 = x + 1 * s.length.x
            xr.3 = x + 3 * s.length.x   
            xl = c(xl.1,xl.2,xl.3)
            xr = c(xr.1,xr.2,xr.3)                
       },
       '4' = {
            xl.1 = x - 4 * s.length.x
            xr.1 = x - 2 * s.length.x
            xl.2 = x - 2 * s.length.x
            xr.2 = x
            xl.3 = x
            xr.3 = x + 2 * s.length.x 
            xl.4 = x + 2 * s.length.x  
            xr.4 = x + 4 * s.length.x 
            xl = c(xl.1,xl.2,xl.3,xl.4)
            xr = c(xr.1,xr.2,xr.3,xr.4)  
       }
    )

    data.sub = data[which(data$Country %in% s.region),]
    data.in = data.sub[var]
    data.in[is.na(data.in)] = 0
    data.t = apply(data.in,2,data.trans)
    data.matrix = as.matrix(data.t)
    dim(data.matrix) = c(dim(data.matrix)[1] * dim(data.matrix)[2],1)
    yt =  rep(y,length(var)) + data.matrix * ymax
    yb = y
    sep.n = nrow(data.in)
    a = cbind(xl,xr,yb,yt)
    e = a[rep(1:dim(a)[1],each = 5) + rep(c(0,dim(a)[1],dim(a)[1],0,0),dim(a)[1])]
    f = a[rep(1:dim(a)[1],each = 5) + rep(c(2*dim(a)[1],2*dim(a)[1],3*dim(a)[1],3*dim(a)[1],2*dim(a)[1]),dim(a)[1])]

    d1 = cbind(e,f)
    ee1 = rep(5,dim(a)[1])
    col = rep(bar.col,each = sep.n)
    bar.obj = list(d1 = d1,col = col,each = ee1)

}


##' Zoom in/out when click the plot
##'
##' @title Zoom in/out
##' @param ratio a numeric value, define the ratio of zomm in or out
##' @return NULL
##' @details if ratio < 1 then zoom in, if ratio > 1 then zoom out, if ratio = 1 then shift the plot.
##' @author Jason
##' @export
sClickOnZoom = function(ratio = 1/2)
{
    s.obj = inzshpobj$s.obj
    bar.obj = inzshpobj$bar.obj
    name = inzshpobj$name    
    latlon = s.obj$latlon
    cols = s.obj$col
    shade.each = s.obj$each
    region.name = inzshpobj$region.name
    value = inzshpobj$value
    ylim = c(-10,10)
    
    ox.lim = inzshpobj$s.obj$xylim
    
    center.x = s.obj$center.region$lon.x
    center.y = s.obj$center.region$lat.y
    region.name = s.obj$center.region$i.region
        
    if(inzshpobj$num == 1)
        seekViewport('VP:MAPSHAPES')

    xylim = c(current.viewport()$xscale,current.viewport()$yscale)
    p.npc = grid.locator()
    p.center = as.numeric(p.npc)
    nx.lim = rep(p.center[1],2) + c(-1,1) * diff(xylim[1:2]) * ratio * 1/2
    ny.lim = rep(p.center[2],2) + c(-1,1) * diff(xylim[3:4]) * ratio * 1/2
    vp = viewport(0.5,0.5,1,1,name = 'VP:map',xscale = nx.lim,yscale = ny.lim)
    pushViewport(vp)


    grid.polygon(c(-360,360,360,-360,-360),c(360,360,-360,-360,360),
                    default.units = "native",gp = gpar(col = '#B29980', fill  = '#F5F5F5'))
    grid.polygon(s.obj$latlon[,1], s.obj$latlon[,2], 
             default.units = "native", id.length = s.obj$each,
             gp = gpar(col = '#B29980', fill  = s.obj$col))
    drawing.option(bar.obj = bar.obj,
                    latlon = latlon,cols = cols,
                    shade.each = shade.each,region.name = region.name ,
                    value = value ,name = name,
                    center.x = center.x,center.y = center.y ,y.shift = ylim)  
    grid.rect(gp = gpar(fill = 'transparent'))
    inzshpobj$num <<- inzshpobj$num + 1
}




drawing.option = function(bar.obj,latlon,cols,shade.each,region.name,value,name,center.x,center.y,y.shift)
{
    full.option = c('bar','r','v','b')
    switch(name,
        'bar' = 
        {
            xmax = 0.004 * diff(range(latlon[,1]))
            ymax = 0.1 * diff(range(latlon[,2]))
            grid.polygon(bar.obj$d1[,1],bar.obj$d1[,2],
                        default.units = "native", id.length = bar.obj$each,
                        gp = gpar(col = '#B29980', fill  = bar.obj$col))
            out.str = countrycode(region.name, "country.name", "iso3c")
            center.y = center.y - diff(y.shift) * 0.01            
        },
        'r' = 
        {
            out.str = region.name
        },
        'v' = 
        {
            center.x = center.x[!is.na(value)]
            center.y = center.y[!is.na(value)]
            out.str = value[!is.na(value)]
        },
        'b' = 
        {
            value[is.na(value)] = ''
            out.str = ifelse(value == '',paste(region.name),paste(region.name,value,sep = '\n'))
        }
    )
    if(name %in% full.option){
        grid.text(out.str, x = center.x, y =center.y,
                just = "centre",default.units = "native",
                gp=gpar(fontsize=9), check=TRUE)
    }
}
    
