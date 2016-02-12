##' extract the information from a SpatialPolygonsDataFrame object.
##'
##' the function will also returns a global object which called global.objects.
##' @title extract and create a shape object
##' @param shp a SpatialPolygonsDataFrame object, see \link{readShapeSpatial}.
##' @return a shape object.
##' @author Jason Wen
##' @import maptools
##' @export
##' @examples
##' location = 'C:/Users/yeamin/Desktop/something/MED Final 20140226_region.shp'
##' shape.extract(location)
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
##' @examples
##' x = runif(100,1,100)
##' data.trans(x)
##' @export
data.trans = function(x,transform = 'linear',data.range)
{
    a = x
    b = a - min(data.range,na.rm = TRUE)
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
    percent.data = b/diff(data.range)
    if(length(a) > 1)
        percent.data
        else
            1

}

##' re-order the region name in shape file
##'
##' @title re-order the region name in shape file
##' @param shp.region a character vector.
##' @param data.region a character vector.
##' @return an integer vector.  
##' @author Jason Wen
##' @examples
##' los = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
##' shp = readRDS(los)
##' shp.region = shp$region
##' lod = 'C:/Users/yeamin/Desktop/Gapminder-2008.csv'
##' data = read.csv(lod,skip = 1)
##' order.match(shp.region,data$Country)
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
##' @param na.fill a character value that specify the color that use to fill the unmatch region/country.
##' @param offset a numeric value within the range of [0,1] .
##' @param col the color for fill the match region/country, it only needs to be specify if display = 'hue'.
##' @return A color vector.
##' @author Jason Wen
##' @import RColorBrewer
##' @details hcl,HCL Color Specification, whith c = 35 and l = 85 see \link{hcl}. hue, when display = 'hue', then the 'col' arg need to be specified. The alpha will depend on the data, see \link{rgb}. rainbow,terrain,topo,cm are the method from \link{RColorBrewer}. r,n , the color filled randomly expect n will fill the entire map even the region is unmatch. e, equal color for all matched region.
##' @examples
##' r.data = runif(100,1,100)
##' p.data = data.trans(r.data)
##' color.index = rep(1,100)
##' col.fun(p.data,color.index)
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
##' @examples
##' x = c(0,100)
##' ratio = 0.5
##' re.scale(x,ratio)
re.scale = function(x,ratio)
{
  mid = mean(x)
  l = diff(x)/2
  l.r = l * ratio
  c(mid - l.r, mid + l.r)
  
}

##' is the latitude and longitude within the limit?
##' @title identify the which set of latitude and longitude are within the limit.
##' @param x a numeric matrix of n*2 dimension.
##' @param lim a numeric vector of length 4.
##' @return a logical vector of length of n, indicates which row in x is within in the limit.
##' @author Jason Wen
##' @export
##' @examples
##' x = cbind(runif(100,-50,50),runif(100,-100,100))
##' lim = c(-25,25,-80,80)
##' lim.inside(x,lim)
lim.inside = function(x,lim)
{
    (x[,1] > lim[1] & x[,1] < lim[2]) & 
    (x[,2] > lim[3] & x[,2] < lim[4])
}

##' Calculate the inner limit.
##' @title inner limit
##' @details the limit is computed by calculate the limit of the given region.
##' @param obj an inzshapemap object.
##' @param d.region a character vector that specify the region or country.
##' @return a numeric vector with length 4, the inner limit/bbox of the map.
##' @author Jason Wen
##' @export
##' @examples
##' los = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
##' lod = 'C:/Users/yeamin/Desktop/Gapminder-2008.csv'
##' data = read.csv(lod,skip = 1)
##' obj = iNZightShapeMap(los, data.region = 'Country', data = data)
##' d.region = data$Country
##' innerLim(obj,d.region)
innerLim = function(obj,d.region)
{
    latlon = obj$latlon
    each = obj$each
    col.index = obj$col.index
    region = obj$region
    bbox = obj$bbox
    
    logic = region %in% d.region
    if(all(logic == FALSE))
        lim.sub = bbox
    else
    {
        polygon.index.fill = rep(logic,col.index)  
        each.sub = each[rep(logic,col.index)]
        latlon.sub = latlon[rep(rep(logic,col.index),each),]
        lim.sub = c(range(latlon.sub[,1],na.rm = TRUE),
                    range(latlon.sub[,2],na.rm = TRUE)
                    )
    }
    lim.sub
}

##' Calculate the outer limit.
##' @title outer limit
##' @details the limit is computed by calculate the limit of the given inner limit. If there is a region been cut by the inner limit, then the outer limit will extend up to the limit of the region.
##' @param obj an inzshapemap object.
##' @param lim a numeric vector of length 4.
##' @param ignore.region a character value or vector, specify which regions are been ignored when calculate the limit.
##' @return a numeric vector with length 4, the outer limit/bbox of the map.
##' @author Jason Wen
##' @export
##' @examples
##' los = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
##' lod = 'C:/Users/yeamin/Desktop/Gapminder-2008.csv'
##' data = read.csv(lod,skip = 1)
##' obj = iNZightShapeMap(los, data.region = 'Country', data = data)
##' lim = c(-40,40,-40,40)
##' outerLim(obj,lim)
outerLim = function(obj,lim,ignore.region = c('Russia','Antarctica'))
{    
    latlon = obj$latlon
    each = obj$each
    col.index = obj$col.index
    region = obj$region
    
    with = lim.inside(latlon,lim)
    id = rep(1:length(each),each)
    each.index = unique(id[with])
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
##' @examples
##' los = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
##' lod = 'C:/Users/yeamin/Desktop/Gapminder-2008.csv'
##' data = read.csv(lod,skip = 1)
##' obj = iNZightShapeMap(los, data.region = 'Country', data = data)
##' lim = c(-40,40,-40,40)
##' subByLim(obj,lim)
subByLim = function(obj,lim)
{
    latlon = obj$latlon
    each = obj$each
    col.index = obj$col.index
    col = obj$col
    region = obj$region
    
    with = lim.inside(latlon,lim)
    id.index = 1:length(each)
    id = rep(id.index,each)
    each.index = unique(id[with])
    each.sub = each[each.index]
    latlon.sub = latlon[id %in% each.index,]
    lim.sub = c(range(latlon.sub[,1]),range(latlon.sub[,2]))
    region.id = rep(rep(region,col.index),each)
    with.sub = lim.inside(latlon,lim.sub)
    
    latlon.out = latlon[id %in% id[with.sub],]
    each.sub.index = unique(id[with.sub])
    each.out = each[each.sub.index]
    
    ## some regions have multiple length
    e = rep(rep(1:length(region),col.index),each)
    e.index = unique(e[with.sub])
    
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
##' @examples
##' los = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
##' shp = readRDS(los)
##' shp.region = shp$region
##' lod = 'C:/Users/yeamin/Desktop/Gapminder-2008.csv'
##' data = read.csv(lod,skip = 1)
##' name.match(shp.region,data$Country)
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
##' @examples
##' xlim = c(-90,90)
##' ylim = c(-80,90)
##' grid.newpage()
##' win.ratio(xlim,ylim)
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
##' @param name a character vector, the name of the country
##' @param vector logical value, if it is TRUE then return a vector of length 4 otherwise return an 2*2 matrix
##' @return a 2*2 numeric matrix or a vector of length 4
##' @author Jason
##' @export
##' @examples
##' los = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
##' lod = 'C:/Users/yeamin/Desktop/Gapminder-2008.csv'
##' data = read.csv(lod,skip = 1)
##' obj = iNZightShapeMap(los, data.region = 'Country', data = data)
##' name = data$Country
##' region.bbox(obj,name)
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


##' a function for create a bar object
##'
##' @title create bar object
##' @param obj a inzightshapemap object
##' @param var a character vectror, the column name of data
##' @param data the data set
##' @param xmax the maximum weight of the bar
##' @param ymax the maximum height of the bar
##' @param bar.col the color of each bar
##' @return NULL
##' @author Jason
##' @export
bar.coor = function(obj,var,data,xmax = 0.85,ymax = 2,
                    bar.col = c('#E0FFFF','#FAFAD2','#FFA07A','#C71585','#DC143C','#B8860B','#00BFFF','#ADFF2F'))
{
    region = obj$region
    col.index = obj$col.index
    each = obj$each

    a = rep(rep(1:length(region),col.index),each)
    b = rep(rep(region %in% data$Country,col.index),each)
    s.region = unique(region[unique(a[b])])
    region.lim = do.call(rbind,lapply(s.region,region.bbox,obj = obj,vector = TRUE))
    ind = match(s.region,obj$center.region$i.region)
    x = obj$center.region$lon.x[ind]
    y = obj$center.region$lat.y[ind]

    l = apply(region.lim,1,function(x)c(diff(x[1:2]),diff(x[3:4])))
    plot.num = as.character(length(var))
    bar.weight.tot = as.numeric(plot.num) * xmax
    s.length.x = ifelse(l[1,] > bar.weight.tot,xmax,l[1,]/as.numeric(plot.num))/2
    
    n = as.numeric(plot.num)
    if(n%%2 != 0)
    {
        if(n == 1)
            a = 1
        else
            a = c(n,rep(seq(n-2,1)[c(TRUE,FALSE)],each = 2))
    }else
    {
        if(n == 2) 
            a = c(2,0)
        else
            a = c(n,rep(((n-2):2)[c(T,F)],each = 2),0)
    }
    out = c(-a,a[length(a):1])
    xtot = rep(x,length(out)) + rep(s.length.x,length(out)) * rep(out,each = length(x))

    xl.sub = (1:length(x)) + rep(seq(0,n*2 - 1,2),each = length(x)) * length(x)
    xr.sub = (1:length(x)) + rep(seq(1,n*2,2),each = length(x)) * length(x)
    
    rep(seq(0,(n - 1)*2,2),each = length(x)) * length(x)
    
    xl = xtot[xl.sub]
    xr = xtot[xr.sub]
    
    data.sub = data[which(data$Country %in% s.region),]
    data.in = data.sub[var]
    data.in[is.na(data.in)] = 0
    data.t = apply(data.in,2,data.trans)
    data.matrix = as.matrix(data.t)
    dim(data.matrix) = c(dim(data.matrix)[1] * dim(data.matrix)[2],1)
    yt = rep(y,length(var)) + data.matrix * ymax
    yb = y
    sep.n = nrow(data.in)
    a = cbind(xl,xr,yb,yt)
    x.coor = a[rep(1:dim(a)[1],each = 5) + rep(c(0,dim(a)[1],dim(a)[1],0,0),dim(a)[1])]
    y.coor = a[rep(1:dim(a)[1],each = 5) + rep(c(2*dim(a)[1],2*dim(a)[1],3*dim(a)[1],3*dim(a)[1],2*dim(a)[1]),dim(a)[1])]

    d1 = cbind(x.coor,y.coor)
    each.polygon = rep(5,dim(a)[1])
    col = rep(bar.col,each = sep.n)
    bar.obj = list(d1 = d1,col = col,each = each.polygon)

}


##' Zoom in/out when click the plot
##'
##' @title Zoom in/out 
##' @param ratio a numeric value, define the ratio of zomm in or out
##' @return NULL
##' @details if ratio < 1 then zoom in, if ratio > 1 then zoom out, if ratio = 1 then shift the plot.
##' @author Jason
##' @export
##' @examples
##' los = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds'
##' data = read.csv('C:/Users/yeamin/Desktop/Gapminder-2008.csv',skip = 1)
##' obj <- iNZightShapeMap(los, data.region = 'Country', data = data)
##' var = c('BodyMassIndex_F','ChildrenPerWoman','Populationtotal','Populationdensity')
##' bar.obj = bar.coor(obj = obj,var = var, data = data, xmax = 1, ymax = 5)
##' plot(obj, variable = ~Imports,
##'                  region = ~Country,
##'                  data = data,
##'                  col.fun = 'hue',
##'                  col = 'orange',
##'                  transform = 'linear',
##'                  na.fill = '#C0C0C0',
##'                  col.offset = 0,
##'                  full.map = F,
##'                  extend.ratio = 1,
##'                  name = 'v')
##' sClickOnZoom()
sClickOnZoom = function(ratio = 1/4,resize = FALSE)
{
    s.obj = inzshpobj$s.obj
    bar.obj = inzshpobj$bar.obj
    name = inzshpobj$name    
    latlon = s.obj$latlon
    cols = s.obj$col
    shade.each = s.obj$each
    region.name = inzshpobj$region.name
    value = inzshpobj$value
    sbbox = inzshpobj$bbox
    ylim = c(-10,10)
    

    ox.lim = inzshpobj$s.obj$xylim
    center.x = s.obj$center.region$lon.x
    center.y = s.obj$center.region$lat.y
    region.name = s.obj$center.region$i.region

    if(inzshpobj$num == 1)
        seekViewport('VP:MAPSHAPES')
    
    
    if(resize == FALSE)
    {
        p.center = as.numeric(grid.locator())
        xylim = c(current.viewport()$xscale,current.viewport()$yscale)
    }
    else
    {
        p.center = inzshpobj$click.point
        xylim = win.ratio(inzshpobj$bbox.record[1:2],inzshpobj$bbox.record[3:4])
    }    
        
    nx.lim = rep(p.center[1],2) + c(-1,1) * diff(xylim[1:2]) * ratio
    ny.lim = rep(p.center[2],2) + c(-1,1) * diff(xylim[3:4]) * ratio
    n.lim = c(nx.lim,ny.lim)

    if(resize == FALSE)
        inzshpobj$bbox.record <<- c(nx.lim,ny.lim)

    s.obj = subByLim(s.obj,c(nx.lim,ny.lim))
    if(ratio > 0.5){
        if(diff(range(nx.lim)) > diff(sbbox[1:2]) & diff(range(ny.lim)) > diff(sbbox[3:4]))
            n.lim = win.ratio(sbbox[1:2],sbbox[3:4])
    }

    vp = viewport(0.5,0.5,1,1,name = 'VP:map',xscale = n.lim[1:2],yscale = n.lim[3:4])
    pushViewport(vp)

    grid.rect(gp = gpar(fill = '#F5F5F5'))
    
    grid.polygon(s.obj$latlon[,1], s.obj$latlon[,2], 
             default.units = "native", id.length = s.obj$each,
             gp = gpar(col = '#B29980', fill  = s.obj$col))
    drawing.features(bar.obj = bar.obj,
                    latlon = latlon,cols = cols,
                    shade.each = shade.each,region.name = region.name ,
                    value = value ,name = name,
                    center.x = center.x,center.y = center.y)
    grid.rect(gp = gpar(fill = 'transparent'))
    
    inzshpobj$num <<- inzshpobj$num + 1
    inzshpobj$click.point <<- p.center
    
}


##' change the zoom within the center point
##'
##' @title change the zoom within the center point
##' @param zoom a numeric value between 0.1 to 0.9(minimum zoom to maximum zoom)
##' @return NULL
##' @author Jason
##' @export
srezoom = function(zoom)
{
    if(zoom > 0.9 | zoom < 0.1)
        stop('invalid zoom')
    sClickOnZoom(ratio = zoom,resize = TRUE)
}


##' a function for drawing bar char, display the value and/or region name
##'
##' @title drawing features
##' @param bar.obj a bar object see \link{bar.coor}
##' @param latlon a n*2 numeric matrix the first column specifies the latitudes and the second column specifies the longitudes 
##' @param cols a color character strings vector
##' @param shade.each a numeric vector
##' @param region.name a character vector 
##' @param value a numeric vector
##' @param name a character vector
##' @param center.x a numeric value
##' @param center.y a numeric value
##' @param y.shift a numeric value
##' @return NULL
##' @author Jason
##' @export
drawing.features = function(bar.obj,latlon,cols,
                            shade.each,region.name,
                            data.region,value,name,
                            center.x,center.y,
                            y.shift = 0.5)
{
    
    full.option = c('bar','r','v','b')
    switch(name,
        'bar' = 
        {
            xmax = 0.004 * diff(range(latlon[,1]))
            ymax = 0.1 * diff(range(latlon[,2]))
            grid.polygon(bar.obj$d1[,1],bar.obj$d1[,2],
                        default.units = "native", id.length = bar.obj$each,
                        gp = gpar(col = NA, fill  = bar.obj$col))
            out.str = countrycode(region.name, "country.name", "iso3c")
            center.y = center.y - y.shift
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
    