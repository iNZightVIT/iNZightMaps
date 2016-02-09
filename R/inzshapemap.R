##' draw a shape file ...
##'
##' details...
##' @param obj an object from within iNZightPlot
##' @return Object
##' @author Tom Elliott
##' @import iNZightPlots maptools
##' @export
create.inz.shapemapplot = function(obj) {
    df = obj$df
    opts = obj$opts
    pf = opts$plot.features
    x.trans = data.trans(df$x, transform = pf$transform)
    x.ord = x.trans[pf$shape.object$ordered]
    col = col.fun(data = x.ord, color.index = pf$shape.object$col.index, 
                display = pf$col.method,col = pf$col,
                offset = pf$col.offset,na.fill = pf$na.fill)
    pf$shape.object$col = col
    obj = pf$shape.object
    obj$extend.ratio = pf$extend.ratio
    obj$xylim = c(range(obj$latlon[,1]),range(obj$latlon[,2]))
    obj$full.map = pf$full.map
    

    ## cols = col.fun(pf$shape.object$col.fun, pf$shape.object$col.args)
    v = colnames(df)
    ## missing data
    missing = is.na(df$x)
    n.missing = sum(missing)
    df = df[!missing, ]
    
    xlim = obj$xylim[1:2]
    ylim = obj$xylim[3:4]

    out = list(x = xlim, y = ylim,colby = obj$col,n.missing = n.missing,
                xlim = xlim, ylim = ylim,shape.object = obj,df = df,name = pf$name)
    class(out) = c("inzshapemap", "inzmap", "inzscatter")
    out$draw.axes = FALSE
    out
}


##' draw a shape file ...
##'
##' details...
##' @title Plot an iNZight Shape Map
##' @param obj object passed from iNZightPlot
##' @param gen other options passed from iNZightPlot
##' @return NULL
##' @author Jason Wen
##' @export
plot.inzshapemap = function(obj, gen) {
    df = obj$df
    full.s.obj = obj$shape.object
    bbox = full.s.obj$bbox
    s.obj =full.s.obj
    
    name = obj$name
    if(s.obj$full.map == FALSE)
    {
        ratio = s.obj$extend.ratio
        inner.lim = innerLim(s.obj,obj$df$y)
        lim.in = c(re.scale(inner.lim[1:2],ratio),
                    re.scale(inner.lim[3:4],ratio))
                    
        ## make sure the bar within the viewport
        if(name == 'bar')
        {
            latlon = s.obj$latlon
            #bar.obj = bar.coor(var = var, data = dataIn, x = xx, y = yy, xmax = xmax, ymax = ymax)
            xmax = 0.004 * diff(range(latlon[,1]))
            ymax = 0.1 * diff(range(latlon[,2]))
            lim.in = c(lim.in[1:3],max(lim.in[4],max(bar.obj$d1[,2],na.rm = TRUE)))

        }
        lim.out = outerLim(s.obj,lim.in)
        lim = win.ratio(lim.out[1:2],lim.out[3:4])
        xlim = lim[1:2]
        ylim = lim[3:4]
        s.obj = subByLim(s.obj,lim)
    }else
    {
        lim = win.ratio(s.obj$xylim[1:2],s.obj$xylim[3:4])
        xlim = lim[1:2]
        ylim = lim[3:4]
    }
    
    latlon = s.obj$latlon
    cols = s.obj$col
    shade.each = s.obj$each
    center.x = s.obj$center.region$lon.x
    center.y = s.obj$center.region$lat.y
    region.name = s.obj$center.region$i.region
    order = match(region.name,df$y)
    value = round(df$x[order],2)
    
    vp = viewport(0.5,0.5,width = 1, height = 1,name = 'VP:MAPSHAPES', xscale = xlim,yscale = ylim)
    pushViewport(vp)
    ## backagound
    grid.polygon(c(-360,360,360,-360,-360),
                c(360,360,-360,-360,360),
                    default.units = "native",
                    gp = gpar
                    (
                        col = '#B29980', fill  = '#F5F5F5'
                    )
                )
    ## shp polygon
    grid.polygon(latlon[,1], latlon[,2], 
                default.units = "native", id.length = shade.each,
                gp = 
                    gpar(col = '#B29980', fill  = cols)
                )
    ## other features added into the map
    drawing.option(bar.obj = bar.obj,
                latlon = latlon,cols = cols,
                shade.each = shade.each,region.name = region.name,
                value = value ,name = name,
                center.x = center.x,center.y = center.y ,y.shift = ylim)
    
    grid.rect(gp = gpar(fill = 'transparent'))
    inzshpobj <<- list(s.obj = full.s.obj,bar.obj = bar.obj,name = name,value = value,region.name = region.name,num = 1,bbox = bbox)
}
