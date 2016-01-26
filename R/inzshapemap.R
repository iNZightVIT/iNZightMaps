##' draw a shape file ...
##'
##' details...
##' @param obj an object from within iNZightPlot
##' @return Object
##' @author Tom Elliott
##' @import iNZightPlots
##' @import grid maptools
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
    extend.ratio = pf$extend.ratio
    pf$shape.object$col = col
    if(pf$full.map == FALSE)
    {
        obj.fill = sub.region(pf$shape.object,df$y)
        lim.in = c(re.scale(obj.fill$xylim[1:2],extend.ratio),
                    re.scale(obj.fill$xylim[3:4],extend.ratio))
        obj = sub.lim(pf$shape.object,lim.in)
        
    }
    ## cols = col.fun(pf$shape.object$col.fun, pf$shape.object$col.args)
    ## missing data
    v = colnames(df)
    missing = is.na(df$x)
    n.missing = sum(missing)
    df = df[!missing, ]
    
    xlim = obj$xylim[1:2]
    ylim = obj$xylim[3:4]
    out = list(x = xlim, y = ylim,colby = obj$col,n.missing = n.missing,
                xlim = xlim, ylim = ylim,shape.object = obj)
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
    latlon = obj$shape.object$latlon
    
    cols = obj$colby
    shade.each = obj$shape.object$each
    ##limit
    xlim = obj$xlim
    ylim = obj$ylim
    
    wh = win.ratio()
    vp = viewport(0.5,0.5,width = wh[1], height = wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim,clip = 'on')
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
    popViewport()
    
    grid.rect(gp = gpar(fill = 'transparent'))
}
