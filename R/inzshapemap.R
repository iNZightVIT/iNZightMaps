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
    pf$shape.object$col = col
    obj = pf$shape.object
    obj$extend.ratio = pf$extend.ratio
    obj$xylim = c(range(obj$latlon[,1]),range(obj$latlon[,2]))
    obj$full.map = pf$full.map
    

    ## cols = col.fun(pf$shape.object$col.fun, pf$shape.object$col.args)
    ## missing data
    v = colnames(df)
    missing = is.na(df$x)
    n.missing = sum(missing)
    df = df[!missing, ]
    
    xlim = obj$xylim[1:2]
    ylim = obj$xylim[3:4]

   # print(obj$shape.object$latlon[,1])
    out = list(x = xlim, y = ylim,colby = obj$col,n.missing = n.missing,
                xlim = xlim, ylim = ylim,shape.object = obj,df = df)
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
    s.obj = obj$shape.object
    if(s.obj$full.map == FALSE)
    {
        ratio = s.obj$extend.ratio
        inner.lim = innerLim(s.obj,obj$df$y)
        lim.in = c(re.scale(inner.lim[1:2],ratio),
                    re.scale(inner.lim[3:4],ratio))
                    
        lim.out = outerLim(s.obj,lim.in)
        xlim = lim.out[1:2]
        ylim = lim.out[3:4]
        
        w = convertWidth(current.viewport()$width, "mm", TRUE)
        h = convertHeight(current.viewport()$height, "mm", TRUE)
        x = diff(xlim)
        y = diff(ylim) 
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
        s.obj = subByLim(s.obj,lim)
        
        veiw.wh = c(1,1)
    }else
    {
        xlim = s.obj$xylim[1:2]
        ylim = s.obj$xylim[3:4]
        veiw.wh = win.ratio()
    }
    
    latlon = s.obj$latlon
    cols = s.obj$col
    shade.each = s.obj$each
    ##limit
    vp = viewport(0.5,0.5,width = veiw.wh[1], height = veiw.wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim)
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
    popViewport()
    
    grid.rect(gp = gpar(fill = 'transparent'))
}
