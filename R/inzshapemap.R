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

    wh = win.ratio()

    
    
    if(obj$shape.object$full.map == FALSE)
    {
        ratio = obj$shape.object$extend.ratio
        obj.fill = subByRegion(obj$shape.object,obj$df$y)
        lim.in = c(re.scale(obj.fill$xylim[1:2],ratio),
                    re.scale(obj.fill$xylim[3:4],ratio))
            print(lim.in)
        obj = subByLim(obj$shape.object,lim.in)
        big.region = c('Russia','Antarctica')
        xlim = obj$xylim[1:2]
        ylim = obj$xylim[3:4]
        
        w = convertWidth(current.viewport()$width, "mm", TRUE)
        h = convertWidth(current.viewport()$height, "mm", TRUE)
        
        x = (diff(xlim))/2
        y = diff(ylim)
        
        print(h/y)
        print(w/x)
        if(h/y > w/x)
        {
            x.tmp = y/(h/w)
            y.new = y
            x.r = x.tmp/x
            xlim = re.scale(xlim,x.r)
            print(xlim)
        }else
        {
            x.new = x
            y.tmp = (h/w) * x
            y.r = y.tmp/y
            ylim = re.scale(ylim,y.r)
            print(ylim)
        }
        lim = c(xlim,ylim)
        #obj = subByLim(obj$shape.object,lim)

    }
    
    latlon = obj$latlon
    cols = obj$col
    shade.each = obj$each
    ##limit
    xlim = obj$xylim[1:2]
    ylim = obj$xylim[3:4]
    
    vp = viewport(0.5,0.5,width = wh[1], height = wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim)
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
    popViewport()
    
    grid.rect(gp = gpar(fill = 'transparent'))
}
