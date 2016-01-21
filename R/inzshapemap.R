create.inz.shapemapplot <- function(obj) {
    df <- obj$df
    aa <<- df
    opts <- obj$opts
    pf <- opts$plot.features
    ## Set the colours for countries `y` based on value `x` (in `df`):
    x.trans <- data.trans(df$x, transform = pf$transform)
    x.ord <- x.trans[pf$shape.object$ordered]
    col <- col.fun(data = x.ord, 
                    color.index = pf$shape.object$col.index, 
                    display = pf$col.method,
                    col = pf$col,
                    offset = pf$col.offset,
                    na.fill = pf$na.fill)
    plot.logic = pf$region %in% df$y
    

    plot.logic = (pf$shape.object$region %in% df$y )
    
    plot.polygon.index = rep(plot.logic,pf$shape.object$col.index)
    
    plot.points = rep(plot.polygon.index,pf$shape.object$each)
    latlon = pf$shape.object$latlon[plot.points,]
    each = pf$shape.object$each[plot.polygon.index]
    col.index = pf$shape.object$col.index[plot.logic]
    col = col[plot.polygon.index]
        
    pf$shape.object$latlon = latlon
    pf$shape.object$each = each
    pf$shape.object$col.index = col.index
    
    
    
    aa <<- pf$shape.object
    ## cols <- col.fun(pf$shape.object$col.fun, pf$shape.object$col.args)
    shp.obj <- color.bind(col, pf$shape.object)
    
    xlim = range(latlon[,1])
    ylim = range(latlon[,2])
    ## missing data
    v <- colnames(df)
    missing <- is.na(df$x)
    n.missing <- sum(missing)
    df <- df[!missing, ]
    
    ## information extraction
    ## I used the xylim within transformed data, instead of from inz.plot.....
    ## should be re-write in the future

    
    out <- list(x = xlim, y = ylim, colby = shp.obj$color,
                n.missing = n.missing, xlim = xlim, ylim = ylim,
                shape.object = shp.obj$obj,col = col)
                
    class(out) <- c("inzshapemap", "inzmap", "inzscatter")
    out$draw.axes <- FALSE
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
##' @import maptools
##' @export
plot.inzshapemap <- function(obj, gen) {
    latlon = obj$shape.object$latlon
    cols = obj$colby
    shade.each <- obj$shape.object$each
    ##limit
    xlim =  current.viewport()$xscale
    ylim =  current.viewport()$yscale

    wh = win.ratio()
    print(wh)
    vp = viewport(0.5,0.5,width = wh[1], height = wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim)
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
            
}
