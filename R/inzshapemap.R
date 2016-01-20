create.inz.shapemapplot <- function(obj) {
    df <- obj$df
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
                    

    ## cols <- col.fun(pf$shape.object$col.fun, pf$shape.object$col.args)
    shp.obj <- color.bind(col, pf$shape.object)

    ## missing data
    v <- colnames(df)
    missing <- is.na(df$x)
    n.missing <- sum(missing)
    df <- df[!missing, ]
    
    ## information extraction
    ## I used the xylim within transformed data, instead of from inz.plot.....
    ## should be re-write in the future

    xlim = range(shp.obj$obj$latlon[, 1])
    ylim = range(shp.obj$obj$latlon[, 2])
    
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
    latlon <<- obj$shape.object$latlon
    cols = obj$colby
    shade.each <- obj$shape.object$each
    ##limit
    xlim =  current.viewport()$xscale
    ylim =  current.viewport()$yscale
    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
    ##compute the ratio
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
    vp = viewport(0.5,0.5,width = w, height = h,name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim)
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
            
}
