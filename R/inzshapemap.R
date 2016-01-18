create.inz.shapemapplot <- function(obj) {
    df <- obj$df
    opts <- obj$opts

    ## when the colby could not find
    colby = opts$plot.features$shape.obj$color
    if(is.null(colby))
        {
            colby = col.missing(opts$plot.features$shape.obj)
            latlon = opts$plot.features$shape.obj$latlon
            country = opts$plot.features$shape.obj$country
            each = opts$plot.features$shape.obj$each
            
            message('color could not find in the shape object, fill the area randomly')
        }else{
            colby = opts$plot.features$shape.obj$color
            latlon = opts$plot.features$shape.obj$obj$latlon
            country = opts$plot.features$shape.obj$obj$country
            each = opts$plot.features$shape.obj$obj$each
        }
    
    ## missing data
    v <- colnames(df)
    missing <- is.na(df$x)
    n.missing <- sum(missing)
    df <- df[!missing, ]
    
    ## information extraction
    
    ## I used the xylim within transformed data, instead of from inz.plot.....
    ## should be re-write in the future
    xlim = range(latlon[, 1])
    ylim = range(latlon[, 2])
    
    
    out <- list(x = xlim, y = ylim, colby = colby,
                n.missing = n.missing, xlim = xlim, ylim = ylim,
                latlon = latlon, country = country, each = each)
    class(out) <- c("inzshapemap", "inzmap", "inzscatter")
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
    latlon = obj$latlon
    cols = obj$colby
    
    shade.each <- obj$each

    ##limit
    xlim = obj$xlim
    ylim = obj$ylim
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
