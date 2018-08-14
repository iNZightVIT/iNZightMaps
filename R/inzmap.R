##' @title title
##' @param obj an object from within iNZightPlot
##' @return Object
##' @author Tom Elliott
##' @import iNZightPlots
##' @import grid maptools
##' @export
create.inz.mapplot <- function(obj) {
    map.type <- obj$opts$plot.features$maptype
    
    ## Create the global object if it isn't already
    if (!"global.objects" %in% ls(envir = .GlobalEnv))
        assign("global.objects", list(), envir = .GlobalEnv)

    features <- obj$opts$plot.features
    
    map.type <- obj$opts$plot.features$maptype
    opts <- obj$opts
    
    ##  it's a "scatter" plot ...
    out <- NextMethod()
    out$map.type = map.type
    
    ##here I do the 'shift' data, it should be done 'before-this-function' call
    ##should be re-write in the future
    is.google.map(out$y,out$x)
    
    out$x = lon.rescale(out$x)
    out$xlim = range(out$x)
    
    ## sort out opacity
    if (!is.null(features$opacity)) {
        ## Move the computation to: function_def - plot.inzightmap; function_eval - iNZightPlots
        out$opacity <- obj$df[[features$opacity]]
        if (any(out$opacity < 1 ))
            out$pch = rep(19, length(out$pch))
    }
    
    class(out) <- c("inzmap", class(out))
    
    out$draw.axes <- FALSE
    out
}


##' draw a map by passing an iNZightPlot object
##'
##' the function will also returns a global object which called global.objects
##' @title Plot an iNZight Map
##' @param obj object passed from iNZightPlot
##' @param gen other options passed from iNZightPlot
##' @param ... additional arguments (ignored)
##' @return NULL
##' @author Jason Wen
##' @import RgoogleMaps
##' @export
plot.inzmap <- function(x, gen, ...) {
    obj <- x
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args
    plot.shp <- opts$plot.features$plot.shp
    
    if (is.null(obj$opacity)) {
        opacity <- 1            
    } else {
        opacity <- obj$opacity
    }
    
    debug <- if (is.null(opts$debug)) FALSE else opts$debug
    
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    
    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
    SCALE  <-  2
    size <- global.objects$maps$map$size
    type <- obj$map.type
    
    get.newmap <- needNewMap(bbox = c(xlim, ylim), size = size, SCALE = SCALE,
                             type = type, window = c(win.width, win.height))

    
    ## need to come up with a better way of doing this!!
    if (get.newmap) {
        getNewMap(lat.lim = ylim, lon.lim = xlim, SCALE = SCALE, type = type,
                  zoom = Get.map.size(ylim, xlim)$zoom)
        ## updating
        global.objects$maps$map.detail$window <<- c(win.width, win.height)
        global.objects$maps$map.detail$bbox <<- c(xlim, ylim)
        global.objects$maps$map.detail$size <<- global.objects$maps$map$size
        global.objects$maps$map.detail$scale <<- global.objects$maps$map$SCALE
        global.objects$maps$map.detail$type <<- type
        global.objects$maps$map.detail$points <<- cbind(obj$y, obj$x)
    }
        
    ptCols <- iNZightPlots:::colourPoints(obj$colby, col.args, opts)
    ## passing the details inorder to redraw.
    global.objects$maps$pf$cex <<- obj$propsize
    global.objects$maps$pf$col <<- ptCols
    global.objects$maps$pf$lwd <<- opts$lwd.pt
    global.objects$maps$pf$alpha <<- opts$alpha * opacity
    global.objects$maps$pf$fill <<- obj$fill.pt
    global.objects$maps$pf$opacity <<- opacity  
    global.objects$maps$pf$pch <<- obj$pch    
    global.objects$maps$map.detail$num <<- 1
    global.objects$maps$pf$click.points <<- c(mean(xlim),mean(ylim))
    global.objects$maps$pf$bbox.record <<- c(xlim,ylim)
    
    ## drawing~~~~
    grid.raster(global.objects$maps$map$myTile,0.5,0.5,1,1)
    
    ## define the limit
    tmp <- map.xylim(ylim,xlim,SCALE = SCALE)$window.lim
    xl <- tmp[1:2]
    yl <- tmp[3:4]
    
    ## setting the viewport
    vp <- viewport(0.5,0.5,1,1,name = 'VP:PLOTlayout',xscale = xl, yscale = yl)
    pushViewport(vp)
    
    ## transform the points
    dd <- cbind(obj$y,obj$x)
    point <- latlon.xy(dd,map = global.objects$maps$map)
    
    ## other scatter plot things
    if (length(obj$x) == 0)
        return()
    
    NotInView <- obj$x < min(xlim) | obj$x > max(xlim) | obj$y < min(ylim) | obj$y > max(ylim)
    obj$pch[NotInView] <- NA
    grid.points(point[[1]], point[[2]], pch = obj$pch,
                gp =
                    gpar(col = ptCols,
                         cex = obj$propsize,
                         lwd = opts$lwd.pt, alpha = opts$alpha * opacity,
                         fill = if (obj$fill.pt == "fill") ptCols else obj$fill.pt),
                name = "SCATTERPOINTS")
    

    ## Connect by dots if they want it ...
    if (opts$join) {
        if (length(unique(obj$colby)) <= 1 | !opts$lines.by) {
            grid.lines(point[[1]], point[[2]], default.units = "native",
                       gp =
                           gpar(lwd = opts$lwd, lty = opts$lty,
                                col = opts$col.line))
        } else {
            byy <- as.factor(obj$colby)  # pseudo-by-variable
            xtmp <- lapply(levels(byy), function(c) subset(point[[1]], obj$colby == c))
            ytmp <- lapply(levels(byy), function(c) subset(point[[2]], obj$colby == c))

            for (b in 1:length(levels(byy)))
                grid.lines(xtmp[[b]], ytmp[[b]], default.units = "native",
                           gp =
                           gpar(lwd = opts$lwd, lty = opts$lty,
                                col = col.args$f.cols[b]))
        }
    }
        
    upViewport()
    invisible(NULL)
}
