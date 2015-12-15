##' @title title
##' @param obj an object from within iNZightPlot
##' @return Object
##' @author Tom Elliott
##' @import iNZightPlots
##' @import grid
##' @export
create.inz.mapplot <- function(obj) {
    out <- NextMethod()

    out$draw.axes <- FALSE
    out$global.object <- list("map" = NULL)
    
    class(out) <- c("inzmap", class(out))
    
    out
}




##' draw a map by passing an iNZightPlot object
##'
##' the function will also returns a global object which called global.objects
##' @title Plot an iNZight Map 
##' @param obj object passed from iNZightPlot
##' @param gen other options passed from iNZightPlot
##' @return NULL
##' @author Jason Wen 
##' @import RgoogleMaps
##' @export
plot.inzmap <- function(obj, gen) {	
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args
    debug <- if (is.null(opts$debug)) FALSE else opts$debug
    
    ## create a global object
    if (!"global.objects" %in% ls(envir = .GlobalEnv))
	{
            assign("global.objects", list(), envir = .GlobalEnv)	
	}
    ## setting
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    
    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
    SCALE  <-  2
    size = global.objects$maps$map$size
    valid.maptypes = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")
    type <- if ("maptype" %in% names(opts$plot.features)) opts$plot.features$maptype else "roadmap"

    if (!type %in% valid.maptypes) 
	{
            type <- "roadmap"
            warning("Maptype was not valid, using roadmap")
	}

    get.newmap <- needNewMap(bbox = c(xlim,ylim),size = size,SCALE = SCALE,type = type,window = c(win.width,win.height))
    if (debug)
        message(paste('get.newmap:',get.newmap))
    
    if (get.newmap) 
	{
            if (debug) message(xlim)
            if (debug) message(ylim)
            getNewMap(xlim = xlim, ylim = ylim, SCALE = SCALE, type = type,zoom = Get.map.size(xlim,ylim)$zoom)
            ## updating
            global.objects$maps$map.detail$window = c(win.width,win.height)
            global.objects$maps$map.detail$bbox = c(xlim,ylim)
            global.objects$maps$map.detail$size = global.objects$maps$map$size
            global.objects$maps$map.detail$scale= global.objects$maps$map$SCALE
            global.objects$maps$map.detail$type = type
            assign("global.objects", global.objects, envir = .GlobalEnv)
            
	}

    ## drawing~~~~
    grid.raster(global.objects$maps$map$myTile,0.5,0.5,1,1)
    
    ## define the limit
    tmp = map.xylim()$window.lim
    xl =tmp[1:2]
    yl = tmp[3:4]
    
    ## setting the viewport
    vp = viewport(0.5,0.5,1,1,name = 'VP:PLOTlayout',xscale = xl, yscale = yl)
    pushViewport(vp)
    
    ## transform the points
    dd = cbind(obj$x,obj$y)
    point = latlon.xy(dd,map = global.objects$maps$map)

    ## other scatter plot things
    if (length(obj$x) == 0)
	return()
    
    ptCols <- iNZightPlots:::colourPoints(obj$colby, col.args, opts)
    NotInView <- obj$x < min(xlim) | obj$x > max(xlim) | obj$y < min(ylim) | obj$y > max(ylim)
    obj$pch[NotInView] <- NA
    grid.points(point[[1]], point[[2]], pch = obj$pch,
	gp =
            gpar(col = ptCols,
                 cex = obj$propsize,
                 lwd = opts$lwd.pt, alpha = opts$alpha,
                 fill = obj$fill.pt),
                name = "SCATTERPOINTS")
    
    invisible(NULL)
}




