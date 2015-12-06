##' @title Create a map object to plot
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

##' @export
plot.inzmap <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args
    
    ## map detail setting
    valid.maptypes = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")
    maptype <- if ("maptype" %in% names(opts$plot.features)) opts$plot.features$maptype else "roadmap"
    if (!maptype %in% valid.maptypes) {
        maptype <- "roadmap"
        warning("Maptype was not valid, using roadmap")
    }

    ## do the same for these:
    SCALE = 2

    ## use viewport size to determine the pixel size
    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
    
    if (win.width > win.height)
        size <- round(c(640, 640 * win.height / win.width))
    else
        size <- round(c(640 * win.width / win.height, 640))

    ## save the map in the global environemnt to retain it across multiple plots
    get.newmap <- TRUE

    ## we need to qualify that the current map can be used:
    if ("global.objects" %in% ls(envir = .GlobalEnv)) {
        if (!is.null(global.objects$map)) {
            ## map exists - is it valid?
            get.newmap <- needNewMap(global.objects$map)#, xrange, yrange, scale, size, maptype, ...)
        }
    } else {
        assign("global.objects", list(map = NULL), envir = .GlobalEnv)
    }

    ## only get the map if we need it - from now, assume global.object$map is the correct map!
    if (get.newmap) getNewMap(xlim, ylim, SCALE, size, maptype)

    ## Now just draw it
    grid.raster(global.objects$map$myTile)

    

    
    return(NULL)
    
    if (is.null(maps)) 
	{
		window.new = c(0,0)
		##get the map
                ##map = GetMap.bbox(range(obj$x)+extra.range,range(obj$y)+extra.range,MINIMUMSIZE = TRUE,SCALE = SCALE,maptype = maptype,size = size)
                map <- GetMap.bbox(xlim, ylim, MINIMUMSIZE = TRUE, SCALE = SCALE,
                                   maptype = maptype, size = size)
		###put 0,0 into window size
		window.new = dev.size("cm")
		maps = list(map = map, window.new = window.new,window.odd = c(0,0))
		get.newmap = TRUE
			
		###default
		maps$window.odd = dev.size("cm")
		assign('maps', maps, envir = .GlobalEnv)
	    
	} else 
		{
		get.newmap = TRUE
		###if the plot window not equal, then get a new map
		if(any(maps$window.new != dev.size("cm")))
		{
                    tem.new = dev.size("cm")
                    maps$window.new = tem.new
                     map <- GetMap.bbox(xlim, ylim, MINIMUMSIZE = TRUE, SCALE = SCALE,
                                        maptype = maptype, size = size)
                    
###update
                    maps$map = map
                    assign('maps', maps, envir = .GlobalEnv)
		}
	}

    if (get.newmap) 
	{
            ##PlotOnStaticMap(maps$map,NEWMAP = TRUE,lat = data.1$Latitude,lon = data.1$Longitude)
		tmp = in.maps.range()
		grid.raster(maps$map$myTile,0.5,0.5, 1,1,interpolate = TRUE)
		###get window size
		window.size = dev.size("cm")
		xl = tmp[1:2]
		yl = tmp[3:4]
		
		vp = viewport(0.5,0.5, 0.99, 0.99,name="VP:PLOTlayout",xscale = xl, yscale = yl)
		pushViewport(vp)
		
		##transform the points
		dd = cbind(obj$y,obj$x)
		point = latlon.xy(dd,map = maps$map)
		
		##updating
		maps$usr = tmp
		assign('maps', maps, envir = .GlobalEnv)
	
    }        

        
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



needNewMap <- function(stuff, ...) {
    need.new <- FALSE
    

    ## check all attributes, if any fail
    ## need.new <- TRUE
    TRUE
}


getNewMap <- function(xlim, ylim, scale, size, type, ...) {
    ## Download a new map and save it into the global environment
    print(size)

    map <- xx <- GetMap.bbox(xlim, ylim, MINIMUMSIZE = TRUE, SCALE = scale,
                             maptype = type, size = size)
    assign("global.objects", list(map = map), envir = .GlobalEnv)

    print(xx$size)
    
    return(NULL)
}
