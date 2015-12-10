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
	opts <- gen$opts
	mcex <- gen$mcex
	col.args <- gen$col.args

	#create a global object
	if (!"global.objects" %in% ls(envir = .GlobalEnv))
	{
		assign("global.objects", list(), envir = .GlobalEnv)	
	}
	###setting
	xlim <- current.viewport()$xscale
	ylim <- current.viewport()$yscale
	win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
	win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
	SCALE  <-  2
	size = global.objects$map$size
	valid.maptypes = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")
	type <- if ("maptype" %in% names(opts$plot.features)) opts$plot.features$maptype else "roadmap"

	if (!type %in% valid.maptypes) 
	{
		type <- "roadmap"
		warning("Maptype was not valid, using roadmap")
	}

	get.newmap <- needNewMap(bbox = c(xlim,ylim),size = size,SCALE = SCALE,type = type,window = c(win.width,win.height))
	print(paste('get.newmap:',get.newmap))
	if(get.newmap) 
	{
		getNewMap(xlim = xlim, ylim = ylim, SCALE = SCALE, type = type,zoom = Get.map.size()$zoom)
		##updating
		global.objects$map.detail$window = c(win.width,win.height)
		global.objects$map.detail$bbox = c(xlim,ylim)
		global.objects$map.detail$size = global.objects$map$size
		global.objects$map.detail$scale= global.objects$map$SCALE
		global.objects$map.detail$type = type
		assign("global.objects", global.objects, envir = .GlobalEnv)

	}


	
	###drawing~~~~
	grid.raster(global.objects$map$myTile,0.5,0.5,1,1)
	
	####define the limit
	tmp = in.maps.range()
	xl =tmp[1:2]
	yl = tmp[3:4]

	##setting the viewport
	vp = viewport(0.5,0.5,1,1,name = 'VP:PLOTlayout',xscale = xl, yscale = yl)
	pushViewport(vp)
	
	##transform the points
	dd = cbind(obj$y,obj$x)
	point = latlon.xy(dd,map = global.objects$map)

	
	
	##not my stuff~~~
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




