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
    if (!"global.objects" %in% ls(envir = .GlobalEnv)) {
        assign("global.objects", list(), envir = .GlobalEnv)
    }

    features <- obj$opts$plot.features

    map.type <- obj$opts$plot.features$maptype
    opts <- obj$opts

    ##  it's a "scatter" plot ...
    out <- NextMethod()
    out$map.type <- map.type

    ## here I do the 'shift' data, it should be done 'before-this-function' call
    ## should be re-write in the future
    # is.google.map(out$y,out$x)

    out$x <- lon.rescale(out$x)
    out$xlim <- range(out$x)

    ## sort out opacity
    if (!is.null(features$opacity)) {
        ## Move the computation to: function_def - plot.inzightmap; function_eval - iNZightPlots
        out$opacity <- obj$df[[features$opacity]]
        if (any(out$opacity < 1)) {
            out$pch <- rep(19, length(out$pch))
        }
    }

    class(out) <- c("inzmap", class(out))

    out$draw.axes <- FALSE
    out
}


##' draw a map by passing an iNZightPlot object
##'
##' the function will also returns a global object which called global.objects
##' @title Plot an iNZight Map
##' @param x object passed from iNZightPlot
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
    zoom <- opts$plot.features$mapzoom

    if (is.null(obj$opacity)) {
        opacity <- 1
    } else {
        opacity <- obj$opacity
    }

    grid.rect(gp = gpar(fill = "black"))

    debug <- if (is.null(opts$debug)) FALSE else opts$debug

    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    if (zoom < 0) {
        zoom <- ggmap::calc_zoom(xlim, ylim)
    }

    ## the SIZE of the current viewport
    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
    # SCALE  <-  2
    # size <- global.objects$maps$map$size
    type <- obj$map.type

    bbox <- c(xlim[1], ylim[1], xlim[2], ylim[2])
    bgmap <- getMap(
        bbox = bbox, zoom = zoom, size = c(win.width, win.height),
        type = opts$plot.features$maptype
    )

    # get.newmap <- needNewMap(bbox = c(xlim, ylim), size = size, SCALE = SCALE,
    #                          type = type, window = c(win.width, win.height))


    ## need to come up with a better way of doing this!!
    # if (get.newmap) {
    #     getNewMap(lat.lim = ylim, lon.lim = xlim, SCALE = SCALE, type = type,
    #               zoom = Get.map.size(ylim, xlim)$zoom)
    #     ## updating
    #     global.objects$maps$map.detail$window <<- c(win.width, win.height)
    #     global.objects$maps$map.detail$bbox <<- c(xlim, ylim)
    #     global.objects$maps$map.detail$size <<- global.objects$maps$map$size
    #     global.objects$maps$map.detail$scale <<- global.objects$maps$map$SCALE
    #     global.objects$maps$map.detail$type <<- type
    #     global.objects$maps$map.detail$points <<- cbind(obj$y, obj$x)
    # }

    ptCols <- iNZightPlots:::colourPoints(obj$colby, col.args, opts)
    # ## passing the details inorder to redraw.
    # global.objects$maps$pf$cex <<- obj$propsize
    # global.objects$maps$pf$col <<- ptCols
    # global.objects$maps$pf$lwd <<- opts$lwd.pt
    # global.objects$maps$pf$alpha <<- opts$alpha * opacity
    # global.objects$maps$pf$fill <<- obj$fill.pt
    # global.objects$maps$pf$opacity <<- opacity
    # global.objects$maps$pf$pch <<- obj$pch
    # global.objects$maps$map.detail$num <<- 1
    # global.objects$maps$pf$click.points <<- c(mean(xlim),mean(ylim))
    # global.objects$maps$pf$bbox.record <<- c(xlim,ylim)

    ## drawing~~~~
    mapGrob <- rasterGrob(bgmap, name = "background.map")
    grid.draw(mapGrob)

    ## define the limit
    # tmp <- map.xylim(ylim,xlim,SCALE = SCALE)$window.lim
    # xl <- tmp[1:2]
    # yl <- tmp[3:4]

    lims <- convertLimits(attr(bgmap, "bbox"), zoom)

    ## setting the viewport
    vp <- viewport(
        height = grobHeight(mapGrob),
        width = grobWidth(mapGrob),
        name = "VP:PLOTlayout",
        xscale = lims$x, yscale = rev(lims$y)
    )
    pushViewport(vp)
    # grid.rect(gp = gpar(fill = "transparent"))

    ## transform the points
    dd <- cbind(obj$y, obj$x)
    point <- latlon.xy(obj$y, obj$x, zoom)

    ## other scatter plot things
    if (length(obj$x) == 0) {
        return()
    }

    NotInView <- obj$x < min(xlim) | obj$x > max(xlim) | obj$y < min(ylim) | obj$y > max(ylim)
    obj$pch[NotInView] <- NA
    grid.points(point$x, point$y,
        pch = obj$pch,
        gp =
            gpar(
                col = ptCols,
                cex = obj$propsize,
                lwd = opts$lwd.pt, alpha = opts$alpha * opacity,
                fill = if (obj$fill.pt == "fill") ptCols else obj$fill.pt
            ),
        name = "SCATTERPOINTS"
    )


    ## Connect by dots if they want it ...
    if (opts$join) {
        if (length(unique(obj$colby)) <= 1 | !opts$lines.by) {
            grid.lines(point$x, point$y,
                default.units = "native",
                gp =
                    gpar(
                        lwd = opts$lwd, lty = opts$lty,
                        col = opts$col.line
                    )
            )
        } else {
            byy <- as.factor(obj$colby) # pseudo-by-variable
            xtmp <- lapply(levels(byy), function(c) subset(point$x, obj$colby == c))
            ytmp <- lapply(levels(byy), function(c) subset(point$y, obj$colby == c))

            for (b in 1:length(levels(byy))) {
                grid.lines(xtmp[[b]], ytmp[[b]],
                    default.units = "native",
                    gp =
                        gpar(
                            lwd = opts$lwd, lty = opts$lty,
                            col = col.args$f.cols[b]
                        )
                )
            }
        }
    }

    upViewport()
    invisible(NULL)
}


convertLimits <- function(bbox, zoom) {
    lons <- bbox[c(1, 3)]
    lats <- bbox[c(2, 4)]
    corners <- expand.grid(lon = lons, lat = lats)
    tile.bounds <- ggmap::LonLat2XY(corners$lon, corners$lat, zoom = zoom)
    xlim <- range(tile.bounds$X + tile.bounds$x / 255)
    ylim <- range(tile.bounds$Y + tile.bounds$y / 255)
    list(x = xlim, y = ylim)
}

getMap <- function(bbox, zoom, size, type) {
    ## get the initial map
    map <- getStamenMap(bbox, zoom, type)
    ## adjust bbox to fill as much space as possible
    mapGrob <- rasterGrob(map, name = "temporary.map")
    grid.draw(mapGrob)

    pr <- size[1] / size[2]
    bbox <- adjustBbox(bbox, pr, zoom)

    ## clean up
    grid.remove(gPath("temporary.map"))

    ## fetch new map with better dimensions
    map <- getStamenMap(bbox, zoom, type)
    attr(map, "bbox") <- bbox
    map
}

adjustBbox <- function(bbox, ratio, zoom) {
    lons <- bbox[c(1, 3)]
    lats <- bbox[c(2, 4)]
    corners <- expand.grid(lon = lons, lat = lats)
    tile.bounds <- ggmap::LonLat2XY(corners$lon, corners$lat, zoom = zoom)

    tile.y <- tile.bounds$Y + tile.bounds$y / 255
    tile.x <- tile.bounds$X + tile.bounds$x / 255
    tile.h <- diff(range(tile.y))
    tile.w <- diff(range(tile.x))

    if (tile.w / tile.h < ratio) {
        tile.w <- tile.h * ratio
        tile.x <- mean(tile.x) + c(-1, 1) * tile.w / 2
        tile.bounds$X <- floor(tile.x)
        tile.bounds$x <- (tile.x - floor(tile.x)) * 255
    } else {
        tile.h <- tile.w / ratio
        tile.y <- mean(tile.y) + c(-1, 1) * tile.h / 2
        tile.bounds$Y <- floor(tile.y)
        tile.bounds$y <- (tile.y - floor(tile.y)) * 255
    }

    newcorners <-
        do.call(rbind, apply(tile.bounds, 1, function(x) {
            ggmap::XY2LonLat(x[1], x[2], zoom, x[3], x[4])
        }))

    bbox <- c(t(apply(newcorners, 2, range)))
    bbox
}

getStamenMap <- function(bbox, zoom, type) {
    ## stamen maps don't like it when you ask for a bounding
    ## box that crosses over (-180,180) longitude,
    ## so we need to fetch two maps and stitch them together
    if (bbox[1] < -180) {
        ## need to test this
        bbox[c(1, 3)] <- bbox[c(1, 3)] + 360
    }
    # bbox.right <- bbox.left <- bbox
    # bbox.left[3] <- 179.99999
    # bbox.right[1] <- -180
    # bbox.right[3] <- bbox.right[3] - 360
    # map.left <- ggmap::get_stamenmap(bbox.left, zoom = zoom, maptype = type)
    # map.right <- ggmap::get_stamenmap(bbox.right, zoom = zoom, maptype = type)
    # map <- grDevices::as.raster(cbind(as.matrix(map.left), as.matrix(map.right)))
    if (bbox[3] > 180) {
        bbox.right <- bbox.left <- bbox
        bbox.left[3] <- 179.99999
        bbox.right[1] <- -180
        bbox.right[3] <- bbox.right[3] - 360
        map.left <- ggmap::get_stamenmap(bbox.left, zoom = zoom, maptype = type)
        map.right <- ggmap::get_stamenmap(bbox.right, zoom = zoom, maptype = type)
        map <- grDevices::as.raster(cbind(as.matrix(map.left), as.matrix(map.right)))
    } else {
        map <- ggmap::get_stamenmap(bbox, zoom = zoom, maptype = type)
    }
    map
}
