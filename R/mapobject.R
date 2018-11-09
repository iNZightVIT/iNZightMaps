##' Create an iNZight Map Object and pass to the plot
##'
##' The plot will download a map that contains all of the points. The size is limited by
##' the fixed zoom levels provided by Google's Static Maps API.
##'
##' @title Create an iNZight Map Object
##' @param lat a formula specifying the name of the latitude variable in \code{data}
##' @param lon a formula specifying the name of the longitude variable in \code{data}
##' @param data a data frame
##' @param name the name of the data set
##' @return data frame object with class \code{inzightmap}
##' @author Tom Elliott
##'
##' @export
iNZightMap <- function(lat, lon, data, name = deparse(substitute(data))) {
    if (missing(data) || is.null(data))
        stop("iNZightMaps required you to use a data.frame.")

    attr(data, "name") <- name

    ## Get latitude values
    if (inherits(lat, "formula")) {
        mf <- substitute(model.frame(lat, data = data))
        lat <- eval.parent(mf)
    } else {
        lat <- data.frame(lat)
    }

    ## Get longitude values
    if (inherits(lon, "formula")) {
        mf <- substitute(model.frame(lon, data = data))
        lon <- eval.parent(mf)
    } else {
        lon <- data.frame(lon)
    }

    data$.latitude <- lat[[1]]
    data$.longitude <- lon[[1]]

    class(data) <- c("inzightmap", class(data))
    data
}



##' @param x an \code{inzightmap} object
##' @param opacity character or expression of the variable name to code point opacity by
##' @param map.zoom the level of zoom to use; this should depend on the scales unless specified
##' @param type the type of map to download from Google
##' @param ... additional arguments passed to \code{iNZightPlot}
##' @param env Environment to evaluate \code{iNZightPlot} in
##' @describeIn iNZightMap Plot an \code{inzightmap} object
##' @export
plot.inzightmap <- function(x,
                            opacity,
                            map.zoom = 5,
                            type =
                                c("terrain", "terrain-background", "terrain-labels", "terrain-lines", 
                                  "toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                                  "toner-labels", "toner-lines", "toner-lite", "watercolor"),
                            ..., env = parent.frame()) {
    mc <- match.call()

    mc$data <- mc$x
    mc$x <- expression(.longitude)
    mc$y <- expression(.latitude)
    mc$plottype <- "map"
    mc$largesample <- FALSE
    mc$plot.features <- list(maptype = match.arg(type), mapzoom = map.zoom)

    if (!missing(opacity)) {
        if (inherits(opacity, "formula")) {
            opacity <- as.character(opacity)[2]
        }
        mc$plot.features$opacity <- opacity
        mc$extra.vars <- opacity
        attr(mc$extra.vars, "fun") <- structure(list(
            function(x, r = 0.2) {
                ## value between min and max -> 0 and 1 -> 0.1 and 1 (* overall opacity)
                (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) * (1 - r) + r
            }), .Names = opacity)
    }

    ## set the plot labels:
    if (is.null(mc$main))
        mc$main <- paste("Map of", attr(x, "name"))
    if (is.null(mc$xlab))
        mc$xlab <- ""
    if (is.null(mc$ylab))
        mc$ylab <- ""



    mc[1] <- expression(iNZightPlots::iNZightPlot)
    eval(mc, envir = env)
}
