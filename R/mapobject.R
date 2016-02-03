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
##' @examples
##' data(nzquakes)
##' mapobj <- iNZightMap(lat = ~Latitude, lon = ~Longitude, data = nzquakes)
##' plot(mapobj, opacity = ~Depth, colby = Felt, sizeby = Magnitude, type = "terrain")
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
##' @param type the type of map to download from Google
##' @param ... additional arguments passed to \code{iNZightPlot}
##' @describeIn iNZightMap Plot an \code{inzightmap} object
##' @export
plot.inzightmap <- function(x,
                            opacity,
                            type =
                                c("roadmap", "mobile", "satellite", "terrain", "hybrid",
                                  "mapmaker-roadmap", "mapmaker-hybrid"),
                            ...) {
    mc <- match.call()
    
    mc$data <- mc$x 
    mc$x <- expression(.longitude)
    mc$y <- expression(.latitude)
    mc$plottype <- "map"
    mc$largesample <- FALSE
    mc$plot.features <- list(maptype = match.arg(type))
    
    if (!missing(opacity)) {
        if (inherits(opacity, "formula")) {
            opacity <- as.character(opacity)[2]
        }
        mc$plot.features$opacity <- opacity
        mc$extra.vars <- opacity
        
    }

    ## set the plot labels:
    if (is.null(mc$main))
        mc$main <- paste("Map of", attr(x, "name"))
    if (is.null(mc$xlab))
        mc$xlab <- ""
    if (is.null(mc$ylab))
        mc$ylab <- ""

    mc[1] <- expression(iNZightPlot)

    eval(mc)
}

