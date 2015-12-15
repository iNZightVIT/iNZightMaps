##' Create an iNZight Map Object and pass to the plot
##'
##' None.
##' @title Create an iNZight Map Object
##' @param lat a formula specifying the name oe the latitude variable in \code{data}
##' @param lon a formula specifying the name oe the longitude variable in \code{data}
##' @param data a data frame
##' @param ... exta arguments, not used
##' @return data frame object with class \code{inzightmap}
##' @author tell029
##' @export
iNZightMap <- function(lat, lon, data, name = deparse(substitute(data)), ...) {
    if (missing(data))
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

##' plot iNZight Map Object
##'
##' None.
##' @title Plot iNZight Map Object
##' @param x an object of class \code{inzightmap}
##' @param ... extra arguments sent to \code{iNZightPlot}
##' @return iNZightPlot object
##' @author Tom Elliott
##' @export
plot.inzightmap <- function(x, ...) {
    mc <- match.call()
    
    mc$data <- mc$x
    mc$x <- expression(.latitude)
    mc$y <- expression(.longitude)
    mc$plottype <- "map"

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
