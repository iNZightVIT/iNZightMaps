##' description ...
##'
##' details ....
##'
##' @title Create an iNZight Shape Map Object
##' @param location the location
##'
##' @examples
##'
##' @export
iNZightShapeMap <- function(location, region, data) {

    ## extract
  #  if (is.character(location))
  #      out <- shape.extract(shp <- readShapePoly(location))
  #  else
    out <- getShapeFile(location)

    if (!missing(data)) {
        ## match if data supplied
        
    }

    class(out) <- c("inzightshapemap", class(out))
    out  ## "shape.obj" from "shape.extract()"
}




plot.inzightshapemap <- function(x, variable, region, data,
                                 col.fun = "terrain", transform = "linear",
                                 col.offset = 0.2, col = "blue",
                                 ...) {

  ##  mc <- match.call(expand.dots = TRUE)
        
    call <- list()

    if (inherits(variable, "formula")) {
        mf <- substitute(model.frame(variable, data = data, na.action = NULL))
        call$x <- eval.parent(mf)[[1]]
    } else {
        call$x <- data.frame(variable)[[1]]
    }

    if (inherits(region, "formula")) {
        mf <- substitute(model.frame(region, data = data, na.action = NULL))
        call$y <- eval.parent(mf)[[1]]
    } else {
        call$y <- data.frame(region)[[1]]
    }

    ## set variable names:
    call$varnames <- list(x = as.character(variable)[2],
                          y = as.character(variable)[2])

    call$data <- data
    call$plottype <- "shapemap"
    call$plot.features <- list(
        shape.object = x,
        transform = transform, col.method = col.fun, col.offset = col.offset,
        col = col
        )

    
    #call <- c(call, list(...))

    do.call("iNZightPlot", call)

    invisible(call)
}
