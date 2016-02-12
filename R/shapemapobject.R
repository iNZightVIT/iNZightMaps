##' Create an iNZightShapeMap object
##'
##' details ....
##'
##' @title Create an iNZight Shape Map Object
##' @param location the shape file
##' @param shp.region a character value, the column name in the region/country column of the shp file
##' @param data.region a character value, the column name in the region/country column of the data set
##' @param data the data set
##' @return an iNZight Shape Map Object
##' @author Tom Elliott
##' @import maptools tools
##' @export
iNZightShapeMap <- function(location,shp.region,data.region, data) {

    if (location == "world") {
      out <- world
    } else if (!missing(location))
    { ## file checking
        ext = file_ext(location)
        switch(ext,
            rds =
            {
                out = readRDS(location)
            },

            shp =
            {
                shp = readShapeSpatial(location)
                out = shape.extract(shp,shp.region)
            }

        )
        ext.read = c('RDS','SHP')
        if(!(toupper(ext) %in% ext.read))
        {
            stop('location must be either shp or rds')
        }
    }
    ## order matching
    if (missing(data)) stop("Data is missing")

    if (missing(data.region))
            stop('require the column name of region in data set')
            
    ## data checking
    iso3c = countrycode(data[,data.region],'country.name','iso3c')
    a = table(iso3c)
    mul.region = names(which(a > 1))
    rows = rownames(data[iso3c %in% mul.region,])
    de.rows = as.numeric(rows[length(rows)])
    data = data[-de.rows,]
    
    sd = name.match(data[,data.region],out$region)
    out$ordered = order.match(sd[[1]],sd[[2]])
    out$bbox = c(range(out$latlon[,1]),range(out$latlon[,2]))
    bar.obj <<- NULL

    out$data = data
    out$region.name = data.region

    class(out) <- c("inzightshapemap", class(out))
    out
}



##' Create an iNZightShapeMap object
##'
##' details ....
##'
##' @title Create an iNZight Shape Map Object
##' @param x the iNZight Shape Map Object
##' @param variablie the variable or the column name in the data
##' @param region a variable or the column name of the region column
##' @param data the data set
##' @param col.fun a character value
##' @param full.map logical value.
##' @return NULL
##' @author Tom Elliott
##' @import maptools
##' @export
plot.inzightshapemap <- function(x, variable,
                                 col.fun = "hue", transform = "linear",
                                 col.offset = 0.2, col = "red",na.fill = '#F4A460',
                                 full.map = TRUE,extend.ratio = 1,name = FALSE,
                                 ...) {

  ##  mc <- match.call(expand.dots = TRUE)

    call <- list()

    data <- x$data

    if (inherits(variable, "formula"))
    {
        mf <- substitute(model.frame(variable, data = data, na.action = NULL))
        call$x <- eval.parent(mf)[[1]]
    } else {
        call$x <- data.frame(variable)[[1]]
    }

    #if (inherits(region, "formula")) {
    #    mf <- substitute(model.frame(region, data = data, na.action = NULL))
    #    call$y <- eval.parent(mf)[[1]]
    #} else {
    call$y <- data[, x$region.name]  #data.frame(region)[[1]]
    #}
    call$xlab <- ""
    call$ylab <- ""

    ## set variable names:
    call$varnames <- list(x = as.character(variable)[2],
                          y = as.character(variable)[2])

    call$data <- data
    call$plottype <- "shapemap"
    call$plot.features <- list(
        shape.object = x,
        transform = transform,
        col.method = col.fun,
        col.offset = col.offset,
        col = col,
        na.fill = na.fill,
        full.map = full.map,
        extend.ratio = extend.ratio,
        name = name
        )

    #call <- c(call, list(...))

    do.call("iNZightPlot", call)

    invisible(call)
}
