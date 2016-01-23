##' Create an iNZightShapeMap object
##'
##' details ....
##'
##' @title Create an iNZight Shape Map Object
##' @param location the location
##' @param shp.region a character value, the column name in the region/country column of the shp file
##' @param data.region a character value, the column name in the region/country column of the data set
##' @param data the data set
##'
##' @examples
##' @import maptools tools
##' @export
iNZightShapeMap <- function(location,shp.region,data.region, data) {

    ## file checking
    if(!missing(location))
    {
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
        ext.read = c('rds','shp')
        if(!(ext %in% ext.read))
        {
            stop('location must be either shp or rds')
        }
    }
    ## order matching
    if (!missing(data))
    {
        if(missing(data.region))
            stop('require the column name of region in data set')
        
        order = order.match(out$region,data[,data.region])
        out$ordered = order        
    }else
        stop('data is missing!')

    class(out) <- c("inzightshapemap", class(out))
    out
}




plot.inzightshapemap <- function(x, variable, region, data,
                                 col.fun = "hue", transform = "linear",
                                 col.offset = 0.2, col = "red",na.fill = 'white',full.map = TRUE,
                                 ...) {

  ##  mc <- match.call(expand.dots = TRUE)
       
    call <- list()

    if (inherits(variable, "formula"))
    {
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
        transform = transform, 
        col.method = col.fun, 
        col.offset = col.offset,
        col = col,
        na.fill = na.fill,
        full.map = full.map
        )
    
    #call <- c(call, list(...))

    do.call("iNZightPlot", call)

    invisible(call)
}
