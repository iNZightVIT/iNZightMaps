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
iNZightShapeMap <- function(file.name,column.index,region, data) {

    ## extract
    if(!missing(file.name))
    {
    ext = file_ext(file.name)
        switch(ext,
            rds = 
            {
                out = readRDS(file.name)
            },
            
            shp = 
            {
                shp = readShapeSpatial(file.name)
                out = shape.extract(shp,column.index)   
            }
        
        )
    ext.read = c('rds','shp')
        if(!(ext %in% ext.read))
        {
            stop('file.name must be either shp or rds')
        }
    }
    
    if (!missing(data))
    {
        order = order.match(out$region,data[,region])
        out$ordered = order        
    }else
        stop('data is missing!')

    class(out) <- c("inzightshapemap", class(out))
    out
}




plot.inzightshapemap <- function(x, variable, region, data,
                                 col.fun = "hue", transform = "linear",
                                 col.offset = 0.2, col = "red",na.fill = 'white',
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
        na.fill = na.fill
        )
    
    #call <- c(call, list(...))

    do.call("iNZightPlot", call)

    invisible(call)
}