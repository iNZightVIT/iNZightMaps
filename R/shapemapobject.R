##' Create an iNZightShapeMap object
##'
##' details ....
##'
##' @title Create an iNZight Shape Map Object
##' @param location the shape file
##' @param shp.region a character value, the column name in the region/country column of the shp file
##' @param data.region a character value, the column name in the region/country column of the data set
##' @param data the data set
##' @param the variable that display in the map
##' @return an iNZight Shape Map Object
##' @author Tom Elliott
##' @import maptools tools
##' @export
iNZightShapeMap <- function(location,shp.region,data.region,data) {

    if (location == "world") {
      out <- world
    } else if (!missing(location))
    { 
		## file checking
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
                                 full.map = TRUE,extend.ratio = 1,name = FALSE,zoom = 1,zoom.center = c(NA,NA),
                                 ...) {
	
    call <- list()

    data <- x$data

    if (inherits(variable, "formula"))
    {
        mf <- substitute(model.frame(variable, data = data, na.action = NULL))
        call$x <- eval.parent(mf)[[1]]
    } else {
        call$x <- data.frame(variable)[[1]]
    }
    
    ## variable range ...
    data.range = range(call$x,na.rm = TRUE)
	print(data.range)
    x$maths$range = data.range
    x$maths$mean = mean(call$x,na.rm = TRUE)
    x$maths$sd = sd(call$x,na.rm = TRUE)
    x$maths$prob = max(dnorm((call$x-x$maths$mean)/x$maths$sd,0,1),na.rm = TRUE)
    
    call$y <- data[, x$region.name]

    call$xlab <- ""
    call$ylab <- ""

    ## set variable names:
    call$varnames <- list(x = as.character(variable)[2],
                          y = x$region.name)

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
        name = name,
		zoom = zoom,
		zoom.center = zoom.center
        )

    dots <- list(...)
    if ("g1" %in% names(dots)) {
        
        if (is.character(dots$g1)) {
            call$varnames$g1 <- dots$g1
            dots$g1 <- data[[dots$g1]]
        } else {
            #warning("Please specify g1 as a character name.")
            #dots$g2 <- NULL
        }
        
    }
    if ("g2" %in% names(dots)) {
        
        if (is.character(dots$g2)) {
            call$varnames$g2 <- dots$g2
            dots$g2 <- data[[dots$g2]]
        } else {
            #warning("Please specify g2 as a character name.")
            #dots$g2 <- NULL
        }
        
    }
    if ("varnames" %in% names(dots)) {
        ## Use the user-specified names over autogen ones
        call$varnames <- modifyList(call$varnames, dots$varnames)
        dots$varnames <- NULL
    }

    call <- c(call, dots)

    do.call("iNZightPlot", call)

    invisible(call)
}
