##' Transform the latitude and longitude into xy
##'
##' wrap function for \link{LatLon2XY} from 'RgoogleMaps' package
##' @title latlon.xy
##' @param latlon the data set that use for plotting, the first cloumn needs to be latitude and the second column needs to be longitude.
##' @param map a map object from RgoogleMaps package.
##' @return a list that contain the tramsformed x and y.
##' @author Jason Wen
##' @examples
##' golbal.objects = list()
##' r.latlon = cbind(runif(100,-90,90),runif(100,-90,90))
##' r.bbox = runif(4,-90,90)
##' getNewMap(r.bbox[1:2],r.bbox[3:4],2,zoom = 3)
##' latlon.xy(r.latlon,global.objects$maps$map)
##' @export
latlon.xy <- function(latlon,map) {
    zoom <- map$zoom
    LatLon2XY.centered(map, latlon[,1], latlon[,2], zoom = zoom)
}


####gat a new map object and assign to global environment
##' Get a new map and save it into Global Env
##'
##' a wrap function of \link{GetMap} from package 'RgoogleMaps'
##' @title Get an new map object
##' @param lat.lim the range of longitude
##' @param lon.lim the range of latitude
##' @param SCALE variable from \link{GetMap}, use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map
##' @param variable from \link{GetMap}, type defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile.
##' @param zoom variable from \link{GetMap}, Google maps zoom level.
##' @return a map object that assign as 'golbal.object'
##' @author Jason Wen
##' @examples
##' golbal.objects = list()
##' r.bbox = runif(4,-90,90)
##' getNewMap(r.bbox[1:2],r.bbox[3:4],2,zoom = 3)
getNewMap <- function(lat.lim, lon.lim, SCALE,
                      type = c("roadmap", "mobile", "satellite", "terrain", "hybrid",
                               "mapmaker-roadmap", "mapmaker-hybrid"), zoom) {
    type <- match.arg(type)
    lat.mean <- mean(lat.lim)
    lon.mean <- mean(lon.lim)
    center <- c(lat.mean,lon.mean)
    map <<- GetMap(center = center, size = Get.map.size(lat.lim, lon.lim)$size,
                   zoom = zoom, maptype = type, SCALE = SCALE)
    global.objects$maps$map = map
    assign("global.objects", global.objects, envir = .GlobalEnv)
}


####a simple tranformation for longitude
##' transform latitude inorder to get the biggest map as possibile
##'
##' transform the longitude
##' @title transform the latitude
##' @param lon a numeric value or a numeric vector of longitude
##' @return a new longitude value or vector that used for plotting in map.
##' @author Jason Wen
##' @examples
##' lon = runif(100,-360,360)
##' lon.rescale(lon)
lon.rescale <- function(lon) {
    r1 <- range(lon, na.rm = TRUE)
    r2 <- range((360 + lon) %% 360, na.rm = TRUE)
    if (diff(r2) < diff(r1)) {
        lon <- (360 + lon) %% 360
    } else {
        r3 <- range(ifelse(lon > 180, lon - 360, lon), na.rm = TRUE)
        if (diff(r3) < diff(r1)) lon <- ifelse(lon > 180, lon - 360, lon)
    }
    lon
}


####return TRUE if we something changed or firt plotting
####FALSE if nothing is changed
##' Do we need a new map or not
##'
##' A function for checking wheather we need to download a new map from google or not, it checks the input with the global inzight objects, if something if not matched, then return TRUE, otherwise FALSE.
##' @title Need New Map
##' @param bbox a numeric vector of length 4, the range of the pervious latitude and longitude
##' @param window a numeric vector of length 2, the size of the pervious window
##' @param sized a numeric vector of length 2, the size of the pervious map
##' @param SCALE a numeric vector of length 1, the scale of the pervious map
##' @param type a character vector of length 1, type the type of the pervious map
##' @return Logical value TRUE/FALSE TRUE = something are not matched, FALSE = the pervious map is ok for re-use.
##' @author Jason Wen
##' @examples
##' needNewMap()
needNewMap <- function(bbox, window, size, SCALE, type) {
    need <- FALSE
    map.odd <- global.objects$maps$map
    
    ##check for the map object
    if (any(map.odd == NULL)) {
        need <- TRUE
    } else {
        ## check if any null
        if ( is.null(global.objects$maps$map.detail$bbox) ||
             is.null(global.objects$maps$map.detail$size) ||
             is.null(global.objects$maps$map.detail$scale) ||
             is.null(global.objects$maps$map.detail$type) ||
             is.null(global.objects$maps$map.detail$window)) {
            need <- TRUE
        } else {
            ## individual checking
            a <- global.objects$maps$map.detail$bbox
            b <- bbox
            if (any(abs(a - b) != 0) ) {
                global.objects$maps$map.detail$bbox <- bbox
                need[1] <- TRUE
            } else {
                need[1] <- FALSE
            }

            if (any(global.objects$maps$map.detail$size != size)) {
                global.objects$maps$map.detail$size <- size
                need[2] <- TRUE
            } else {
                need[2] <- FALSE
            }
            
            if (global.objects$maps$map.detail$scale != SCALE) {
                global.objects$maps$map.detail$scale <- SCALE
                need[3] <- TRUE
            } else {
                need[3] <- FALSE
            }
            
            if (type != global.objects$maps$map.detail$type) {
                global.objects$maps$map.detail$type <- type
                need[4] <- TRUE
            } else {
                need[4] <- FALSE
            }

            if (any(global.objects$maps$map.detail$window != window)) {
                global.objects$maps$map.detail$window <- window
                need[5] <- TRUE
            } else {
                need[5] <- FALSE
            }
            
            if (global.objects$maps$map.detail$num > 1) {
                need[6] <- TRUE
            } else {
                need[6] <- FALSE
            }
        }

        any(need)
    }
}


###return the size of map that we request the map from google
###get the range of latitude/longitude, then transform it into resolution unit,
###then make it into the same ratio as the window's
###also make sure the size lie on the interval of [0,640]

##' compute the size and the zoom level that needs for request a new map.
##'
##' since the size of the map should not greater than 640, the size and the zoom needs to be transform before pass to the getNewMap function.
##' @title Get the size of the map in pixels
##' @param latR a numeric vector of length 2, the range of Latitude
##' @param lonR a numeric vector of length 2, the range of Longitude
##' @param SCALE variable from GetMap, use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map
##' @return a list that contain the size of the map(in pixels), and the zoom level
##' @author Jason Wen
##' @examples
##' r.center = runif(4,min = -90,max = 90)
##' SCALE = 2
##' Get.map.size(r.center[1:2],r.center[3:4],SCALE)
Get.map.size <- function(latR, lonR, SCALE) {
    if (missing(SCALE)) {
        SCALE <- global.objects$maps$map.detail$scale
    }
    
    win.size <- c(convertWidth(current.viewport()$width, "mm", TRUE),
                  convertHeight(current.viewport()$height, "mm", TRUE))

    ## give an origin size that helps to compute the zoom
    if (win.size[1] > win.size[2]) {
        size.1 <- round(c(640 ,640 * win.size[2] / win.size[1]))
    } else {
        size.1 <- round(c(640 * win.size[1] / win.size[2], 640))
    }

    lat.range <- range(latR)
    lon.range <- range(lonR)
    zoom <- min(MaxZoom(lat.range, lon.range, size.1))

    
    ll <- LatLon2XY(latR[1], lonR[1], zoom)
    ur <- LatLon2XY(latR[2], lonR[2], zoom)
    cr <- LatLon2XY(mean(latR), mean(lonR), zoom)
    ll.Rcoords <- Tile2R(ll, cr)
    ur.Rcoords <- Tile2R(ur, cr)
    size <- 0
    size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X))))
    size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y))))
    
    ## first get a square with the maximum length
    size <- c(max(size), max(size))
    
    ## transform the size ratio to be the same as window.size's ratio
    if (win.size[1] > win.size[2]) {
        size[1] <- round(size[2] * (win.size[1] / win.size[2]), 0)
        size[2] <- size[2]
    } else {
        size[2] <- round(size[1] * (win.size[2] / win.size[1]), 0)
        size[1] <- size[1]
    }

    ## rearrange the ratio if any > 640
    size.final <- size

    if (size[1] > size[2]) {
        size.final <- round(c(640, 640 * win.size[2] / win.size[1]))
    } else {
        size.final <- round(c(640 * win.size[1] / win.size[2], 640))
    }

    ZoomSize <- list(zoom = zoom, size = pmin(640, size.final))
    ## hence the we will get the map with this zoom and size
    ZoomSize
}

##' return the limit of x-axis and y-axis of the plot.
##'
##' the limit is not the same as latitude/longitude, it is the limit of the plot window
##' @param latR a numeric vector of length 2
##' @param lonR a numeric vector of length 2
##' @param SCALE a numeric value
##' @title get the limit of x-axis and y-axis
##' @return return a numeric of length 4 that contain the limit of x-axis and y-axis
##' @author Jason Wen
##' @examples
##' r.center = runif(4,min = 0,max = 90)
##' r.center = runif(4,min = -90,max = 90)
##' SCALE = 2
##' map.xylim(r.center[1:2],r.center[3:4],SCALE)
map.xylim <- function(latR, lonR, SCALE) {
    ZoomSize <- Get.map.size(latR = latR, lonR = lonR, SCALE = SCALE)
    scale <- global.objects$maps$map$SCALE * 2
    size <- global.objects$maps$map$size
    offset <- 1
    window.xlim <- c(-size[1] + offset, size[1] - offset) / (scale)
    window.ylim <- c(-size[2] + offset, size[2] - offset) / (scale)
    window.lim <- c(window.xlim, window.ylim)
    global.objects$maps$map.detail$xylim <- window.lim
    
    assign("global.objects", global.objects, envir = .GlobalEnv)
    list(window.lim = window.lim)
}



##' The coordinates should be within on the same range of Google Map's. i.e. longitude = [-180,180], latitude = [-90,90]
##' @param lat a numeric vector
##' @param lon a numeric vector
##' @return return an logical value that tells wheater the coordinates are all within the same range of google map's or not.
##' @title Is Google Map?
##' @author Jason Wen
##' @examples
##' lat = runif(100,min = -180,max = 180)
##' lon = runif(100,min = -180,max = 180)
##' is.google.map(lat,lon)
is.google.map <- function(lat,lon) {
    TRUE
    
    if (any(lat > 90)  || any(lat < -90)) {
        warning('latitudes are not in the range of [-90,90]')
        FALSE
    }
    
    if(any(lon > 180)  || any(lon < -180)) {
        warning('longitudes are not in the range of [-180,180]')
        FALSE
    }
}

##' Zoom in/out when click the plot
##'
##' @title Zoom in/out
##' @param ratio a numeric value, define the ratio of zomm in or out
##' @return NULL
##' @details if ratio < 1 then zoom in, if ratio > 1 then zoom out, if ratio = 1 then shift the plot.
##' @author Jason
##' @examples
##' data("nzquakes")
##' iNZightPlot(Longitude,Latitude,data = nzquakes,colby = Depth, plottype = 'map',plot.features = list(maptype = 'roadmap'))
##' ClickOnZoom(ratio = 1)
##' @export
ClickOnZoom <- function(ratio = 1, resize = FALSE, p.center = global.objects$maps$pf$click.points) {
    global.objects$maps$map.detail$num <<- global.objects$maps$map.detail$num + 1
    xlim <- global.objects$maps$map.detail$bbox[1:2]
    ylim <- global.objects$maps$map.detail$bbox[3:4]


    if (resize == FALSE) {
        pushViewport(viewport(0.5, unit(1, "char"), 1, unit(2, "char")))
        grid::grid.rect(gp = gpar(fill = "red"))
        grid::grid.text("Click a point on the map to zoom", x = 0.5, y = 0.5, default.units = "native",
                        gp = gpar(col = "white"))
        popViewport()
        p.cen <- as.numeric(grid.locator())
        p.center <- XY2LatLon(global.objects$maps$map, p.cen[1], p.cen[2])[2:1]
    } else {
        p.center <- global.objects$maps$pf$click.points
        xlim <- global.objects$maps$pf$bbox.record[1:2]
        ylim <- global.objects$maps$pf$bbox.record[3:4]
    }

    plot.lim <- global.objects$maps$map.detail$xylim
    new.xlim <- rep(p.center[1],2) + c(-1,1) * diff(xlim) * ratio/ 2
    new.ylim <- rep(p.center[2],2) + c(-1,1) * diff(ylim) * ratio/ 2

    if (resize == FALSE)
        global.objects$maps$pf$bbox.record <<- c(new.xlim,new.ylim)

    
    
    global.objects$maps$map.detail$bbox <<- c(new.xlim,new.ylim)
    global.objects$maps$pf$click.points <<- p.center
    
    SCALE <- global.objects$maps$map.detail$scale
    size <- global.objects$maps$map$size
    type <- global.objects$maps$map.detail$type
    cex <- global.objects$maps$pf$cex
    col <- global.objects$maps$pf$col
    lwd <- global.objects$maps$pf$lwd
    alpha <- global.objects$maps$pf$alpha
    fill <- global.objects$maps$pf$fill
    opacity <- global.objects$maps$pf$opacity
    pch <- global.objects$maps$pf$pch

    getNewMap(lat.lim = new.ylim, lon.lim = new.xlim,
              SCALE = SCALE, type = type,
              zoom = Get.map.size(new.ylim, new.xlim)$zoom)

    grid.raster(global.objects$maps$map$myTile, 0.5, 0.5, 1, 1)
    tmp <- map.xylim(new.ylim, new.xlim, SCALE = SCALE)$window.lim
    xl <- tmp[1:2]
    yl <- tmp[3:4]
    vp <- viewport(0.5, 0.5, 1, 1,name = 'VP:PLOTlayout', xscale = xl, yscale = yl)
    pushViewport(vp)

    y <- global.objects$maps$map.detail$points[,1]
    x <- global.objects$maps$map.detail$points[,2]
    dd <- cbind(y,x)
    point <- latlon.xy(dd, map = global.objects$maps$map)
    grid.points(point[[1]], point[[2]], pch = pch,
                gp =
                    gpar(col = col,
                         cex = cex,
                         lwd = lwd, alpha = alpha * opacity,
                         fill = fill),
                name = "SCATTERPOINTS")
    invisible(NULL)
}


##' change the zoom within the center point
##'
##' @title change the zoom within the center point
##' @param zoom a numeric value between 0.1 to 0.9(minimum zoom to maximum zoom)
##' @return NULL
##' @author Jason
##' @export
rezoom <- function(zoom) {
    if(zoom > 2| zoom < 0.1)
        stop('invalid zoom')
    ClickOnZoom(ratio = zoom, resize = TRUE)
}
