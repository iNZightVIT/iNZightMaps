##' Transform the latitude and longitude into xy
##'
##' Wrapper function for \link{LatLon2XY} from 'ggmap' package
##' @title latlon.xy
##' @param lat latitude values to convert
##' @param lon longitude values to convert
##' @param zoom the level of zoom used in the map
##' @return a dataframe of x and y values
##' @author Tom Elliott
##' @export
latlon.xy <- function(lat, lon, zoom) {
    tmp <- ggmap::LonLat2XY(lon, lat, zoom)
    data.frame(x = tmp$X + tmp$x / 255, y = tmp$Y + tmp$y / 255)
}

##' Transform longitude in order to get the biggest map as possible
##'
##' @title Transform the longitude
##' @param lon a numeric value or a numeric vector of longitude
##' @return a new longitude value or vector that used for plotting in map.
##' @author Jason Wen
lon.rescale <- function(lon) {
    r1 <- range(lon, na.rm = TRUE)
    r2 <- range((360 + lon) %% 360, na.rm = TRUE)
    if (round(diff(r2) - diff(r1), 10) < 0) {
        lon <- (360 + lon) %% 360
    } else {
        r3 <- range(ifelse(lon > 180, lon - 360, lon), na.rm = TRUE)
        if (diff(r3) < diff(r1)) lon <- ifelse(lon > 180, lon - 360, lon)
    }
    lon
}


##' @title Zoom in/out
##' @param ... additional parameters
##' @return NULL
##' @details if ratio < 1 then zoom in, if ratio > 1 then zoom out, if ratio = 1 then shift the plot.
##' @author Jason
##' @export
ClickOnZoom <- function(...) { NULL }
# ClickOnZoom <- function(ratio = 1, resize = FALSE, p.center = global.objects$maps$pf$click.points) {
#     global.objects$maps$map.detail$num <<- global.objects$maps$map.detail$num + 1
#     xlim <- global.objects$maps$map.detail$bbox[1:2]
#     ylim <- global.objects$maps$map.detail$bbox[3:4]


#     if (resize == FALSE) {
#         pushViewport(viewport(0.5, unit(1, "char"), 1, unit(2, "char")))
#         grid::grid.rect(gp = gpar(fill = "red"))
#         grid::grid.text("Click a point on the map to zoom", x = 0.5, y = 0.5, default.units = "native",
#                         gp = gpar(col = "white"))
#         popViewport()
#         p.cen <- as.numeric(grid.locator())
#         p.center <- XY2LatLon(global.objects$maps$map, p.cen[1], p.cen[2])[2:1]
#     } else {
#         p.center <- global.objects$maps$pf$click.points
#         xlim <- global.objects$maps$pf$bbox.record[1:2]
#         ylim <- global.objects$maps$pf$bbox.record[3:4]
#     }

#     plot.lim <- global.objects$maps$map.detail$xylim
#     new.xlim <- rep(p.center[1],2) + c(-1,1) * diff(xlim) * ratio/ 2
#     new.ylim <- rep(p.center[2],2) + c(-1,1) * diff(ylim) * ratio/ 2

#     if (resize == FALSE)
#         global.objects$maps$pf$bbox.record <<- c(new.xlim,new.ylim)



#     global.objects$maps$map.detail$bbox <<- c(new.xlim,new.ylim)
#     global.objects$maps$pf$click.points <<- p.center

#     SCALE <- global.objects$maps$map.detail$scale
#     size <- global.objects$maps$map$size
#     type <- global.objects$maps$map.detail$type
#     cex <- global.objects$maps$pf$cex
#     col <- global.objects$maps$pf$col
#     lwd <- global.objects$maps$pf$lwd
#     alpha <- global.objects$maps$pf$alpha
#     fill <- global.objects$maps$pf$fill
#     opacity <- global.objects$maps$pf$opacity
#     pch <- global.objects$maps$pf$pch

#     getNewMap(lat.lim = new.ylim, lon.lim = new.xlim,
#               SCALE = SCALE, type = type,
#               zoom = Get.map.size(new.ylim, new.xlim)$zoom)

#     grid.raster(global.objects$maps$map$myTile, 0.5, 0.5, 1, 1)
#     tmp <- map.xylim(new.ylim, new.xlim, SCALE = SCALE)$window.lim
#     xl <- tmp[1:2]
#     yl <- tmp[3:4]
#     vp <- viewport(0.5, 0.5, 1, 1,name = 'VP:PLOTlayout', xscale = xl, yscale = yl)
#     pushViewport(vp)

#     y <- global.objects$maps$map.detail$points[,1]
#     x <- global.objects$maps$map.detail$points[,2]
#     dd <- cbind(y,x)
#     point <- latlon.xy(dd, map = global.objects$maps$map)
#     grid.points(point[[1]], point[[2]], pch = pch,
#                 gp =
#                     gpar(col = col,
#                          cex = cex,
#                          lwd = lwd, alpha = alpha * opacity,
#                          fill = fill),
#                 name = "SCATTERPOINTS")
#     invisible(NULL)
# }


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
