#' Transform the latitude and longitude into xy
#'
#' Wrapper function for \link{LatLon2XY} from 'ggmap' package
#' @title latlon.xy
#' @param lat latitude values to convert
#' @param lon longitude values to convert
#' @param zoom the level of zoom used in the map
#' @return a dataframe of x and y values
#' @author Tom Elliott
#' @export
latlon.xy <- function(lat, lon, zoom) {
    tmp <- ggmap::LonLat2XY(lon, lat, zoom)
    data.frame(x = tmp$X + tmp$x / 255, y = tmp$Y + tmp$y / 255)
}

#' Transform longitude in order to get the biggest map as possible
#'
#' @title Transform the longitude
#' @param lon a numeric value or a numeric vector of longitude
#' @return a new longitude value or vector that used for plotting in map.
#' @author Jason Wen
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


#' @title Zoom in/out
#' @param ... additional parameters
#' @return NULL
#' @details if ratio < 1 then zoom in, if ratio > 1 then zoom out, if ratio = 1 then shift the plot.
#' @author Jason
#' @export
ClickOnZoom <- function(...) {
    NULL
}

#' change the zoom within the center point
#'
#' @title change the zoom within the center point
#' @param zoom a numeric value between 0.1 to 0.9(minimum zoom to maximum zoom)
#' @return NULL
#' @author Jason
#' @export
rezoom <- function(zoom) {
    if (zoom > 2 | zoom < 0.1) {
        stop("invalid zoom")
    }
    ClickOnZoom(ratio = zoom, resize = TRUE)
}
