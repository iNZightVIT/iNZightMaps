#' Constructing an iNZightMapPlot object
#'
#' @param data Data values for each coordinate or region.
#' @param map A map to use with the values in \code{data}.
#' @param type Either \code{"region"} or \code{"point"} depending on the data.
#' @param ... Any further required arguments for the type being constructed.
#'
#' @return An iNZightMapPlot object.
#' @export
#'
#' @examples
#' 

iNZightMapPlot <- function(data, map, type, ...) {
  switch(type,
         region = iNZightMapPlotRegion(data, map, ...),
         point = iNZightMapPlotPoint(data, map, ...),
         stop("Invalid type argument"))
}

#' @describeIn iNZightMapPlot Constructs a iNZightMapPlot using region values.

iNZightMapPlotRegion <- function(.data, .map, by.data, by.map) {
  by.vect <- c(by.data)
  names(by.vect) <- by.map
  
  .mapdata <- dplyr::left_join(.map, .data, by = by.vect)
  
  map.layers <- list(baselayer = ggplot2::geom_sf(data = .mapdata))
  
  mapplot.obj <- list(map.layers = map.layers,
                      point.layers = NULL,
                      type = "region",
                      crs = sf::st_crs(.map))
  
  class(mapplot.obj) <- c("iNZightMapPlot", "list")
  
  mapplot.obj
}

#' @describeIn iNZightMapPlot Constructs a iNZightMapPlot using point values.

iNZightMapPlotPoint <- function(.data, .map, coord = c("lon", "lat"), crs = 4326) {
  .datasf <- sf::st_as_sf(.data, coords = coord, crs = crs)
  
  map.layers <- list(baselayer = ggplot2::geom_sf(data = .map))
  point.layers <- list(baselayer = ggplot2::geom_sf(data = .datasf))
  
  mapplot.obj <- list(map.layers = map.layers,
                      point.layers = point.layers,
                      type = "point",
                      crs = sf::st_crs(.map))
  
  class(mapplot.obj) <- c("iNZightMapPlot", "list")
  
  mapplot.obj
}

#' Plot an iNZightMapPlot object
#'
#' @param obj An iNZightMapPlot object of any type.
#'
#' @export
#'
#' @examples
#' 

plot.iNZightMapPlot <- function(obj) {
  to.plot <- Reduce(`+`, 
                    x = obj$map.layers, 
                    init = ggplot2::ggplot())
  
#   if(obj$type == "point") {
    to.plot <- Reduce(`+`, 
                      x = obj$point.layers,
                      init = to.plot)
#   }
  
  to.plot <- to.plot + ggplot2::coord_sf(crs = obj$crs)
  
  plot(to.plot)
}

#' Summarise an iNZightMapPlot object
#'
#' @param obj An iNZightMapPlot object of any type.
#'
#' @export
#'
#' @examples
#' 

summary.iNZightMapPlot <- function(obj) {
  cat(paste0("iNZightMapPlot of type '", obj$type, "': \n"))
  
  cat(paste0("\tMap Layers   (", length(obj$map.layers), "): \n\t\t"))
  cat(paste0(names(obj$map.layers), collapse = "\n\t\t"))
  
  cat("\n")
  
  cat(paste0("\tPoint Layers (", length(obj$point.layers), "): \n\t\t"))
  cat(paste0(names(obj$point.layers), collapse = "\n\t\t"))
  
  cat("\n")
  
  cat(paste0("\tProjection: \n\t\t"))
  cat(obj$crs$proj4string)
  cat("\n")
}
