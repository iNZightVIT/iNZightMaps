iNZightMapPlot <- function(data, map, type, ...) {
  switch(type,
         region = iNZightMapPlotRegion(data, map, ...),
         point = iNZightMapPlotPoint(data, map, ...),
         stop("Invalid type argument"))
}

## Merges .data to .map using the arguments by.data, by.map
iNZightMapPlotRegion <- function(.data, .map, by.data, by.map) {
  by.vect <- c(by.data)
  names(by.vect) <- by.map
  
  .mapdata <- left_join(.map, .data, by = by.vect)
  
  map.layers <- list(baselayer = ggplot2::geom_sf(data = .mapdata))
  
  mapplot.obj <- list(map.layers = map.layers,
                      point.layers = NULL,
                      type = "region",
                      crs = sf::st_crs(.map))
  
  class(mapplot.obj) <- c("iNZightMapPlot", "list")
  
  mapplot.obj
}

## Stores both the background map and the points
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
