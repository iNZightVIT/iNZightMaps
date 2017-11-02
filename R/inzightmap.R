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
#' ## Region Data
#' library(gapminder)
#' data(gapminder)
#' map.world <- sf::st_as_sf(maps::map("world", ".", plot = FALSE, fill = TRUE))
#'
#' world.mapplot <- iNZightMapPlot(gapminder, map.world, type = "region",
#'                                 by.data = "country", by.map = "ID")
#'
#' plot(world.mapplot)
#'
#' ## Point Data
#' data(nzquakes)
#' map.nz <- sf::st_as_sf(maps::map("nz", ".", plot = FALSE, fill = TRUE))
#'
#' nzquake.mapplot <- iNZightMapPlot(nzquakes, map.nz, type = "point",
#'                                   coord = c("Longitude", "Latitude"))
#'
#' plot(nzquake.mapplot)

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

  .mapdata <- sf::st_as_sf(dplyr::left_join(.map, .data, by = by.vect))
  ## .mapdata <- .mapdata[!is.na(sf::st_dimension(.mapdata)), ]

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
  .datasf <- sf::st_transform(.datasf, crs = sf::st_crs(.map))

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

plot.iNZightMapPlot <- function(obj, facet = NULL) {
  to.plot <- Reduce(`+`,
                    x = obj$map.layers,
                    init = ggplot2::ggplot())

#   if(obj$type == "point") {
    to.plot <- Reduce(`+`,
                      x = obj$point.layers,
                      init = to.plot)
#    }

  if(!any(c(names(obj$map.layers), names(obj$point.layers)) == "coordlims") && !is.null(obj$crs)) {
    to.plot <- to.plot + ggplot2::coord_sf(crs = obj$crs)
  }

  if(!is.null(facet)) {
    if(facet == "_MULTI") {
      plot(to.plot)
    } else {
      plot.grob <- ggplot2::ggplotGrob(to.plot)
      
      panel.cols <- plot.grob$layout[grepl("^panel-", plot.grob$layout$name), c("l")] * (-1)
      spacer.rows <- which(plot.grob$layout$name == "spacer") * (-1)
      
      facet.var <- obj$facet.var
      
      col.to.keep <- which(levels(obj[[layersetHelper(obj$type)]]$baselayer[[1]]$data[[facet.var]]) == facet)
      panel.cols <- panel.cols[-col.to.keep]
      
      to.plot <- plot.grob[spacer.rows, panel.cols]
    }

  }

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


#' Fetch map from `maps` package
#'
#' @param map.name Which map to take
#' @param region Which regions inside that map to extract
#'
#' This function is a wrapper around the \link[maps]{map} function that creates an `sf' object from the map returned by that function.
#'
#' @return
#' @export
#'
#' @examples
fetchMap <- function(database = "world", region = ".", crs = 4326) {
  map.data <- sf::st_as_sf(maps::map(database, region, plot = FALSE, fill = TRUE))

  map.data <- sf::st_transform(map.data, crs = crs)

  map.data
}

#' Simplify Map Polygons
#'
#' @param map
#' @param level
#'
#' @return
#' @export
#'
#' @examples
#'
#' nzquake.mapplot <- iNZightMapPlot(nzquakes, map.nz, type = "point",
#' coord = c("Longitude", "Latitude"))
#' system.time(plot(nzquake.mapplot))
#'
#' map.nz2 <- simplifyMap(map.nz, level = "country")
#' nzquake.mapplot2 <- iNZightMapPlot(nzquakes, map.nz2, type = "point",
#' coord = c("Longitude", "Latitude"))
#'
#' system.time(plot(nzquake.mapplot2))

simplifyMap <- function(map, level) {
  sf::st_simplify(map, preserveTopology = TRUE, dTolerance = 0.05)
}
