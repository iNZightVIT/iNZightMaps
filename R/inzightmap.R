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

iNZightMapPlotRegion <- function(data, map, by.data, by.map) {
  by.vect <- c(by.data)
  names(by.vect) <- by.map

  ## orig.data <- orig.data[!is.na(sf::st_dimension(orig.data)), ]

  mapdata <- sf::st_as_sf(dplyr::left_join(map, data, by = by.vect))

  map.layers <- list(baselayer = ggplot2::geom_sf(data = mapdata))

  map.centroids <- sf::st_centroid(mapdata)

  point.layers <- list(baselayer = ggplot2::geom_sf(data = map.centroids))

  mapplot.obj <- list(map.layers = map.layers,
                      point.layers = point.layers,
                      type = "region")

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

plot.iNZightMapPlot <- function(obj, fill.var = NULL, colour.var = NULL, size.var = NULL, alpha.var = NULL,
                                fill.const = NULL, colour.const = NULL, size.const = NULL, alpha.const = NULL,
                                facet = NULL, multiple.vars = FALSE,
                                main = NULL, xlab = "Longitude", ylab = "Latitude", axis.labels = TRUE,
                                datum.lines = TRUE, theme = NULL, projection = NULL) {
    if(multiple.vars) {
        args <- names(as.list(args(plot.iNZightMapPlot)))
        args <- args[-length(args)]
        args <- args[args != "fill.var"]
        args <- args[args != "multiple.vars"]
        args <- args[args != "main"]
        arg.call <- paste(args, sep = " = ", args, collapse = ", ")
        
        plots <- invisible(lapply(fill.var, function(x) {
            func.call <- paste0("plot(", arg.call,
                                ", fill.var = '", x, "'",
                                ", main = '", x, "', multiple.vars = FALSE)")
            eval(parse(text = func.call))
        }))
        
        plot.grid <- do.call(gridExtra::arrangeGrob, list(grobs = plots, nrow = 1, top = main))
        return(plot.grid)
    } else {
        obj[["map.layers"]][["baselayer"]] <- setMapping2.iNZightMapPlot(obj,
                                                                         "map", "baselayer",
                                                                         "fill", fill.var)
        
        obj[["map.layers"]][["map.title"]] <- ggplot2::labs(title = main)
        
        if(axis.labels) {
            obj[["map.layers"]][["map.axistitles"]] <- ggplot2::labs(x = xlab, y = ylab)
        }
        
        if(datum.lines) {
            obj[["map.layers"]][["map.projection"]] <- ggplot2::coord_sf(crs = projection)
        } else {
            obj[["map.layers"]][["map.projection"]] <- ggplot2::coord_sf(crs = projection, datum = NA)
        }
        
        to.plot <- Reduce(`+`,
                          x = obj$map.layers,
                          init = ggplot2::ggplot())
        
        if(obj$type == "point") {
            to.plot <- Reduce(`+`,
                              x = obj$point.layers,
                              init = to.plot)
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
        
        to.plot
    }
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

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param filename 
##' @return 
##' @export
##' @author 
retrieveMap <- function(filename) {
    if(grepl(".rds$", filename)) {
        readRDS(filename)
    } else {
        sf::st_read(filename)
    }
}
        
##' Find the pair of variables from the map and data that have the
##' highest number of matches. This function returns in form: (data
##' variable, map variable)
##' @export
findBestMatch <- function(data, map.data) {
    ## Eliminate variables that are associated with multiple regions
    ## in the map
    map.data <- map.data[, !(apply(map.data, 2, anyDuplicated))]
    
    ## Only count the number of unique matches (prevents situations
    ## where one particular matched region may have so many matches it
    ## obfuscates the fact that few other regions were matched with
    ## that variable)
    match.sums <- apply(data, 2, function(x) {
        apply(map.data, 2, function(y) {
            sum(y %in% unique(x))
        })
    })
    
    ## Find indices of the best matching pair and return
    best.match <- which(match.sums == max(match.sums), arr.ind = TRUE)[1,]
    best.match.vars <- c(colnames(match.sums)[best.match[2]], rownames(match.sums)[best.match[1]])
    best.match.vars
}
