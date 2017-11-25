iNZightMapPlot <- function(data, map, type, ...) {
  switch(type,
         region = iNZightMapPlotRegion(data, map, ...),
         ## point = iNZightMapPlotPoint(data, map, ...),
         stop("Invalid type argument"))
}

#' @describeIn iNZightMapPlot Constructs a iNZightMapPlot using region values.

iNZightMapPlotRegion <- function(data, map, by.data, by.map, simplification.level = 0.01) {
  by.vect <- c(by.data)
  names(by.vect) <- by.map

  map <- map[!is.na(sf::st_dimension(map)), ]

  mapdata <- sf::st_as_sf(dplyr::left_join(map, data, by = by.vect))
  map.centroids <- sf::st_centroid(mapdata)

  mapdata <- sf::st_simplify(mapdata, dTolerance = simplification.level)

  has.multipleobs <- isTRUE(any(table(data[, by.data]) > 1))

  mapplot.obj <- list(region.data = mapdata,
                      centroid.data = map.centroids,
                      type = "region",
                      projection = "",
                      code.history = "TODO...",
                      region.var = by.map,
                      multiple.obs = has.multipleobs) 
  class(mapplot.obj) <- c("iNZightMapPlot", "list")

  mapplot.obj
}

iNZightMapVars <- function(obj) {
    colnames(obj$region.data)[-ncol(obj$region.data)]
}

plot.iNZightMapPlot <- function(obj, colour.var = NULL, size.var = NULL, alpha.var = NULL,
                                fill.const = NULL, colour.const = NULL, size.const = NULL, alpha.const = NULL,
                                facet = NULL, multiple.vars = FALSE,
                                main = NULL, xlab = "Longitude", ylab = "Latitude", axis.labels = TRUE,
                                datum.lines = TRUE, theme = NULL, projection = NULL,
                                label.title = "") {
    if (multiple.vars) {
        args <- names(as.list(args(plot.iNZightMapPlot)))
        args <- args[-length(args)]
        args <- args[args != "colour.var"]
        args <- args[args != "multiple.vars"]
        args <- args[args != "main"]
        arg.call <- paste(args, sep = " = ", args, collapse = ", ")
        
        plots <- invisible(lapply(colour.var, function(x) {
            func.call <- paste0("plot(", arg.call,
                                ", colour.var = '", x, "'",
                                ", main = '", x, "', multiple.vars = FALSE)")
            eval(parse(text = func.call))
        }))
        
        plot.grid <- do.call(gridExtra::arrangeGrob, list(grobs = plots, top = main, as.table = FALSE))
        return(plot.grid)
    } else {
        layers.list <- list(regions = NULL,
                            points = NULL,
                            title = NULL,
                            axislabels = NULL,
                            projection = NULL,
                            theme = NULL)
        
        if (obj$type == "region") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj$region.data,
                                             mapping = ggplot2::aes_string(fill = colour.var),
                                             shape = 21, stroke = 1)
        } else if (obj$type == "point") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj$region.data)
            obj$centroid.data[, paste0(colour.var, "_na")] <- !is.na(as.data.frame(obj$centroid.data)[, colour.var])
            print(obj$centroid.data)
            layers.list[["points"]] <- ggplot2::geom_sf(data = obj$centroid.data,
                                            mapping = ggplot2::aes_string(colour = colour.var,
                                                                          size = size.var,
                                                                          alpha = paste0(colour.var, "_na")),
                                            show.legend = "point")
        } else if (obj$type == "sparklines") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj$region.data)
            layers.list[["sparklines"]] <- geom_sparkline(data = obj$centroid.data,
                                                                    aes_string(group = obj$region.var,
                                                                               x_line = "Year",
                                                                               y_line = colour.var))
        }
        
        layers.list[["title"]] <- ggplot2::labs(title = main)

        if (axis.labels) {
            layers.list[["axislabels"]] <- ggplot2::labs(x = xlab, y = ylab)
        }
        
        projection <- sf::st_crs(projection)
        if (datum.lines) {
            layers.list[["projection"]] <- ggplot2::coord_sf(crs = projection)
        } else {
            layers.list[["projection"]] <- ggplot2::coord_sf(crs = projection, datum = NA)
        }
        
        
##         if (!is.null(facet)) {
##             if (facet == "_MULTI") {
##                 plot(to.plot)
##             } else {
##                 plot.grob <- ggplot2::ggplotGrob(to.plot)
##                 
##                 panel.cols <- plot.grob$layout[grepl("^panel-", plot.grob$layout$name), c("l")] * (-1)
##                 spacer.rows <- which(plot.grob$layout$name == "spacer") * (-1)
##                 
##                 facet.var <- obj$facet.var
##                 
##                 col.to.keep <- which(levels(obj[[layersetHelper(obj$type)]]$baselayer[[1]]$data[[facet.var]]) == facet)
##                 panel.cols <- panel.cols[-col.to.keep]
##                 
##                 to.plot <- plot.grob[spacer.rows, panel.cols]
##             }
##             
##         }

        Reduce(`+`, x = layers.list, init = ggplot2::ggplot())
        
        ## + ggplot2::labs(fill = label.title, colour = label.title) +
            ## ggplot2::scale_fill_continuous(label=scales::comma) 
    }
}
