iNZightMapPlot <- function(data, map, type, ...) {
  switch(type,
         region = iNZightMapPlotRegion(data, map, ...),
         ## point = iNZightMapPlotPoint(data, map, ...),
         stop("Invalid type argument"))
}

#' @describeIn iNZightMapPlot Constructs a iNZightMapPlot using region values.
iNZightMapPlotRegion <- function(data, map, by.data, by.map, simplification.level = 0.01,
                                 multiple.obs = FALSE, sequence.var = NULL, agg.type = "last") {
  by.vect <- c(by.data)
  names(by.vect) <- by.map

  map <- map[!is.na(sf::st_dimension(map)), ]

  map[, by.map] <- as.character(as.data.frame(map)[, by.map])
  data[, by.data] <- as.character(data[, by.data])

  mapdata <- sf::st_as_sf(dplyr::left_join(map, data, by = by.vect))
  map.centroids <- sf::st_centroid(mapdata)

  mapdata <- sf::st_simplify(mapdata, dTolerance = simplification.level)
  
  if (multiple.obs) {
      mapdata.agg <- mapdata %>%
          dplyr::group_by(!!as.name(by.map)) %>%
          dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")), "last")

      centroid.agg <- map.centroids %>%
          dplyr::group_by(!!as.name(by.map)) %>%
          dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")), "last")
  } else {
      mapdata.agg <- NULL
      centroid.agg <- NULL
  }

  var.types <- sapply(mapdata, class)

  mapplot.obj <- list(region.data = mapdata,
                      centroid.data = map.centroids,
                      type = "region",
                      projection = "",
                      code.history = "TODO...",
                      region.var = by.map,
                      multiple.obs = multiple.obs,
                      sequence.var = sequence.var,
                      region.aggregate = mapdata.agg, 
                      centroid.aggregate = centroid.agg,
                      var.types = var.types,
                      map.vars = colnames(map)) 

  class(mapplot.obj) <- c("iNZightMapPlot", "list")

  mapplot.obj
}

iNZightMapVars <- function(obj, map.vars = FALSE) {
    cols <- colnames(obj$region.data)[-ncol(obj$region.data)]
    if (!map.vars) {
        cols <- cols[!(cols %in% obj$map.vars)]
    }

    cols                                 
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Plot an iNZightMapPlot object
##' @param obj iNZightMapPlot object
##' @param colour.var String containing the variable name to map to
##'     colour for either the regions or points (depending on the map
##'     type)
##' @param size.var String containing the variable name to map to
##'     point size
##' @param multiple.vars Is the plot of multiple variables?
##' @param main Title of the plot
##' @param xlab x-axis label
##' @param ylab y-axis label
##' @param axis.labels Should the axis labels be visible?
##' @param datum.lines Should the datum lines (grid behind the map) be
##'     visible?
##' @param theme Either NULL for no theme or a string corresponding to
##'     a theme. See [...]
##' @param projection Either a PROJ4 string (or EPSG code?)
##' @param aggregate Is the plot some form of aggregate of the
##'     original data (either mean/median/etc or a single year's data)
##' @return A ggplot object that can be plotted
##' @author Daniel Barnett
##' @export
plot.iNZightMapPlot <- function(obj, colour.var = NULL, size.var = NULL, alpha.var = NULL,
                                fill.const = NULL, colour.const = NULL, size.const = NULL, alpha.const = NULL,
                                facet = NULL, multiple.vars = FALSE,
                                main = NULL, xlab = "Longitude", ylab = "Latitude", axis.labels = TRUE,
                                datum.lines = TRUE, theme = NULL, projection = NULL,
                                label.title = "", aggregate = FALSE) {
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

        d.size <- dev.size()
        opt.layout <- n2mfrow(length(plots))
        if(d.size[1] > d.size[2]) {
            opt.layout <- rev(opt.layout)
        }
        
        plot.grid <- do.call(gridExtra::arrangeGrob,
                             list(grobs = plots, top = main,
                                  nrow = opt.layout[1], ncol = opt.layout[2]))
        return(plot.grid)
    } else {
        layers.list <- list(regions = NULL,
                            points = NULL,
                            title = NULL,
                            axislabels = NULL)

        if(obj$multiple.obs) {
            region.data.to.use <- "region.aggregate"
            centroid.data.to.use <- "centroid.aggregate"
        } else {
            region.data.to.use <- "region.data"
            centroid.data.to.use <- "centroid.data"
        }         

        if (obj$type == "region") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                             mapping = ggplot2::aes_string(fill = colour.var),
                                             shape = 21, stroke = 1, alpha = alpha.const)
        } else if (obj$type == "point") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                                         colour = alpha("#000000", alpha.const),
                                                         alpha = alpha.const)

            obj[[centroid.data.to.use]][, paste0(colour.var, "_na")] <- is.na(as.data.frame(obj[[centroid.data.to.use]])[, colour.var])
            layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                            mapping = ggplot2::aes_string(colour = colour.var,
                                                                          size = size.var,
                                                                          alpha = paste0(colour.var, "_na")),
                                            show.legend = "point") 
            if (!is.null(size.var)) {
                layers.list[["legend.size"]] <- ggplot2::scale_size(guide = FALSE)
            }

            layers.list[["legend.alpha"]] <- ggplot2::scale_alpha_discrete(guide = FALSE, range = c(1, 0.1))
            
        } else if (obj$type == "sparklines") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[["region.aggregate"]],
                                                         colour = alpha("#000000", alpha.const),
                                                         alpha = alpha.const)
            if (isTRUE(!is.null(colour.var))) {
                layers.list[["sparklines"]] <- geom_sparkline(data = obj[["centroid.data"]],
                                                              ggplot2::aes_string(group = obj$region.var,
                                                                                  line_x = obj$sequence.var,
                                                                                  line_y = colour.var),
                                                              fill = "white", fill_alpha = 0.75)
            }
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

        if (!is.null(theme)) {
            layers.list[["theme"]] <- mapThemes[[theme]]
        }

        layers.list[["legend.bottom"]] <- ggplot2::theme(legend.position = "bottom")

        Reduce(`+`, x = layers.list, init = ggplot2::ggplot()) 
    }
}

##' @export
iNZightMapAggregation <- function(obj, aggregation = "mean", single.value = NULL) {
    if (aggregation == "singlevalue") {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(!!as.name(obj$region.var)) %>%
            dplyr::filter((!!as.name(obj$sequence.var)) == single.value | is.na(!!as.name(obj$sequence.var)))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(!!as.name(obj$region.var)) %>%
            dplyr::filter((!!as.name(obj$sequence.var)) == single.value | is.na(!!as.name(obj$sequence.var)))
    } else {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(!!as.name(obj$region.var)) %>%
            dplyr::summarise_at(vars(-dplyr::matches("^geometry$")),
                       funs(if (is.numeric(.))
                                          eval(substitute(chosen_fun(., na.rm = TRUE),
                                                          list(chosen_fun = as.name(aggregation))))
                                      else last(.)))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(!!as.name(obj$region.var)) %>%
            dplyr::summarise_at(vars(-dplyr::matches("^geometry$")),
                                funs(if (is.numeric(.))
                                         eval(substitute(chosen_fun(., na.rm = TRUE),
                                                         list(chosen_fun = as.name(aggregation))))
                                     else last(.)))
    }

   obj 
}

theme_dark <- ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#343434"),
                             line = ggplot2::element_line(colour = "#555555"))

mapThemes <- list("Default" = NULL,
                  "Dark" = theme_dark)

