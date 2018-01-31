#' @title Create iNZightMapPlot object
#'
#' @param data Dataset with values for rows of the map object
#' @param map  An sf object containing a row for each feature of the map
#' @param type What type of map is being passed in. Only \code{"region"} is implemented right now.
#' @param ... Extra arguments required for the type of map being created.
#' @export
iNZightMapPlot <- function(data, map, type, ...) {
  switch(type,
         region = iNZightMapPlotRegion(data, map, ...),
         ## point = iNZightMapPlotPoint(data, map, ...),
         stop("Invalid type argument"))
}

#' @describeIn iNZightMapPlot Constructs a iNZightMapPlot using region values.
#' @import sf
#' @param data Dataset with values for rows of the map object
#' @param map  An sf object containing a row for each feature of the map
#' @param by.data Variable name in the dataset that will be matched to \code{by.map} in the map
#' @param by.map Variable name in the map that will be matched to \code{by.data} in the dataset
#' @param simplification.level How much should the map be simplified by? (see \code{\link[sf]{st_simplify}})
#' @param multiple.obs Does the dataset have multiple observations for each region of the map (i.e. observations from multiple years)
#' @param sequence.var If \code{multiple.obs = TRUE}, which variable identifies the different observations for a region?
#' @param agg.type If \code{multiple.obs = TRUE}, which aggregation should be used to produce one observation for each region.
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
      library(sf)
      ## print(search())
      ## print(methods(summarise))
      mapdata.agg <- mapdata %>%
          dplyr::group_by(UQ((as.name(by.map)))) %>%
          dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")), dplyr::last)

      centroid.agg <- map.centroids %>%
          dplyr::group_by(UQ((as.name(by.map)))) %>%
          dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")), "last")
  } else {
      mapdata.agg <- NULL
      centroid.agg <- NULL
  }

  var.types <- sapply(mapdata, class)

  if (isTRUE(sf::st_crs(map)$proj4string != "")) {
      proj <- sf::st_crs(map)$proj4string
  } else {
      proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  }

  mapplot.obj <- list(region.data = mapdata,
                      centroid.data = map.centroids,
                      type = "region",
                      projection = proj,
                      region.var = by.map,
                      multiple.obs = multiple.obs,
                      sequence.var = sequence.var,
                      region.aggregate = mapdata.agg,
                      centroid.aggregate = centroid.agg,
                      var.types = var.types,
                      map.vars = colnames(map))

  class(mapplot.obj) <- c("iNZightMapPlot", "list")
  attr(mapplot.obj, "code") <- list(
      "## Import the map data",
      sprintf("mapobject <- sf::st_read(\"%s\")", "INSERT FILENAME HERE"),
      "",
      "## Remove regions of the map which are empty",
      "mapobject <- mapobject[!is.na(sf::st_dimension(mapobject)), ]",
      "",
      sprintf("## Join the data up to the map by matching rows using the %s variable from the data and the %s variable from the map.", by.data, by.map),
      sprintf("region.data <- dplyr::left_join(mapobject, data, by = c(\"%s\" = \"%s\"))", by.map, by.data),
      sprintf("centroid.data <- sf::st_centroid(region.data)")
      )

  mapplot.obj
}

#' @title Extract column names from an iNZightMapPlot object
#' @param obj iNZightMapPlot object
#' @param map.vars Should the variables included in the original map be included in the output?
#' @export
iNZightMapVars <- function(obj, map.vars = FALSE) {
    cols <- colnames(obj$region.data)[-ncol(obj$region.data)]
    if (!map.vars) {
        cols <- cols[!(cols %in% obj$map.vars)]
    }

    cols
}


#' Plot an iNZightMapPlot
#' @param obj iNZightMapPlot object
#' @param colour.var Variable to colour the regions or points by
#' @param size.var If plotting a map of points, a variable to scale the points by
#' @param size.const Size of plotted points (ignored if plotting regions or \code{size.var} is also passed)
#' @param alpha.const Alpha value of the underlying region map when plotting points
#' @param multiple.vars Are multiple variables being plotted?
#' @param main Main title for the plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param axis.labels Should the x- and y-axis labels be plotted
#' @param datum.lines Should the datum lines be plotte
#' @param darkTheme Enable dark background
#' @param projection Either \code{"Default"} or a proj4string
#' @param palette Palette to use
#' @param aggregate Is the plot an aggregate
#' @param current.seq Current value of the sequence variable or aggregation
#' @param sparkline.type Either \code{"Absolute"} or \code{"Relative"}
#' @param scale.limits Limits for the legend scale
#' @export
plot.iNZightMapPlot <- function(obj, colour.var = NULL, size.var = NULL, alpha.var = NULL,
                                fill.const = NULL, colour.const = NULL, size.const = 1, alpha.const = 1,
                                facet = NULL, multiple.vars = FALSE,
                                main = NULL, xlab = "Longitude", ylab = "Latitude", axis.labels = TRUE,
                                datum.lines = TRUE, darkTheme = NULL, projection = "Default", palette = NULL,
                                label.title = "", aggregate = FALSE,
                                current.seq = NULL, sparkline.type = "Absolute",
                                scale.limits = NULL) {
    if (multiple.vars) {
        orig.call <- match.call()
		orig.call[1] <- call("plot")
        orig.call$multiple.vars <- FALSE

        plots <- vector("list", length(colour.var))

        for (i in 1:length(plots)) {
            orig.call$colour.var <- colour.var[i]
            if (isTRUE(!is.null(current.seq))) {
                orig.call$main <- paste0(colour.var[i], " (", current.seq, ")")
            } else {
                orig.call$main <- colour.var[i]
            }

            plots[[i]] <- eval.parent(orig.call)
        }

        d.size <- dev.size()
        opt.layout <- n2mfrow(length(plots))
        if(d.size[1] > d.size[2]) {
            opt.layout <- rev(opt.layout)
        }
        if (length(plots) == 3)
            opt.layout <- c(2, 2)

        plot.grid <- do.call(gridExtra::arrangeGrob,
                             list(grobs = plots, top = main,
                                  nrow = opt.layout[1], ncol = opt.layout[2]))
        return(plot.grid)
    } else {
        ## layers.list <- list(regions = NULL,
                            ## points = NULL,
                            ## title = NULL,
                            ## axislabels = NULL)
        layers.list <- list()

        if(obj$multiple.obs) {
            region.data.to.use <- "region.aggregate"
            centroid.data.to.use <- "centroid.aggregate"
        } else {
            region.data.to.use <- "region.data"
            centroid.data.to.use <- "centroid.data"
        }

        if (obj$type == "region") {
            base.ggplot <- ggplot2::ggplot(obj[[region.data.to.use]])
            attr(base.ggplot, "code") <- sprintf("ggplot2::ggplot(%s)", region.data.to.use)

            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                             mapping = ggplot2::aes_string(fill = colour.var),
                                             shape = 21, stroke = 1, inherit.aes = FALSE)

            if (isTRUE(colour.var != "")) {
                attr(layers.list[["regions"]], "code") <- sprintf("ggplot2::geom_sf(ggplot2::aes(fill = %s))", colour.var)
            } else {
                attr(layers.list[["regions"]], "code") <- "ggplot2::geom_sf()"
            }
        } else if (obj$type == "point") {
            base.ggplot <- ggplot2::ggplot(obj[[centroid.data.to.use]])
            attr(base.ggplot, "code") <- sprintf("ggplot2::ggplot()")
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                                         colour = scales::alpha("#000000", alpha.const),
                                                         alpha = alpha.const, inherit.aes = FALSE)
            attr(layers.list[["regions"]], "code") <- sprintf("ggplot2::geom_sf(data = %s, colour = scales::alpha(\"#000000\", %.1f), alpha = %.1f)", region.data.to.use, alpha.const, alpha.const)

            obj[[centroid.data.to.use]][, paste0(colour.var, "_na")] <- is.na(as.data.frame(obj[[centroid.data.to.use]])[, colour.var])
            if (!isTRUE(is.null(size.var) || size.var == "")) {
                layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                                            mapping = ggplot2::aes_string(colour = colour.var,
                                                                                          size = size.var,
                                                                                          alpha = paste0(colour.var, "_na")),
                                                            show.legend = "point", inherit.aes = FALSE)
                attr(layers.list[["points"]], "code") <- sprintf("ggplot2::geom_sf(data = %s, ggplot2::aes(colour = %s, size = %s))", centroid.data.to.use, colour.var, size.var)

                layers.list[["legend.size"]] <- ggplot2::scale_size(guide = FALSE)
                attr(layers.list[["legend.size"]], "code") <- "ggplot2::scale_size(guide = FALSE)"
            } else {
                layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                                            mapping = ggplot2::aes_string(colour = colour.var,
                                                                                          alpha = paste0(colour.var, "_na")),
                                                            show.legend = "point", size = size.const, inherit.aes = FALSE)
                attr(layers.list[["points"]], "code") <- sprintf("ggplot2::geom_sf(data = %s, ggplot2::aes(colour = %s))", centroid.data.to.use, colour.var)
            }


            layers.list[["legend.alpha"]] <- ggplot2::scale_alpha_discrete(guide = FALSE, range = c(1, 0.1))

        } else if (obj$type == "sparklines") {
            base.ggplot <- ggplot2::ggplot(obj[[region.data.to.use]])
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[["region.aggregate"]],
                                                         colour = scales::alpha("#000000", alpha.const),
                                                         alpha = alpha.const, inherit.aes = FALSE)
            if (isTRUE(!is.null(colour.var))) {
                sparkline.relative <- sparkline.type == "Relative"
                layers.list[["sparklines"]] <- ggsfextra::geom_sparkline(data = obj[["centroid.data"]],
                                                              ggplot2::aes_string(group = obj$region.var,
                                                                                  line_x = obj$sequence.var,
                                                                                  line_y = colour.var),
                                                              fill = "white", fill_alpha = 0.75,
                                                              inherit.aes = FALSE, plot_size = size.const,
                                                              relative = sparkline.relative)
            }
        }

        layers.list[["title"]] <- ggplot2::labs(title = main)
        attr(layers.list[["title"]], "code") <- sprintf("ggplot2::labs(title = \"%s\")", main)

        if (axis.labels) {
            layers.list[["axislabels"]] <- ggplot2::labs(x = xlab, y = ylab)
            attr(layers.list[["axislabels"]], "code") <- sprintf("ggplot2::labs(x = \"%s\", y = \"%s\")", xlab, ylab)
        }

        if (isTRUE(projection != "Default")) {
            proj_crs <- sf::st_crs(projection)
        } else {
            proj_crs <- sf::st_crs(obj$projection)
        }

        ## In the case of non-default projections, we need to specifically define it. Otherwise
        ## it is stored in the ggplot object anyway (as it comes from the sf object).
        if (datum.lines) {
            layers.list[["projection"]] <- ggplot2::coord_sf(crs = proj_crs)
            if (isTRUE(projection != "Default")) {
                attr(layers.list[["projection"]], "code") <- sprintf("ggplot2::coord_sf(crs = \"%s\")",
                                                                     proj_crs$proj4string)
            }
        } else {
            layers.list[["projection"]] <- ggplot2::coord_sf(crs = proj_crs, datum = NA)
            if (isTRUE(projection != "Default")) {
                attr(layers.list[["projection"]], "code") <- sprintf("ggplot2::coord_sf(crs = \"%s\", datum = NA)",
                                                                     proj_crs$proj4string)
            } else {
                attr(layers.list[["projection"]], "code") <- "ggplot2::coord_sf(datum = NA)"
            }
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

        if (isTRUE(darkTheme)) {
            layers.list[["theme"]] <- ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#343434"),
                                                     line = ggplot2::element_line(colour = "#555555"))
            attr(layers.list[["theme"]], "code") <- "ggplot2::theme(panel.background = ggplot2::element_rect(fill = \"#343434\"), line = ggplot2::element_line(colour = \"#555555\"))"
        }

        ## layers.list[["legend.bottom"]] <- ggplot2::theme(legend.position = "bottom")
        layers.list[["center.title"]] <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
        attr(layers.list[["center.title"]], "code") <- "ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))"

        if (isTRUE(palette != "Default") && isTRUE(colour.var != "")) {
          layers.list[["palette"]] <- getMapPalette(palette, obj$type, obj$var.types[[colour.var]], limits = scale.limits)
        } else if (isTRUE(!is.null(scale.limits)) && isTRUE(!is.null(colour.var))) {
            if (obj$type == "region" && obj$var.types[[colour.var]] %in% c("numeric", "integer")) {
                layers.list[["palette"]] <- ggplot2::scale_fill_gradient(limits = scale.limits)
            } else if (obj$type == "point" && obj$var.types[[colour.var]] %in% c("numeric", "integer")) {
                layer.list[["palette"]] <- ggplot2::scale_colour_gradient(limits = scale.limits)
            }
        }

        layers.list[["legend.title"]] <- ggplot2::theme(legend.title = ggplot2::element_blank())
        attr(layers.list[["legend.title"]], "code") <- "ggplot2::theme(legend.title = ggplot2::element_blank())"

        plot.obj <- Reduce(`+`, x = layers.list, init = base.ggplot)

        ## Collate the code for each layer and format
        code.list <- lapply(c(list(base.ggplot), layers.list),
                                         function(x) attr(x, "code"))
        code.list <- code.list[!sapply(code.list, is.null)]
        n.lines <- length(code.list)
        code.list[1:(n.lines - 1)] <- paste(code.list[1:(n.lines - 1)], "+")
        code.list[2:n.lines] <- paste0("\t", code.list[2:n.lines])
        attr(plot.obj, "code") <- c(list("## Plot the map using ggplot2"), code.list)

        plot.obj
    }
}

#' @export
iNZightMapAggregation <- function(obj, aggregation = "mean", single.value = NULL) {
    if (aggregation == "singlevalue") {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(UQ((as.name(obj$region.var)))) %>%
            dplyr::filter((UQ((as.name(obj$sequence.var)))) == single.value | is.na(UQ((as.name(obj$sequence.var)))))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(UQ((as.name(obj$region.var)))) %>%
            dplyr::filter((UQ((as.name(obj$sequence.var)))) == single.value | is.na(UQ((as.name(obj$sequence.var)))))
    } else {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(UQ((as.name(obj$region.var)))) %>%
            dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")),
                       dplyr::funs(if (is.numeric(.))
                                          eval(substitute(chosen_fun(., na.rm = TRUE),
                                                          list(chosen_fun = as.name(aggregation))))
                                      else dplyr::last(.)))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(UQ((as.name(obj$region.var)))) %>%
            dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")),
                                dplyr::funs(if (is.numeric(.))
                                         eval(substitute(chosen_fun(., na.rm = TRUE),
                                                         list(chosen_fun = as.name(aggregation))))
                                     else dplyr::last(.)))
    }

   obj
}

theme_dark <- ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#343434"),
                             line = ggplot2::element_line(colour = "#555555"))

mapThemes <- list("Default" = NULL,
                  "Dark" = theme_dark)

getMapPalette <- function(palette, map.type, var.type, direction = 1, limits = NULL) {
  viridis.pals <- c("Viridis", "Magma", "Plasma", "Inferno")
  brewer.pals <- c("BrBG", "PiYG", "PRGn",
                   "Accent", "Dark2", "Paired", "Pastel1", "Set1",
                   "Blues", "BuGn", "BuPu", "GnBu")

  if (map.type == "region") {
    if (var.type %in% c("numeric", "integer")) {
      if (palette %in% viridis.pals) {
        ggplot2::scale_fill_viridis_c(option = tolower(palette), direction = direction, limits = limits)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_fill_distiller(palette = palette, direction = direction, limits = limits)
      }
    } else {
      if (palette %in% viridis.pals) {
        ggplot2::scale_fill_viridis_d(option = tolower(palette), direction = direction, limits = limits)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_fill_brewer(palette = palette, direction = direction, limits = limits)
      }
    }
  } else {
    if (var.type %in% c("numeric", "integer")) {
      if (palette %in% viridis.pals) {
        ggplot2::scale_colour_viridis_c(option = tolower(palette), direction = direction, limits = limits)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_colour_distiller(palette = palette, direction = direction, limits = limits)
      }
    } else {
      if (palette %in% viridis.pals) {
        ggplot2::scale_colour_viridis_d(option = tolower(palette), direction = direction, limits = limits)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_colour_brewer(palette = palette, direction = direction, limits = limits)
      }
    }
  }
}

