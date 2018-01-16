#' @title Create iNZightMapPlot object
#' @export
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
          dplyr::group_by(rlang::`!!`(as.name(by.map))) %>%
          dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")), dplyr::last)

      centroid.agg <- map.centroids %>%
          dplyr::group_by(rlang::`!!`(as.name(by.map))) %>%
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

  print(summarise.sf)
  print(methods(summarise))

  mapplot.obj
}

#' @export
iNZightMapVars <- function(obj, map.vars = FALSE) {
    cols <- colnames(obj$region.data)[-ncol(obj$region.data)]
    if (!map.vars) {
        cols <- cols[!(cols %in% obj$map.vars)]
    }

    cols
}


#' @export
plot.iNZightMapPlot <- function(obj, colour.var = NULL, size.var = NULL, alpha.var = NULL,
                                fill.const = NULL, colour.const = NULL, size.const = NULL, alpha.const = NULL,
                                facet = NULL, multiple.vars = FALSE,
                                main = NULL, xlab = "Longitude", ylab = "Latitude", axis.labels = TRUE,
                                datum.lines = TRUE, darkTheme = NULL, projection = NULL, palette = NULL,
                                label.title = "", aggregate = FALSE,
                                current.seq = NULL) {
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
                                             shape = 21, stroke = 1)
        } else if (obj$type == "point") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                                         colour = scales::alpha("#000000", alpha.const),
                                                         alpha = alpha.const)

            obj[[centroid.data.to.use]][, paste0(colour.var, "_na")] <- is.na(as.data.frame(obj[[centroid.data.to.use]])[, colour.var])
            if (!isTRUE(is.null(size.var) || size.var == "")) {
                layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                                            mapping = ggplot2::aes_string(colour = colour.var,
                                                                                          size = size.var,
                                                                                          alpha = paste0(colour.var, "_na")),
                                                            show.legend = "point")

                layers.list[["legend.size"]] <- ggplot2::scale_size(guide = FALSE)
            } else {
                layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                                            mapping = ggplot2::aes_string(colour = colour.var,
                                                                                          alpha = paste0(colour.var, "_na")),
                                                            show.legend = "point", size = size.const)
            }


            layers.list[["legend.alpha"]] <- ggplot2::scale_alpha_discrete(guide = FALSE, range = c(1, 0.1))

        } else if (obj$type == "sparklines") {
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[["region.aggregate"]],
                                                         colour = scales::alpha("#000000", alpha.const),
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

        if (isTRUE(darkTheme)) {
            layers.list[["theme"]] <- ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#343434"),
                                                     line = ggplot2::element_line(colour = "#555555"))
        }

        ## layers.list[["legend.bottom"]] <- ggplot2::theme(legend.position = "bottom")
        layers.list[["center.title"]] <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

        if (isTRUE(palette != "Default") && isTRUE(colour.var != "")) {
          layers.list[["palette"]] <- getMapPalette(palette, obj$type, obj$var.types[[colour.var]])
        }

        Reduce(`+`, x = layers.list, init = ggplot2::ggplot())
    }
}

#' @export
iNZightMapAggregation <- function(obj, aggregation = "mean", single.value = NULL) {
    if (aggregation == "singlevalue") {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(rlang::`!!`(as.name(obj$region.var))) %>%
            dplyr::filter((rlang::`!!`(as.name(obj$sequence.var))) == single.value | is.na(rlang::`!!`(as.name(obj$sequence.var))))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(rlang::`!!`(as.name(obj$region.var))) %>%
            dplyr::filter((rlang::`!!`(as.name(obj$sequence.var))) == single.value | is.na(rlang::`!!`(as.name(obj$sequence.var))))
    } else {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(rlang::`!!`(as.name(obj$region.var))) %>%
            dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$")),
                       dplyr::funs(if (is.numeric(.))
                                          eval(substitute(chosen_fun(., na.rm = TRUE),
                                                          list(chosen_fun = as.name(aggregation))))
                                      else dplyr::last(.)))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(rlang::`!!`(as.name(obj$region.var))) %>%
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

getMapPalette <- function(palette, map.type, var.type, direction = 1) {
  viridis.pals <- c("Viridis", "Magma", "Plasma", "Inferno")
  brewer.pals <- c("BrBG", "PiYG", "PRGn",
                   "Accent", "Dark2", "Paired", "Pastel1", "Set1",
                   "Blues", "BuGn", "BuPu", "GnBu")

  if (map.type == "region") {
    if (var.type %in% c("numeric", "integer")) {
      if (palette %in% viridis.pals) {
        ggplot2::scale_fill_viridis_c(option = tolower(palette), direction = direction)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_fill_distiller(palette = palette, direction = direction)
      }
    } else {
      if (palette %in% viridis.pals) {
        ggplot2::scale_fill_viridis_d(option = tolower(palette), direction = direction)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_fill_brewer(palette = palette, direction = direction)
      }
    }
  } else {
    if (var.type %in% c("numeric", "integer")) {
      if (palette %in% viridis.pals) {
        ggplot2::scale_colour_viridis_c(option = tolower(palette), direction = direction)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_colour_distiller(palette = palette, direction = direction)
      }
    } else {
      if (palette %in% viridis.pals) {
        ggplot2::scale_colour_viridis_d(option = tolower(palette), direction = direction)
      } else if (palette %in% brewer.pals) {
        ggplot2::scale_colour_brewer(palette = palette, direction = direction)
      }
    }
  }
}

