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

  ## This is actually being done by iNZightModules, which will be enough for now.
  # if (packageVersion('ggplot2') < numeric.version("2.2.1.9000")) {
  #   ## R is too old ...
  #   if (getRversion() < numeric_version(3.3))
  #     stop("The region maps are only available on R v3.3.0 or later")

  #   ## ggplot2 is too old ...
  #   stop("You need to install the development version of `ggplot2`:\n  https://github.com/tidyverse/ggplot2")
  # }

  by.vect <- c(by.data)
  names(by.vect) <- by.map

  map <- map[!is.na(sf::st_dimension(map)), ]

  map[, by.map] <- as.character(as.data.frame(map)[, by.map])
  data[, by.data] <- as.character(data[, by.data])

  mapdata <- sf::st_as_sf(dplyr::left_join(map, data, by = by.vect))
  map.centroids <- sf::st_centroid(mapdata)

  mapdata <- sf::st_simplify(mapdata, dTolerance = simplification.level)

  if (multiple.obs) {
      ## library(sf)
      mapdata.agg <- mapdata %>%
          dplyr::group_by(rlang::UQ((as.name(by.map)))) %>%
          dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$"), -rlang::UQ(as.name(by.map))), dplyr::last)

      centroid.agg <- map.centroids %>%
          dplyr::group_by(rlang::UQ((as.name(by.map)))) %>%
          dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$"), -rlang::UQ(as.name(by.map))), dplyr::last)
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

#' @export
iNZightMapRegions <- function(obj) {
    sort(unique(as.data.frame(obj$region.data)[, obj$region.var]))
}


#' Plot an iNZightMapPlot
#' @param x iNZightMapPlot object
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
#' @param ... additional arguments (ignored)
#' @importFrom rlang ":=" UQ
#' @export
plot.iNZightMapPlot <- function(x, colour.var = NULL, size.var = NULL, alpha.var = NULL,
                                fill.const = NULL, colour.const = NULL, size.const = 1, alpha.const = 1,
                                facet = NULL, multiple.vars = FALSE,
                                main = NULL, xlab = "Longitude", ylab = "Latitude", axis.labels = TRUE,
                                datum.lines = TRUE, darkTheme = NULL, projection = "Default", palette = NULL,
                                label.title = "", aggregate = FALSE,
                                current.seq = NULL, sparkline.type = "Absolute",
                                scale.limits = NULL, ci.plot = FALSE,
                                regions.to.plot = NULL, keep.other.regions = TRUE,
                                label.var = NULL, scale.label = 1, scale.axis = 1,
                                ...) {
    obj <- x
    if (multiple.vars) {
        orig.call <- match.call()
		orig.call[1] <- call("plot")
        orig.call$multiple.vars <- FALSE

        plots <- vector("list", length(colour.var))

        for (i in 1:length(plots)) {
            orig.call$colour.var <- colour.var[i]
            
            if (isTRUE(is.list(scale.limits) && length(scale.limits) > 1)) {
              orig.call$scale.limits <- scale.limits[[i]]
            }
            
            if (isTRUE(!is.null(current.seq))) {
                orig.call$main <- paste0(colour.var[i], " (", current.seq, ")")
            } else {
                orig.call$main <- colour.var[i]
            }

            plots[[i]] <- eval.parent(orig.call)
        }

        d.size <- grDevices::dev.size()
        opt.layout <- grDevices::n2mfrow(length(plots))
        if(d.size[1] > d.size[2]) {
            opt.layout <- rev(opt.layout)
        }
        if (length(plots) == 3)
            opt.layout <- c(2, 2)

        plot.grid <- do.call(gridExtra::arrangeGrob,
                             list(grobs = plots, top = main,
                                  nrow = opt.layout[1], ncol = opt.layout[2]))

        all.code <- lapply(plots, function(x) attr(x, "code"))
        attr(plot.grid, "code") <- unlist(all.code, recursive = FALSE)

        return(plot.grid)
    } else {
        layers.list <- list()
        
        if (isTRUE(is.list(scale.limits))) {
          scale.limits <- scale.limits[[1]]
        }

        if(obj$multiple.obs) {
            region.data.to.use <- "region.aggregate"

            if (obj$type == "sparklines") {
                centroid.data.to.use <- "centroid.data"
            } else {
                centroid.data.to.use <- "centroid.aggregate"
            }
        } else {
            region.data.to.use <- "region.data"
            centroid.data.to.use <- "centroid.data"
        }

        if (!is.null(regions.to.plot)) {
            if (keep.other.regions & length(regions.to.plot) > 0) {
                 obj[[region.data.to.use]] <- dplyr::mutate(obj[[region.data.to.use]],
                                                             rlang::UQ(as.name(colour.var)) := replace(rlang::UQ(as.name(colour.var)), !(rlang::UQ(as.name(obj$region.var)) %in% regions.to.plot), NA))

                 obj[[centroid.data.to.use]] <- dplyr::mutate(obj[[centroid.data.to.use]],
                                                            rlang::UQ(as.name(colour.var)) := replace(rlang::UQ(as.name(colour.var)), !(rlang::UQ(as.name(obj$region.var)) %in% regions.to.plot), NA))
            } else {
                obj[[region.data.to.use]] <- dplyr::filter(obj[[region.data.to.use]],
                                                           rlang::UQ(as.name(obj$region.var)) %in% regions.to.plot)
                obj[[centroid.data.to.use]] <- dplyr::filter(obj[[centroid.data.to.use]],
                                                             rlang::UQ(as.name(obj$region.var)) %in% regions.to.plot)
            }
        }


        if (obj$type == "region") {
            base.ggplot <- ggplot2::ggplot(obj[[region.data.to.use]])
            attr(base.ggplot, "code") <- sprintf("ggplot2::ggplot(%s)", region.data.to.use)

            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                             mapping = ggplot2::aes_string(fill = colour.var),
                                             shape = 21, stroke = 1, inherit.aes = FALSE)

            ## Only add the fill aesthetic if we have a variable plotted
            attr(layers.list[["regions"]], "code") <- sprintf("ggplot2::geom_sf(%s)",
                                                              if (isTRUE(colour.var != ""))
                                                                  sprintf("ggplot2::aes(fill = %s)", colour.var)
                                                              else
                                                                  "")
        } else if (obj$type == "point") {
            base.ggplot <- ggplot2::ggplot(obj[[centroid.data.to.use]])
            attr(base.ggplot, "code") <- sprintf("ggplot2::ggplot()")
            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                                         colour = scales::alpha("#000000", alpha.const),
                                                         alpha = alpha.const, inherit.aes = FALSE)

            ## Only put the alpha argument if its required (i.e. alpha < 1)
            attr(layers.list[["regions"]], "code") <- sprintf("ggplot2::geom_sf(data = %s%s)",
                                                              region.data.to.use,
                                                              if (alpha.const < 1)
                                                                  sprintf(", colour = scales::alpha(\"#000000\", %.1f), alpha = %.1f)", alpha.const, alpha.const)
                                                              else
                                                                  "")

            if (isTRUE(colour.var != "")) {
                obj[[centroid.data.to.use]][, paste0(colour.var, "_na")] <- is.na(as.data.frame(obj[[centroid.data.to.use]])[, colour.var])

                if (isTRUE(size.var != "")) {
                    layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                                                mapping = ggplot2::aes_string(colour = colour.var,
                                                                                              size = size.var,
                                                                                              alpha = paste0(colour.var, "_na")),
                                                                show.legend = "point", inherit.aes = FALSE)

                    layers.list[["legend.size"]] <- ggplot2::scale_size(guide = FALSE,
                                                                        range = (size.const - 1) + c(1, 6))

                    attr(layers.list[["points"]], "code") <- sprintf("ggplot2::geom_sf(data = %s, ggplot2::aes(colour = %s, size = %s))",
                                                                     centroid.data.to.use, colour.var, size.var)

                    attr(layers.list[["legend.size"]], "code") <- "ggplot2::scale_size(guide = FALSE)"
                } else {
                    layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                                                mapping = ggplot2::aes_string(colour = colour.var,
                                                                                              alpha = paste0(colour.var, "_na")),
                                                                show.legend = "point", size = size.const, inherit.aes = FALSE)

                    ## Only add size argument if constant size has been changed
                    attr(layers.list[["points"]], "code") <- sprintf("ggplot2::geom_sf(data = %s, ggplot2::aes(colour = %s)%s)",
                                                                     centroid.data.to.use, colour.var,
                                                                     if (size.const != 1)
                                                                         paste0(", size = ", size.const)
                                                                     else
                                                                         "")
                }

                layers.list[["legend.alpha"]] <- ggplot2::scale_alpha_discrete(guide = FALSE, range = c(1, 0.1))
            } else {
                if (isTRUE(size.var != "")) {
                    layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]],
                                                                mapping = ggplot2::aes_string(size = size.var),
                                                                show.legend = "point", inherit.aes = FALSE)

                    layers.list[["legend.size"]] <- ggplot2::scale_size(guide = FALSE)

                    attr(layers.list[["points"]], "code") <- sprintf("ggplot2::geom_sf(data = %s, ggplot2::aes(size = %s))",
                                                                     centroid.data.to.use, size.var)

                    attr(layers.list[["legend.size"]], "code") <- "ggplot2::scale_size(guide = FALSE)"
                } else {
                    layers.list[["points"]] <- ggplot2::geom_sf(data = obj[[centroid.data.to.use]], size = size.const, inherit.aes = FALSE)

                    ## Only add size argument if constant size has been changed
                    attr(layers.list[["points"]], "code") <- sprintf("ggplot2::geom_sf(data = %s%s)",
                                                                     centroid.data.to.use,
                                                                     if (size.const != 1)
                                                                         paste0(", size = ", size.const)
                                                                     else
                                                                         "")
                }
            }

        } else if (obj$type == "sparklines") {
            base.ggplot <- ggplot2::ggplot(obj[[region.data.to.use]])
            attr(base.ggplot, "code") <- sprintf("ggplot2::ggplot(%s)", region.data.to.use)

            layers.list[["regions"]] <- ggplot2::geom_sf(data = obj[[region.data.to.use]],
                                                         colour = scales::alpha("#000000", alpha.const),
                                                         alpha = alpha.const, inherit.aes = FALSE)

            ## Only put the alpha argument if its required (i.e. alpha < 1)
            attr(layers.list[["regions"]], "code") <- sprintf("ggplot2::geom_sf(%s)",
                                                              if (alpha.const < 1)
                                                                  sprintf("colour = scales::alpha(\"#000000\", %.1f), alpha = %.1f)", alpha.const, alpha.const)
                                                              else
                                                                  "")

            if (isTRUE(colour.var != "")) {
                layers.list[["sparklines"]] <- ggsfextra::geom_sparkline(data = obj[[centroid.data.to.use]],
                                                              ggplot2::aes_string(group = obj$region.var,
                                                                                  line_x = obj$sequence.var,
                                                                                  line_y = colour.var),
                                                              fill = "white", fill_alpha = 0.75,
                                                              inherit.aes = FALSE, plot_size = size.const,
                                                              sparkline_type = sparkline.type)
                attr(layers.list[["sparklines"]], "code") <- sprintf("ggsfextra::geom_sparkline(data = %s, aes(group = %s, line_x = %s, line_y = %s), fill_alpha = 0.75, plot_size = %f, sparkline_type = %s)",
                                                                     centroid.data.to.use, obj$region.var, obj$sequence.var, colour.var, size.const, sparkline.type)
            }
        }

        if (!is.null(label.var)) {
            if (label.var == "use_colour_var") {
                label.var = colour.var
            }

            if (obj$type != "sparklines") {
                layers.list[["sftext"]] <- ggsfextra::geom_sftext(data = obj[[centroid.data.to.use]], ggplot2::aes_string(label = label.var, colour = colour.var), size = scale.label, inherit.aes = FALSE)
            } else {
                layers.list[["sftext"]] <- ggsfextra::geom_sftext(data = obj[["centroid.aggregate"]], ggplot2::aes_string(label = label.var), inherit.aes = FALSE)
            }
        }

        ## If projection is "Default", take the one stored in the sf object. Otherwise, use
        ## the proj4string passed in.
        if (isTRUE(projection != "Default")) {
            proj_crs <- sf::st_crs(projection)
        } else {
            proj_crs <- sf::st_crs(obj$projection)
        }

        ## print(proj_crs)

        ## In the case of non-default projections, we need to specifically define it. Otherwise
        ## it is stored in the ggplot object anyway (as it comes from the sf object).
        if (datum.lines) {
            if (!is.null(regions.to.plot) & length(regions.to.plot) > 0 & keep.other.regions) {
                region.bbox <- sf::st_bbox(sf::st_transform(stats::na.omit(obj[[region.data.to.use]][, colour.var]), crs = proj_crs))
                layers.list[["projection"]] <- ggplot2::coord_sf(crs = proj_crs,
                                                                 xlim = region.bbox[c(1, 3)],
                                                                 ylim = region.bbox[c(2, 4)])
            } else if (is.null(regions.to.plot) || length(regions.to.plot) > 0) {
                layers.list[["projection"]] <- ggplot2::coord_sf(crs = proj_crs)
            }

            if (isTRUE(projection != "Default")) {
                attr(layers.list[["projection"]], "code") <- sprintf("ggplot2::coord_sf(crs = \"%s\")",
                                                                     proj_crs$proj4string)
            }
        } else {
            ## print(regions.to.plot)
            if (!is.null(regions.to.plot) & length(regions.to.plot) > 0 & keep.other.regions) {
                region.bbox <- sf::st_bbox(sf::st_transform(stats::na.omit(obj[[region.data.to.use]][, colour.var]), crs = proj_crs))
                ## print("I'm here")
                layers.list[["projection"]] <- ggplot2::coord_sf(crs = proj_crs, datum = NA,
                                                                 xlim = region.bbox[c(1, 3)],
                                                                 ylim = region.bbox[c(2, 4)])
            } else if (is.null(regions.to.plot) || length(regions.to.plot) > 0) {
                ## print("I'm actually here")
                layers.list[["projection"]] <- ggplot2::coord_sf(crs = proj_crs, datum = NA)
            } else {
                ## print("Here")
                layers.list[["projection"]] <- ggplot2::coord_sf(datum = NA)
            }

            if (isTRUE(projection != "Default")) {
                attr(layers.list[["projection"]], "code") <- sprintf("ggplot2::coord_sf(crs = \"%s\", datum = NA)",
                                                                     proj_crs$proj4string)
            } else {
                attr(layers.list[["projection"]], "code") <- "ggplot2::coord_sf(datum = NA)"
            }
        }

        ## print(layers.list[["projection"]])
        layers.list[["text.scale"]] <- ggplot2::theme_gray(base_size = scale.axis)

        ## Dark background
        if (isTRUE(darkTheme)) {
            layers.list[["theme"]] <- ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#343434"),
                                                     line = ggplot2::element_line(colour = "#555555"))
            attr(layers.list[["theme"]], "code") <- "ggplot2::theme(panel.background = ggplot2::element_rect(fill = \"#343434\"), line = ggplot2::element_line(colour = \"#555555\"))"
        }

        ## Palette
        if (isTRUE(palette != "Default") && isTRUE(colour.var != "")) {
          layers.list[["palette"]] <- getMapPalette(palette, obj$type, obj$var.types[[colour.var]], limits = scale.limits)
        } else if (isTRUE(!is.null(scale.limits)) && isTRUE(!is.null(colour.var))) {
            if (obj$type == "region" && obj$var.types[[colour.var]] %in% c("numeric", "integer")) {
                layers.list[["palette"]] <- ggplot2::scale_fill_gradient(limits = scale.limits)
            } else if (obj$type == "point" && obj$var.types[[colour.var]] %in% c("numeric", "integer")) {
                layers.list[["palette"]] <- ggplot2::scale_colour_gradient(limits = scale.limits)
            }
        }

        ## Title
        if (isTRUE(main != "")) {
            layers.list[["title"]] <- ggplot2::labs(title = main)
            layers.list[["center.title"]] <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

            attr(layers.list[["title"]], "code") <- sprintf("ggplot2::labs(title = \"%s\")", main)
            attr(layers.list[["center.title"]], "code") <- "ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))"
        }

        ## Axis labels
        if (axis.labels) {
            layers.list[["axislabels"]] <- ggplot2::labs(x = xlab, y = ylab)
            attr(layers.list[["axislabels"]], "code") <- sprintf("ggplot2::labs(x = \"%s\", y = \"%s\")", xlab, ylab)
        }

        ## Remove legend title if required
        if (isTRUE(colour.var != "")) {
            layers.list[["legend.title"]] <- ggplot2::theme(legend.title = ggplot2::element_blank())
            attr(layers.list[["legend.title"]], "code") <- "ggplot2::theme(legend.title = ggplot2::element_blank())"
        }


        plot.obj <- Reduce(`+`, x = layers.list, init = base.ggplot)

        ## Collate the code for each layer and format
        code.list <- lapply(c(list(base.ggplot), layers.list),
                                         function(x) attr(x, "code"))
        code.list <- code.list[!sapply(code.list, is.null)]
        n.lines <- length(code.list)
        code.list[1:(n.lines - 1)] <- paste(code.list[1:(n.lines - 1)], "+")
        code.list[2:n.lines] <- paste0("\t", code.list[2:n.lines])
        code.list[n.lines] <- paste0(code.list[n.lines], "\n")

        if (isTRUE(colour.var != ""))
            opening.comment <- sprintf("## Plot a map of variable %s", colour.var)
        else
            opening.comment <- "## Plot the map"

        attr(plot.obj, "code") <- c(list(opening.comment), code.list)

        plot.obj
    }
}

#' @title Aggregate an iNZightMapPlot with Multiple Observations
#' @param obj iNZightMapPlot object
#' @param aggregation Type of aggregation to use
#' @param single.value If \code{aggregation = "singlevalue"}, the observation that should be extracted
#' @importFrom rlang UQ .data
#' @export
iNZightMapAggregation <- function(obj, aggregation = "mean", single.value = NULL) {
    if (aggregation == "singlevalue") {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(rlang::UQ((as.name(obj$region.var)))) %>%
            dplyr::filter((rlang::UQ((as.name(obj$sequence.var)))) == single.value | is.na(rlang::UQ((as.name(obj$sequence.var)))))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(rlang::UQ((as.name(obj$region.var)))) %>%
            dplyr::filter((rlang::UQ((as.name(obj$sequence.var)))) == single.value | is.na(rlang::UQ((as.name(obj$sequence.var)))))
    } else {
        obj$region.aggregate <- obj$region.data %>%
            dplyr::group_by(rlang::UQ((as.name(obj$region.var)))) %>%
            dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$"), -UQ((as.name(obj$region.var)))),
                       dplyr::funs(if (is.numeric(.))
                                          eval(substitute(chosen_fun(., na.rm = TRUE),
                                                          list(chosen_fun = as.name(aggregation))))
                                      else dplyr::last(.)))
        obj$centroid.aggregate <- obj$centroid.data %>%
            dplyr::group_by(rlang::UQ((as.name(obj$region.var)))) %>%
            dplyr::summarise_at(dplyr::vars(-dplyr::matches("^geometry$"), -UQ((as.name(obj$region.var)))),
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

getMapPalette <- function(col.pal, map.type, var.type, direction = 1, limits = NULL) {
  viridis.pals <- c("Viridis", "Magma", "Plasma", "Inferno")
  brewer.pals <- c("BrBG", "PiYG", "PRGn",
                   "Accent", "Dark2", "Paired", "Pastel1", "Set1",
                   "Blues", "BuGn", "BuPu", "GnBu")

  ## Define some string representations to make code writing easier
  if (isTRUE(is.null(limits)))
      limit.str <- ""
  else
      limit.str <- sprintf(", limits = c(%f, %f)", limits[1], limits[2])

  if (map.type == "region") {
    if (var.type %in% c("numeric", "integer")) {
      if (col.pal %in% viridis.pals) {
        palette <- ggplot2::scale_fill_viridis_c(option = tolower(col.pal), direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_fill_viridis_c(option = \"%s\", direction = %d%s)",
                                         tolower(col.pal), direction, limit.str)

      } else if (col.pal %in% brewer.pals) {
        palette <- ggplot2::scale_fill_distiller(palette = col.pal, direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_fill_distiller(palette = \"%s\", direction = %d%s)",
                                         col.pal, direction, limit.str)
      }
    } else {
      if (col.pal %in% viridis.pals) {
        palette <- ggplot2::scale_fill_viridis_d(option = tolower(col.pal), direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_fill_viridis_d(option = \"%s\", direction = %d%s)",
                                         tolower(col.pal), direction, limit.str)
      } else if (col.pal %in% brewer.pals) {
        palette <- ggplot2::scale_fill_brewer(palette = col.pal, direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_fill_brewer(palette = \"%s\", direction = %d%s)",
                                         col.pal, direction, limit.str)
      }
    }
  } else {
    if (var.type %in% c("numeric", "integer")) {
      if (col.pal %in% viridis.pals) {
        palette <- ggplot2::scale_colour_viridis_c(option = tolower(col.pal), direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_colour_viridis_c(option = \"%s\", direction = %d%s)",
                                         tolower(col.pal), direction, limit.str)
      } else if (col.pal %in% brewer.pals) {
        palette <- ggplot2::scale_colour_distiller(palette = col.pal, direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_colour_distiller(palette = \"%s\", direction = %d%s)",
                                         col.pal, direction, limit.str)
      }
    } else {
      if (col.pal %in% viridis.pals) {
        palette <- ggplot2::scale_colour_viridis_d(option = tolower(col.pal), direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_colour_viridis_d(option = \"%s\", direction = %d%s)",
                                         tolower(col.pal), direction, limit.str)
      } else if (col.pal %in% brewer.pals) {
        palette <- ggplot2::scale_colour_brewer(palette = col.pal, direction = direction, limits = limits)
        attr(palette, "code") <- sprintf("ggplot2::scale_colour_brewer(palette = \"%s\", direction = %d%s)",
                                         col.pal, direction, limit.str)
      }
    }
  }

  return(palette)
}

