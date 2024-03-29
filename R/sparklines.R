##' Plots a minimalist time series line chart at each coordinate
##' provided
##'
##' `geom_sparkline` plots a small time series chart on points on an `sf` map.
##'
##' @title Sparkline layer for each map point
##' @param mapping Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
##' @param data The data to be displayed in this layer
##' @param stat The statistical transformation to use on the data for this layer, as a string.
##' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
##' @param na.rm 	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
##' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
##' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them.
##' @param ... Other arguments passed on to layer().
##' @return A sparkline layer
##' @export
geom_sparkline <- function(mapping = ggplot2::aes(), data = NULL, stat = "sf",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  mapping$geometry <- as.name("geometry")

  c(
    ggplot2::layer(
      geom = GeomSparkLine,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = if (is.character(show.legend)) TRUE else show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        legend = if (is.character(show.legend)) show.legend else "line",
        ...
      )
    ),
    ggplot2::coord_sf(default = TRUE)
  )
}

GeomSparkLine <- ggplot2::ggproto("GeomSf", ggplot2::Geom,
  required_aes = c(
    "geometry",
    "line_x", # THIS HAS TO BE LINE_X...
    "line_y",
    "group"
  ),
  default_aes = ggplot2::aes(
    shape = NULL,
    colour = NULL,
    fill = NULL,
    size = NULL,
    linetype = 1,
    alpha = NA,
    fill_alpha = NA,
    stroke = 0.5,
    plot_size = 1,
    sparkline_type = "Absolute",
    relative_start = 1,
    confint = FALSE,
    up.to = NULL
  ),
  draw_panel = function(data, panel_params, coord, legend = NULL) {
    if (!inherits(coord, "CoordSf")) {
      stop("geom_sf() must be used with coord_sf()", call. = FALSE)
    }

    # Need to refactor this to generate one grob per geometry type
    coord <- coord$transform(data, panel_params)

    ## Scale all lines to have the same ranges
    x.rng <- range(coord$line_x)
    y.rng <- range(coord$line_y)
    coord$line_x <- (coord$line_x - x.rng[1]) / (x.rng[2] - x.rng[1])
    coord$line_y <- (coord$line_y - y.rng[1]) / (y.rng[2] - y.rng[1])

    sparkline_type <- coord$sparkline_type[1]
    up.to <- coord$up.to[1]
    relative_start <- coord$relative_start[1]
    if (sparkline_type == "Relative") {
      coord$line_y <- unlist(tapply(coord$line_y, coord$group, function(x) x - x[relative_start]))
    } else if (sparkline_type == "Percent Change") {
      coord$line_y <- unlist(tapply(coord$line_y, coord$group, function(x) {
        x.lagged <- c(x[1], x[-length(x)])
        (x - x.lagged) / x.lagged
      }))
    }


    ## Size of each sparkline's viewport (in native)
    vp.width <- 0.05 * coord$plot_size[1]
    vp.height <- 0.025 * coord$plot_size[1]

    ## Extract out the position of each group's centroid and
    ## hence the bounding box of the line graph
    unique.points <- match(unique(coord$group), coord$group)
    curr.pos <- matrix(ncol = 6, nrow = length(unique.points))
    colnames(curr.pos) <- c("x", "y", "x0", "y0", "x1", "y1")
    curr.pos[, c("x", "y")] <- sf::st_coordinates(coord[unique.points, "geometry"])
    curr.pos[, "x0"] <- curr.pos[, "x"] - vp.width / 2
    curr.pos[, "y0"] <- curr.pos[, "y"] - vp.height / 2
    curr.pos[, "x1"] <- curr.pos[, "x"] + vp.width / 2
    curr.pos[, "y1"] <- curr.pos[, "y"] + vp.height / 2

    ## Make sure none of the bounding boxes overlap
    cent.coords <- as.matrix(ggrepel:::repel_boxes(
      data_points = curr.pos[, c("x", "y"), drop = FALSE],
      point_padding_x = 0.0,
      point_padding_y = 0.0,
      boxes = curr.pos[, 3:6, drop = FALSE],
      xlim = c(0, 1),
      ylim = c(0, 1),
      hjust = 0, vjust = 0,
      direction = "both"
    ))

    ## Viewport of each line graph
    cent.vp <- apply(cent.coords, 1, function(row) {
      grid::viewport(row["x"], row["y"],
        width = vp.width, height = vp.height,
        default.units = "native"
      )
    })

    groups <- unique(coord$group)
    grobs <- lapply(1:length(groups), function(i) {
      has.moved <- !isTRUE(all.equal(curr.pos[i, c("x", "y")],
        cent.coords[i, c("x", "y")],
        tolerance = 0.01
      ))
      ## sf_sparkline2(coord[coord$group == groups[i], , drop = FALSE],
      ## vp = cent.vp[[i]], has.moved = has.moved)
      sf_sparkline3(coord[coord$group == groups[i], , drop = FALSE],
        vp = cent.vp[[i]], has.moved = has.moved,
        up.to.index = up.to
      )
    })

    do.call(grid::gList, grobs)
  },
  draw_key = function(data, params, size) {
    data <- utils::modifyList(ggplot2:::default_aesthetics(params$legend), data)
    draw_key_path(data, params, size)
  }
)


sf_sparkline <- function(coords, vp, has.moved = FALSE) {
  row <- coords[1, , drop = FALSE]
  geometry <- row$geometry[[1]]

  row <- utils::modifyList(ggplot2::GeomLine$default_aes, row)

  rect_grob <- grid::roundrectGrob(gp = grid::gpar(col = NA, fill = scales::alpha(row$fill, row$fill_alpha)), vp = vp)

  line_grob <- grid::linesGrob(coords$line_x,
    coords$line_y,
    gp = grid::gpar(
      col = scales::alpha(row$colour, row$alpha),
      lwd = row$size * .pt,
      lty = row$linetype,
      lineend = "butt"
    ),
    default.units = "native",
    vp = vp
  )
  if (has.moved) {
    seg_grob <- grid::segmentsGrob(vp$x, vp$y,
      geometry[1], geometry[2],
      default.units = "native", gp = grid::gpar(col = "blue")
    )
  } else {
    seg_grob <- NULL
  }

  grid::grobTree(rect_grob, line_grob, seg_grob)
}

sf_sparkline2 <- function(coords, vp, has.moved = FALSE, confint = FALSE) {
  row <- coords[1, , drop = FALSE]
  geometry <- row$geometry[[1]]

  row <- utils::modifyList(ggplot2::GeomLine$default_aes, row)

  line_grob <- grid::pathGrob(c(coords$line_x[1], coords$line_x, coords$line_x[length(coords$line_x)]),
    c(0, coords$line_y, 0),
    gp = grid::gpar(
      fill = scales::alpha("tomato", 0.75),
      lwd = 0,
      lty = 0,
      lineend = "butt"
    ),
    default.units = "native",
    vp = vp
  )

  if (has.moved) {
    seg_grob <- grid::segmentsGrob(vp$x, vp$y,
      geometry[1], geometry[2],
      default.units = "native", gp = grid::gpar(col = "blue")
    )
  } else {
    seg_grob <- NULL
  }

  grid::grobTree(line_grob, seg_grob)
}

sf_sparkline3 <- function(coords, vp, has.moved = FALSE, up.to.index = NULL) {
  row <- coords[1, , drop = FALSE]
  geometry <- row$geometry[[1]]

  row <- utils::modifyList(ggplot2::GeomLine$default_aes, row)

  if (!is.null(up.to.index)) {
    coords <- coords[1:up.to.index, ]
  }

  line_grob <- grid::pathGrob(c(coords$line_x[1], coords$line_x, coords$line_x[length(coords$line_x)]),
    c(0, coords$line_y, 0),
    gp = grid::gpar(
      fill = scales::alpha("tomato", 0.75),
      lwd = 0,
      lty = 0,
      lineend = "butt"
    ),
    default.units = "native",
    vp = vp
  )

  if (has.moved) {
    seg_grob <- grid::segmentsGrob(vp$x, vp$y,
      geometry[1], geometry[2],
      default.units = "native", gp = grid::gpar(col = "blue")
    )
  } else {
    seg_grob <- NULL
  }

  grid::grobTree(line_grob, seg_grob)
}

sf_confint <- function(coords, vp, has.moved = FALSE) {
  row <- coords[1, , drop = FALSE]
  geometry <- row$geometry[[1]]

  row <- utils::modifyList(ggplot2::GeomLine$default_aes, row)

  ## line_grob <- grid::pathGrob(c(coords$line_x[1], coords$line_x, coords$line_x[length(coords$line_x)]),
  ## c(0, coords$line_y, 0),
  ## gp = grid::gpar(fill = alpha("tomato", 0.75),
  ## lwd = 0,
  ## lty = 0,
  ## lineend = "butt"),
  ## default.units = "native",
  ## vp = vp)

  line_grob <- grid::linesGrob(coords$line_x, 0,
    gp = grid::gpar(
      fill = "black",
      alpha = 0.40,
      lwd = 1,
      lty = 1,
      lineend = "butt"
    ),
    default.units = "native",
    vp = vp
  )

  ## upper_grob <- grid::linesGrob(coords$line_x, coords$line_y * 1.5,
  ## gp = grid::gpar(fill = alpha("steelblue", 0.90),
  ## lwd = 1,
  ## lty = 1,
  ## lineend = "butt"),
  ## default.units = "native",
  ## vp = vp)
  ##
  ## lower_grob <- grid::linesGrob(coords$line_x, coords$line_y * 0.5,
  ## gp = grid::gpar(fill = alpha("steelblue", 0.90),
  ## lwd = 1,
  ## lty = 1,
  ## lineend = "butt"),
  ## default.units = "native",
  ## vp = vp)

  band_grob <- grid::pathGrob(c(coords$line_x, coords$line_x[nrow(coords)], rev(coords$line_x), coords$line_x[1]),
    c(coords$lower_y, coords$lower_y[nrow(coords)], rev(coords$upper_y), coords$upper_y[1]),
    gp = grid::gpar(
      fill = scales::alpha("tomato", 0.75),
      lwd = 0, lty = 0
    ),
    default.units = "native",
    vp = vp
  )

  grid::grobTree(line_grob, band_grob)
}

GeomConfInt <- ggplot2::ggproto("GeomConfInt", ggplot2::Geom,
  required_aes = c(
    "geometry",
    "line_x", # THIS HAS TO BE LINE_X...
    "lower_y",
    "upper_y",
    "group"
  ),
  default_aes = ggplot2::aes(
    shape = NULL,
    colour = "black",
    fill = NULL,
    size = NULL,
    linetype = 1,
    alpha = NA,
    fill_alpha = NA,
    stroke = 0.5,
    plot_size = 1,
    relative = FALSE,
    pctchange = FALSE,
    confint = TRUE
  ),
  draw_panel = function(data, panel_params, coord, legend = NULL) {
    if (!inherits(coord, "CoordSf")) {
      stop("geom_sf() must be used with coord_sf()", call. = FALSE)
    }

    # Need to refactor this to generate one grob per geometry type
    coord <- coord$transform(data, panel_params)

    ## Scale all lines to have the same ranges
    x.rng <- range(coord$line_x)
    y.rng <- range(coord$lower_y, coord$upper_y)
    coord$line_x <- (coord$line_x - x.rng[1]) / (x.rng[2] - x.rng[1])
    coord$lower_y <- (coord$lower_y - y.rng[1]) / (y.rng[2] - y.rng[1])
    coord$upper_y <- (coord$upper_y - y.rng[1]) / (y.rng[2] - y.rng[1])

    relative <- coord$relative[1]
    pctchange <- coord$pctchange[1]
    if (relative) {
      coord$lower_y <- unlist(tapply(coord$lower_y, coord$group, function(x) x - x[1]))
      coord$upper_y <- unlist(tapply(coord$upper_y, coord$group, function(x) x - x[1]))
    } else if (pctchange) {
      coord$lower_y <- unlist(tapply(coord$lower_y, coord$group, function(x) (x - x[1]) / x[1]))
      coord$upper_y <- unlist(tapply(coord$upper_y, coord$group, function(x) (x - x[1]) / x[1]))
    }


    ## Size of each sparkline's viewport (in native)
    vp.width <- 0.05 * coord$plot_size[1]
    vp.height <- 0.025 * coord$plot_size[1]

    ## Extract out the position of each group's centroid and
    ## hence the bounding box of the line graph
    unique.points <- match(unique(coord$group), coord$group)
    curr.pos <- matrix(ncol = 6, nrow = length(unique.points))
    colnames(curr.pos) <- c("x", "y", "x0", "y0", "x1", "y1")
    curr.pos[, c("x", "y")] <- sf::st_coordinates(coord[unique.points, "geometry"])
    curr.pos[, "x0"] <- curr.pos[, "x"] - vp.width / 2
    curr.pos[, "y0"] <- curr.pos[, "y"] - vp.height / 2
    curr.pos[, "x1"] <- curr.pos[, "x"] + vp.width / 2
    curr.pos[, "y1"] <- curr.pos[, "y"] + vp.height / 2

    ## Make sure none of the bounding boxes overlap
    cent.coords <- as.matrix(ggrepel:::repel_boxes(
      data_points = curr.pos[, c("x", "y")],
      point_padding_x = 0.0,
      point_padding_y = 0.0,
      boxes = curr.pos[, 3:6],
      xlim = c(0, 1),
      ylim = c(0, 1),
      direction = "both"
    ))

    ## Viewport of each line graph
    cent.vp <- apply(cent.coords, 1, function(row) {
      grid::viewport(row["x"], row["y"],
        width = vp.width, height = vp.height,
        default.units = "native"
      )
    })

    groups <- unique(coord$group)
    grobs <- lapply(1:length(groups), function(i) {
      has.moved <- !isTRUE(all.equal(curr.pos[i, c("x", "y")],
        cent.coords[i, c("x", "y")],
        tolerance = 0.01
      ))
      sf_confint(coord[coord$group == groups[i], , drop = FALSE],
        vp = cent.vp[[i]], has.moved = has.moved
      )
    })

    do.call(grid::gList, grobs)
  },
  draw_key = function(data, params, size) {
    data <- utils::modifyList(ggplot2:::default_aesthetics(params$legend), data)
    draw_key_path(data, params, size)
  }
)

geom_confint <- function(mapping = ggplot2::aes(), data = NULL, stat = "sf",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  mapping$geometry <- as.name("geometry")

  c(
    ggplot2::layer(
      geom = GeomConfInt,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = if (is.character(show.legend)) TRUE else show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        legend = if (is.character(show.legend)) show.legend else "line",
        ...
      )
    ),
    ggplot2::coord_sf(default = TRUE)
  )
}
