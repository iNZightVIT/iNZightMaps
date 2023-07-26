##' draw a shape file ...
##'
##' details...
##' @param obj an object from within iNZightPlot
##' @return Object
##' @author Tom Elliott
##' @import iNZightPlots maptools
##' @export
create.inz.shapemapplot <- function(obj) {
    df <- obj$df
    ## a <<- df
    data.region <- df$y

    ## warning message
    unique.length <- length(unique(df$y))
    total.length <- length(df$y)
    if (unique.length != total.length) {
        tt <- tapply(df$x, df$y, mean, na.rm = TRUE)
        df <- data.frame(x = as.numeric(tt), y = rownames(tt))
        warning("Multiple matches found, using the mean.")
    }

    opts <- obj$opts
    pf <- opts$plot.features
    sd <- name.match(df[, 2], pf$shape.obj$region)
    pf$shape.object$ordered <- order.match(sd[[1]], sd[[2]])
    x.trans <- data.trans(df$x,
        transform = pf$transform,
        data.range = pf$shape.obj$maths$range,
        mean = pf$shape.obj$maths$mean,
        sd = pf$shape.obj$maths$sd,
        max.prob = pf$shape.obj$maths$prob
    )
    x.ord <- x.trans[pf$shape.object$ordered]
    col <- col.fun(
        data = x.ord, color.index = pf$shape.object$col.index,
        display = pf$col.method, col = pf$col,
        offset = pf$col.offset, na.fill = pf$na.fill
    )
    pf$shape.object$col <- col
    obj <- pf$shape.object
    obj$extend.ratio <- +pf$extend.ratio
    obj$xylim <- c(range(obj$latlon[, 1]), range(obj$latlon[, 2]))
    obj$full.map <- pf$full.map

    v <- colnames(df)
    ## missing data
    missing <- is.na(df$x)
    n.missing <- sum(missing)
    df <- df[!missing, ]

    xlim <- obj$xylim[1:2]
    ylim <- obj$xylim[3:4]

    out <- list(
        x = xlim, y = ylim, colby = obj$col, n.missing = n.missing,
        xlim = xlim, ylim = ylim, shape.object = obj, df = df,
        name = pf$name, zoom = pf$zoom, zoom.center = pf$zoom.center
    )
    class(out) <- c("inzshapemap", "inzmap", "inzscatter")
    out$draw.axes <- FALSE
    out
}


##' draw a shape file ...
##'
##' details...
##' @title Plot an iNZight Shape Map
##' @param x object passed from iNZightPlot
##' @param gen other options passed from iNZightPlot
##' @param ... additional arguments (ignored)
##' @return NULL
##' @author Jason Wen
##' @importFrom stats sd dnorm
##' @importFrom utils modifyList
##' @export
plot.inzshapemap <- function(x, gen, ...) {
    obj <- x
    df <- obj$df
    full.s.obj <- obj$shape.object
    bbox <- full.s.obj$bbox
    s.obj <- full.s.obj
    name <- obj$name
    zoom <- obj$zoom
    zoom.center <- obj$zoom.center

    if (s.obj$full.map == FALSE) {
        ratio <- s.obj$extend.ratio
        inner.lim <- innerLim(s.obj, obj$df$y)
        lim.in <- c(
            re.scale(inner.lim[1:2], ratio),
            re.scale(inner.lim[3:4], ratio)
        )

        if (all(lim.in >= bbox)) {
            lim <- win.ratio(lim.in[1:2], lim.in[3:4])
        } else {
            lim.out <- outerLim(s.obj, lim.in)
            lim <- win.ratio(lim.out[1:2], lim.out[3:4])
            s.obj <- subByLim(s.obj, lim)
        }

        ## make sure the bar within the viewport
        if (name == "bar") {
            latlon <- s.obj$latlon
            ## bar.obj = bar.coor(var = var, data = dataIn, x = xx, y = yy, xmax = xmax, ymax = ymax)
            xmax <- 0.004 * diff(range(latlon[, 1]))
            ymax <- 0.1 * diff(range(latlon[, 2]))
            lim.in <- c(lim.in[1:3], max(lim.in[4], max(bar.obj$d1[, 2], na.rm = TRUE)))
        }
    } else {
        lim <- win.ratio(s.obj$xylim[1:2], s.obj$xylim[3:4])
    }

    latlon <- s.obj$latlon
    cols <- s.obj$col
    shade.each <- s.obj$each
    center.x <- s.obj$center.region$lon.x
    center.y <- s.obj$center.region$lat.y
    region.name <- s.obj$center.region$i.region
    sd <- name.match(df$y, region.name)
    order <- order.match(sd[[1]], sd[[2]])
    value <- round(df$x[order], 2)

    if (any(is.na(zoom.center))) {
        zoom.center <- c(mean(lim[1:2]), mean(lim[3:4]))
    }
    xlim <- rep(zoom.center[1], 2) + c(-1, 1) * diff(lim[1:2]) * zoom / 2
    ylim <- rep(zoom.center[2], 2) + c(-1, 1) * diff(lim[3:4]) * zoom / 2
    zoom.lim <- c(xlim, ylim)

    ## if lim > bbox then using bbox instead of lim
    if (zoom > 1) {
        if (diff(range(zoom.lim)) > diff(lim[1:2]) &
            diff(range(zoom.lim)) > diff(lim[3:4])) {
            zoom.lim <- lim
        }
    }

    vp <- viewport(0.5, 0.5,
        width = 1, height = 1, name = "VP:MAPSHAPES",
        xscale = zoom.lim[1:2], yscale = zoom.lim[3:4]
    )
    pushViewport(vp)
    ## backagound
    grid.rect(gp = gpar(fill = "#F5F5F5"))

    ## shp polygon
    grid.polygon(latlon[, 1], latlon[, 2],
        default.units = "native", id.lengths = shade.each,
        gp = gpar(col = "#B29980", fill = cols)
    )
    ## other features added into the map
    drawing.features(
        bar.obj = bar.obj,
        latlon = latlon,
        cols = cols,
        shade.each = shade.each,
        region.name = region.name,
        value = value,
        name = name,
        center.x = center.x,
        center.y = center.y,
        data.region = df$y
    )

    grid.rect(gp = gpar(fill = "transparent"))
    upViewport()

    ## global object
    inzshpobj <<- list(
        s.obj = full.s.obj, bar.obj = bar.obj,
        name = name, value = value,
        region.name = region.name, num = 1,
        bbox = bbox, bbox.record = bbox,
        click.point = c(mean(lim[1:2]), mean(lim[3:4])),
        data.region = df$y
    )
}
