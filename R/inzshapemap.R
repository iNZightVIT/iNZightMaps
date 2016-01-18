create.inz.shapemapplot <- function(obj) {
    df <- obj$df
    opts <- obj$opts

    ## missing data
    v <- colnames(df)
    missing <- is.na(df$x)
    n.missing <- sum(missing)
    df <- df[!missing, ]

    ## information extraction
    latlon = opts$plot.features$shape.obj$latlon
    xlim = range(latlon[, 1])
    ylim = range(latlon[, 2])
    colby = opts$plot.features$shape.obj$color

    out <- list(x = xlim, y = ylim, colby = colby, map.type = map.type,
                n.missing = n.missing, xlim = xlim, ylim = ylim, latlon = latlon)
    class(out) <- c("inzshapemap", "inzmap", "inzscatter")
}


##' draw a shape file ...
##'
##' details...
##' @title Plot an iNZight Shape Map
##' @param obj object passed from iNZightPlot
##' @param gen other options passed from iNZightPlot
##' @return NULL
##' @author Jason Wen
##' @import maptools
##' @export
plot.inzshapemap <- function(obj, gen) {
    latlon = obj$latlon
    cols = obj$colby

    shade.id = obj$latlon$id

    ## limit
    xlim = obj$xlim
    ylim = obj$ylim
    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)

    ## compute the ratio
    ratio.map = (diff(xlim) / diff(ylim))
    ratio.win = win.width / win.height
    if (ratio.map < ratio.win) {
        h = unit(1,'npc')
        w = unit(ratio.map/ratio.win, 'npc')
    } else {
        w = unit(1,'npc')
        h = unit(ratio.win/ratio.map, 'npc')
    }

    vp = viewport(0.5, 0.5, width = w, height = h, name = 'VP:PLOTlayout',
                 xscale = xlim, yscale = ylim)
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native",
                 id = shade.id,
                 gp = gpar(col = 'black', fill  = cols))

}
