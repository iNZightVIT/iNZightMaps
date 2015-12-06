##' @title Create a map object to plot
##' @param obj an object from within iNZightPlot
##' @return Object
##' @author Tom Elliott
##' @import iNZightPlots
##' @import grid
##' @export
create.inz.mapplot <- function(obj) {
    out <- NextMethod()

    out$draw.axes <- FALSE
    out$global.object <- list("map" = NULL)
    
    class(out) <- c("inzmap", class(out))

    out
}

##' @export
plot.inzmap <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args

    map <- parent.frame()$global.object$map
    get.newmap <- FALSE
    if (is.null(map)) {
        #get.newmap <- TRUE
    } else {
        ## check valid map
    }

    if (get.newmap) {
        ## DL new map
        map <- something()
    }
        
    if (length(obj$x) == 0)
        return()

    ptCols <- iNZightPlots:::colourPoints(obj$colby, col.args, opts)

    NotInView <- obj$x < min(xlim) | obj$x > max(xlim) | obj$y < min(ylim) | obj$y > max(ylim)
    obj$pch[NotInView] <- NA
    grid.points(obj$x, obj$y, pch = obj$pch,
                gp =
                gpar(col = ptCols,
                     cex = obj$propsize,
                     lwd = opts$lwd.pt, alpha = opts$alpha,
                     fill = obj$fill.pt),
                name = "SCATTERPOINTS")

    invisible(NULL)
}
