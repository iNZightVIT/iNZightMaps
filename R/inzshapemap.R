##' draw a shape file ...
##'
##' details...
##' @param obj an object from within iNZightPlot
##' @return Object
##' @author Tom Elliott
##' @import iNZightPlots
##' @import grid maptools
##' @export
create.inz.shapemapplot = function(obj) {

    df = obj$df
    opts = obj$opts
    pf = opts$plot.features
    x.trans = data.trans(df$x, transform = pf$transform)
    x.ord = x.trans[pf$shape.object$ordered]
    col.full = col.fun(
                data = x.ord, 
                color.index = pf$shape.object$col.index, 
                display = pf$col.method,
                col = pf$col,
                offset = pf$col.offset,
                na.fill = pf$na.fill
            )
    col = col.full
    latlon = pf$shape.object$latlon
    extend.ratio = pf$extend.ratio
    #latlon[,1] = lon.rescale(latlon[,1])
    xlim = range(latlon[,1])
    ylim = range(latlon[,2])
    if(pf$full.map == FALSE)
    {
        ## fill
        plot.logic.fill = pf$shape.object$region %in% df$y
        polygon.index.fill = rep(plot.logic.fill,pf$shape.object$col.index)
        points.fill = rep(polygon.index.fill,pf$shape.object$each)
        latlon.fill = latlon[points.fill,]        
        each.fill = pf$shape.object$each[polygon.index.fill]
        col.index.fill = pf$shape.object$col.index[plot.logic.fill]
        col.fill = col.full[polygon.index.fill]
        lim.fill = c(range(latlon.fill[,1]),range(latlon.fill[,2]))
        lim.extend = c(re.scale(lim.fill[1:2],extend.ratio),
                        re.scale(lim.fill[3:4],extend.ratio))
            
        with.range.logic = (latlon[,1] > lim.extend[1]) & (latlon[,1] < lim.extend[2]) & 
                            (latlon[,2] > lim.extend[3]) & (latlon[,2] < lim.extend[4])

        ## in
        id = rep(1:length(pf$shape.object$each),pf$shape.object$each)
        in.match.id = id[with.range.logic]
        in.match.polygon = rownames(table(in.match.id))
        in.polygon.index.na = (1:length(pf$shape.object$each)) %in% in.match.polygon
        in.points.na = rep(in.polygon.index.na,pf$shape.object$each)
        in.latlon.na = latlon[in.points.na,]    
        
        latlon.sub = rbind(latlon.fill,in.latlon.na)
        lim.sub = c(range(latlon.sub[,1]),range(latlon.sub[,2]))
        xlim.sub = range(latlon.sub[,1])
        ylim.sub = range(latlon.sub[,2])
        ## out
        out.range.logic = (latlon[,1] > lim.sub[1]) & (latlon[,1] < lim.sub[2]) & 
                            (latlon[,2] > lim.sub[3]) & (latlon[,2] < lim.sub[4])
        out.match.id = id[out.range.logic]
        out.match.polygon = rownames(table(out.match.id))
        out.polygon.index.na = (1:length(pf$shape.object$each)) %in% out.match.polygon
        out.points.na = rep(out.polygon.index.na,pf$shape.object$each)
        out.latlon.na = latlon[out.points.na,]    
        latlon.sub = rbind(latlon.fill,in.latlon.na,out.latlon.na)
        each.na = append(pf$shape.object$each[in.polygon.index.na],pf$shape.object$each[out.polygon.index.na])
        col.na = append(col.full[in.polygon.index.na],col.full[out.polygon.index.na])
        each.sub = append(each.fill,each.na)
        
        ## big
        country.rep = rep(rep(pf$shape.obj$region,pf$shape.object$col.index),pf$shape.object$each)
        table.country = table(country.rep[with.range.logic])
        country.draw = rownames(table.country[table.country > 0])
        country.na = country.draw[country.draw %in% pf$shape.object$region]
        big.region = c('Russia','Antarctica')
        if(any(big.region %in% country.na))
        {
            plot.logic.big = pf$shape.object$region %in% big.region
            polygon.index.big = rep(plot.logic.big,pf$shape.object$col.index)
            points.big = rep(polygon.index.big,pf$shape.object$each)
            in.points.na[points.big] = FALSE
            ## prepare for updating
            latlon.big = latlon[in.points.na,]     
            xlim.sub = range(latlon.big[,1])
            ylim.sub = range(latlon.big[,2])
        }
        ## may need
        #col.index.sub = append(col.index.fill,col.index.na)
        #pf$shape.object$col.index = col.index.sub
        col = append(col.fill,col.na)
        pf$shape.object$latlon = latlon.sub 
        pf$shape.object$each = each.sub
        xlim = xlim.sub
        ylim = ylim.sub
    }
    latlon = pf$shape.object$latlon
    shp.obj = color.bind(col, pf$shape.object)
    ## cols = col.fun(pf$shape.object$col.fun, pf$shape.object$col.args)
    ## missing data
    v = colnames(df)
    missing = is.na(df$x)
    n.missing = sum(missing)
    df = df[!missing, ]
    out = list(x = xlim, y = ylim, colby = shp.obj$color,
                n.missing = n.missing, xlim = xlim, ylim = ylim,
                shape.object = shp.obj$obj,col = col)
                
    class(out) = c("inzshapemap", "inzmap", "inzscatter")
    out$draw.axes = FALSE
    out
}


##' draw a shape file ...
##'
##' details...
##' @title Plot an iNZight Shape Map
##' @param obj object passed from iNZightPlot
##' @param gen other options passed from iNZightPlot
##' @return NULL
##' @author Jason Wen
##' @export
plot.inzshapemap = function(obj, gen) {
    latlon = obj$shape.object$latlon
    cols = obj$colby
    shade.each = obj$shape.object$each
    ##limit
    xlim = obj$xlim
    ylim = obj$ylim
    wh = win.ratio()
    vp = viewport(0.5,0.5,width = wh[1], height = wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim,clip = 'on')
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
    popViewport()
    
    grid.rect(gp = gpar(fill = 'transparent'))
}
