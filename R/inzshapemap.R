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
    #latlon[,1] = lon.rescale(latlon[,1])
    xlim = range(latlon[,1])
    ylim = range(latlon[,2])
        
    if(pf$full.map == FALSE)
    {
        ## only the subsetting region(colored.region)
        plot.logic.fill = pf$shape.object$region %in% df$y
        polygon.index.fill = rep(plot.logic.fill,pf$shape.object$col.index)
        points.fill = rep(polygon.index.fill,pf$shape.object$each)
        latlon.fill = latlon[points.fill,]        
        each.fill = pf$shape.object$each[polygon.index.fill]
        col.index.fill = pf$shape.object$col.index[plot.logic.fill]
        col.fill = col.full[polygon.index.fill]
        
        ## the bbox for latlon.fill
        lim.fill = c(range(latlon.fill[,1]),range(latlon.fill[,2]))
        with.range.logic = (latlon[,1] > lim.fill[1]) & (latlon[,1] < lim.fill[2]) & 
                            (latlon[,2] > lim.fill[3]) & (latlon[,2] < lim.fill[4])

        ## matching by using polygons
        match.id = rep(1:length(pf$shape.object$each),pf$shape.object$each)[with.range.logic]
        match.polygon = rownames(table(match.id))
        polygon.index.na = (1:length(pf$shape.object$each)) %in% match.polygon
        points.na = rep(polygon.index.na,pf$shape.object$each)
        latlon.na = latlon[points.na,]
        each.na = pf$shape.object$each[polygon.index.na]
        col.na = col.full[polygon.index.na]  
        
        ## prepare for updating
        latlon.sub = rbind(latlon.fill,latlon.na)
        xlim.sub = range(latlon.sub[,1])
        ylim.sub = range(latlon.sub[,2])
        each.sub = append(each.fill,each.na)
        col = append(col.fill,col.na)
        
        ## change the limit 
        country.rep = rep(rep(pf$shape.obj$region,pf$shape.object$col.index),pf$shape.object$each)
        table.country = table(country.rep[with.range.logic])
        country.draw = rownames(table.country[table.country > 0])
        country.na = country.draw[country.draw %in% pf$shape.object$region]
        big.region = c('Russia','Antarctica')
        if(any(big.region %in% country.na))
        {
            print('something')
            plot.logic.big = pf$shape.object$region %in% big.region
            polygon.index.big = rep(plot.logic.big,pf$shape.object$col.index)
            points.big = rep(polygon.index.big,pf$shape.object$each)
            points.fill[points.big] = FALSE
           
            ## prepare for updating
            latlon.big = latlon[points.fill,]     
            xlim.sub = range(latlon.big[,1])
            ylim.sub = range(latlon.big[,2])
            
        }
        ## may need
        #col.index.sub = append(col.index.fill,col.index.na)
        #pf$shape.object$col.index = col.index.sub
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
##' @import maptools
##' @export
plot.inzshapemap = function(obj, gen) {
    latlon = obj$shape.object$latlon
    cols = obj$colby
    shade.each = obj$shape.object$each
    ##limit
    grid.rect()
    xlim = obj$xlim
    ylim = obj$ylim

    wh = win.ratio()
    vp = viewport(0.5,0.5,width = wh[1], height = wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim)
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
            
}
