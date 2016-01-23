create.inz.shapemapplot = function(obj) {
    df = obj$df
    opts = obj$opts
    pf = opts$plot.features
    ## Set the colours for countries `y` based on value `x` (in `df`):
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
    xlim = range(latlon[,1])
    ylim = range(latlon[,2])
    
    if(pf$full.map == FALSE)
    {
        ##only the subsetting region(colored.region)
        plot.logic.fill = pf$shape.object$region %in% df$y
        polygon.index.fill = rep(plot.logic.fill,pf$shape.object$col.index)
        points.fill = rep(polygon.index.fill,pf$shape.object$each)
        latlon.fill = latlon[points.fill,]
        each.fill = pf$shape.object$each[polygon.index.fill]
        col.index.fill = pf$shape.object$col.index[plot.logic.fill]
        col.fill = col.full[polygon.index.fill]
        ## the bbox for latlon.fill
        xlim.fill = range(latlon.fill[,1])
        ylim.fill = range(latlon.fill[,2])
        x.range.logic = latlon[,1] > xlim.fill[1] & latlon[,1] < xlim.fill[2]
        y.range.logic = latlon[,2] > ylim.fill[1] & latlon[,2] < ylim.fill[2]
        with.range.logic = x.range.logic & y.range.logic
        country.rep = rep(rep(pf$shape.obj$region,pf$shape.object$col.index),pf$shape.object$each)
        table.country = table(country.rep[with.range.logic])
        country.draw = names(table.country[table.country > 1])
        big.region = c('Russia','Antarctica')
        
        
        ###sweoifjewiofjewoifjewiofjewiofoefwjf
        if(any(big.region %in% pf$shape.object$region))
        {
            country.na = country.draw[!(pf$shape.object$region %in% 'China')]
            print(country.draw)
            print(country.na)
        }
        ## still plot the country within the viewport(na.region.plot)
        country.na = country.draw[country.draw %in% pf$shape.object$region]
        plot.logic.na = pf$shape.object$region %in% country.na
        polygon.index.na = rep(plot.logic.na,pf$shape.object$col.index)
        ## still draw the region/country if the country lies within the map
        points.na = rep(polygon.index.na,pf$shape.object$each)
        latlon.na = latlon[points.na,]
        each.na = pf$shape.object$each[polygon.index.na]
        col.index.na = pf$shape.object$col.index[plot.logic.na]
        col.na = col.full[polygon.index.na]  
        ###final passing
        latlon.sub = rbind(latlon.fill,latlon.na)
        each.sub = append(each.fill,each.na)
        col.index.sub = append(col.index.fill,col.index.na)
        col = append(col.fill,col.na)
        pf$shape.object$latlon = latlon.sub 
        pf$shape.object$each = each.sub
        pf$shape.object$col.index = col.index.sub
        xlim = xlim.fill
        ylim = ylim.fill

        ##define for 'big region'
        big.region = c('Russia','Antarctica')

        print(xlim)

    }


    ## cols = col.fun(pf$shape.object$col.fun, pf$shape.object$col.args)
    latlon = pf$shape.object$latlon
    shp.obj = color.bind(col, pf$shape.object)
    

    ## missing data
    v = colnames(df)
    missing = is.na(df$x)
    n.missing = sum(missing)
    df = df[!missing, ]
    
    ## information extraction
    ## I used the xylim within transformed data, instead of from inz.plot.....
    ## should be re-write in the future

    
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
    xlim = obj$xlim
    ylim = obj$ylim

    wh = win.ratio()
    vp = viewport(0.5,0.5,width = wh[1], height = wh[2],name = 'VP:PLOTlayout', xscale = xlim,yscale = ylim)
    pushViewport(vp)
    grid.polygon(latlon[,1], latlon[,2], default.units = "native", id.length = shade.each,
                 gp = gpar(col = 'black', fill  = cols))
            
}
