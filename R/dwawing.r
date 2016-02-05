drawing.option = function(s.obj,bar.obj,name,latlon,cols,shade.each,region.name,value,center.x,center.y,x.shift,y.shift)
{
    full.option = c('bar','r','v','b')
    switch(name,
        'bar' = 
        {
            xmax = 0.004 * diff(range(latlon[,1]))
            ymax = 0.1 * diff(range(latlon[,2]))
            grid.polygon(bar.obj$d1[,1],bar.obj$d1[,2],default.units = "native", id.length = bar.obj$each,
                        gp = gpar(col = '#B29980', fill  = bar.obj$col))
            out.str = countrycode(region.name, "country.name", "iso3c")
            center.y = center.y - diff(y.shift) * 0.01            
        },
        'r' = 
        {
            out.str = region.name
        },
        'v' = 
        {
            center.x = center.x[!is.na(value)]
            center.y = center.y[!is.na(value)]
            out.str = value[!is.na(value)]
        },
        'b' = 
        {
            value[is.na(value)] = ''
            out.str = ifelse(value == '',paste(region.name),paste(region.name,value,sep = '\n'))       
        }
    )
    if(name %in% full.option){
        grid.text(out.str, x = center.x, y =center.y,
                just = "centre",default.units = "native",
                gp=gpar(fontsize=9), check=TRUE)
    }
}
    
