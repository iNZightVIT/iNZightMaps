shape.extract = function(shp)
{
    polygon.data = list()
    j = 0
    color = 0
    col.index = 0
    poly.out = length(shp@polygons)
    index = 0
    for(i in 1:poly.out)
    {
        poly.in = length(shp@polygons[[i]]@Polygons)
        col.index[i] = poly.in
        for(ii in 1:poly.in)
        {
            j = j + 1
            polygon.data[j] = list(shp@polygons[[i]]@Polygons[[ii]]@coords)
            index[j] = dim(polygon.data[[j]])[1]
        }
        
    }
    poly.index = rep(1:j,index)
    latlon = do.call(rbind,polygon.data)
    latlon.data = data.frame(latlon = latlon)
	colnames(latlon.data) = c('lon.x','lat.y')
	country = shp[[5]]		
    obj = list(latlon = latlon.data,each = index,country = country,col.index = col.index)
	obj
}


data.trans = function(x,transform = 'linear')
{
    a = x
    b = a - min(a,na.rm = TRUE)
    switch(transform,
        linear = {b = b},
        log = {b = log(b + 1)},
        sqrt = {b = sqrt(b)},
        exp = {b = exp(b)},
        power = {b = b^2},
        normal = 
        {
            x = (a - mean(a,na.rm = TRUE))/sd(a,na.rm = TRUE)
            b = dnorm(x,0,1)
            b = b - min(b,na.rm = TRUE)
        },
    )
    percent.data = b/diff(range(b,na.rm = TRUE))
    percent.data
}

order.match = function(unmatch.data,shp.region,data.region)
{
    order = match(shp.region,data.region)
    orderd.data = unmatch.data[order]
    na.data = shp.region[is.na(orderd.data)]
    print(paste('number of unmatch region:',length(na.data)))
    orderd.data
}


col.fun = function(percent.data,color.index,
                    display = 'hue',na.fill = 'white',offset = 0,col = 'red')
{
    impossible.number = 0.091823021983
    bio.color = c('bi.polar','cm.colors	')
    if(display %in% bio.color)
    {
        fill.float = ifelse(is.na(percent.data) == TRUE, impossible.number,percent.data)
    }else
    {
        fill.float = ifelse(is.na(percent.data) == TRUE, impossible.number,percent.data * (1 - offset) + (offset))
    }
    
    ###the color can not be offset if it is bio-color

    ###display transform
    if(display == 'hue')
    {
        char.col.trans = col2rgb(col)/255
        fill.col =rgb(char.col.trans[1],char.col.trans[2],char.col.trans[3],fill.float)
    }
    
    switch(display,
    
        hcl = {fill.col = hcl(as.numeric(fill.float)*100,l = 85)},
        
        hue = 
        {
            char.col.trans = col2rgb(col)/255
            fill.col =rgb(char.col.trans[1],char.col.trans[2],char.col.trans[3],fill.float)
        },
        
        heat = 
        {
            over.col = heat.colors(length(color.index) * 100)
            orderd.col = over.col[length(over.col):1]
            id = round(fill.float * length(color.index) * 100)
            fill.col = orderd.col[id]
        },
        
        rainbow = 
        {
            over.col = rainbow(length(color.index) * 100)
            orderd.col = over.col
            id = round(fill.float * length(color.index) * 100)
            fill.col = orderd.col[id]            
        },
        
        terrain.colors = 
        {
            over.col = terrain.colors(length(color.index) * 100)
            orderd.col = over.col
            id = round(fill.float * length(color.index) * 100)
            fill.col = orderd.col[id]	            
        },
        
        topo.colors = 
        {
            over.col = topo.colors(length(color.index) * 100)
            orderd.col = over.col
            id = round(fill.float * length(color.index) * 100)
            fill.col = orderd.col[id]
        },
        
        cm.colors = 
        {
            over.col = cm.colors(length(color.index) * 100)
            orderd.col = over.col
            id = round(fill.float * length(color.index) * 100)
            fill.col = orderd.col[id]
        },
        
        bi.polar = 
        {
            col.center = mean(fill.float)
            fill = ''
            re.scale= 1 / max(abs(fill.float - col.center))
            alpha = ifelse(fill.float >= col.center,
                            (fill.float- col.center) * re.scale,
                            (col.center - fill.float) * re.scale
                           )
            fill.col = ifelse(fill.float >= col.center,
                             rgb(1,0,0,alpha = alpha),
                             rgb(0,0,1,alpha = alpha)
                          )
        },
        
        gray =  
        {
            fill.col = gray(round(1 - as.numeric(fill.float),5))
        },
        
        r = 
        {
            r = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
            g = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
            b = ifelse(is.na(order) == TRUE, impossible.number,runif(order))
            fill.col = rgb(r,g,b)	
        },
        
        n = 
        {
            r = runif(length(shp@polygons))
            g = runif(length(shp@polygons))
            b = runif(length(shp@polygons))
            na.fill = rgb(r,g,b)
            fill.col = rgb(r,g,b)	
        }
        
    )
    color.each = ifelse(fill.float== impossible.number, na.fill, fill.col)
    color.out = rep(color.each,color.index)	
    color.out
}


color.bind = function(color,obj)
{
    with.color = list(obj = obj, color = color)
    with.color
}


function(){
e = rep(FALSE,length(m.split))
for(i in 1:length(m.split)){
    for(j in 1:length(s.split)){
        e[i] = any(c(any(m.split[[i]] %in% s.split[[j]]),e[i]))

    }
}
missing.name[e]
}
