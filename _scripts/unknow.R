data = data.1
a = rep(rep(1:length(region),col.index),each)
b = rep(rep(region %in% data$Country,col.index),each)
bar.col = c('#E0FFFF','#FAFAD2','#FFA07A','#C71585')

s.region = rownames(table(as.character(region[as.numeric(rownames(table(a[b])))])))
region.lim = do.call(rbind,lapply(s.region,region.bbox1,obj = obj))
ind = match(s.region,obj$center.region$i.region)
x = obj$center.region$lon.x[ind]
y = obj$center.region$lat.y[ind]


l = apply(region.lim,1,function(x)c(diff(x[1:2]),diff(x[3:4])))

s.length.x = ifelse(xmax > l[1,] * 0.85, l[1,] * 0.85, xmax )/2
plot.num = as.character(length(var))

switch(plot.num,
       '1' = 
       {
         xl = x - s.length.x
         xr = x + s.length.x
       },
       '2' = {
         xl.1 = x - 2 * s.length.x
         xr.1 = x
         xl.2 = x
         xr.2 = x + 2 * s.length.x
         xl = c(xl.1,xl.2)
         xr = c(xr.1,xr.2)
         
       },
       '3' = {   
         xl.1 = x - 3 * s.length.x
         xr.1 = x - 1 * s.length.x
         xl.2 = x - 1 * s.length.x
         xr.2 = x + 1 * s.length.x
         xl.3 = x + 1 * s.length.x
         xr.3 = x + 3 * s.length.x   
         xl = c(xl.1,xl.2,xl.3)
         xr = c(xr.1,xr.2,xr.3)                
       },
       '4' = {
         xl.1 = x - 4 * s.length.x
         xr.1 = x - 2 * s.length.x
         xl.2 = x - 2 * s.length.x
         xr.2 = x
         xl.3 = x
         xr.3 = x + 2 * s.length.x 
         xl.4 = x + 2 * s.length.x  
         xr.4 = x + 4 * s.length.x 
         xl = c(xl.1,xl.2,xl.3,xl.4)
         xr = c(xr.1,xr.2,xr.3,xr.4)  
       }
)

data.in = data[which(data$Country %in% s.region),]

t.length.y = ifelse(ymax > l[2,] * 0.85, l[2,] * 0.85, ymax )
data.t = apply(data.in[,var],2,data.trans)
data.matrix = as.matrix(data.t)
dim(data.matrix) = c(dim(data.matrix)[1] * dim(data.matrix)[2],1)
yt =  rep(y,length(var)) + data.matrix * ymax
yb = y
sep.n = nrow(dataIn)
a = cbind(xl,xr,yb,yt)
e = a[rep(1:dim(a)[1],each = 5) + rep(c(0,dim(a)[1],dim(a)[1],0,0),dim(a)[1])]
f = a[rep(1:dim(a)[1],each = 5) + rep(c(2*dim(a)[1],2*dim(a)[1],3*dim(a)[1],3*dim(a)[1],2*dim(a)[1]),dim(a)[1])]
d1 = cbind(e,f)
ee1 = rep(5,dim(a)[1])
col = rep(bar.col,each = sep.n)
bar.obj = list(d1 = d1,col = col,each = ee1)

