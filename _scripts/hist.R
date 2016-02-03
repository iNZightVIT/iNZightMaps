var = c('BodyMassIndex_F','ChildrenPerWoman','Populationtotal','Populationdensity')

dataIn[,var]

dataIn = data.4
sd = name.match(dataIn$Country,region)

dataIn = dataIn[dataIn$Country %in% region,]


a = rep(rep(1:length(region),col.index),each)
b = rep(rep(region %in% dataIn$Country,col.index),each)

s.region = rownames(table(as.character(region[as.numeric(rownames(table(a[b])))])))
region.lim = do.call(rbind,lapply(s.region,region.bbox1,obj = obj))
ind = match(s.region,obj$center.region$i.region)
x = obj$center.region$lon.x[ind]
y = obj$center.region$lat.y[ind]
l = apply(region.lim,1,function(x)c(diff(x[1:2]),diff(x[3:4])))
rownames(l) = c('lat','lon') 
colnames(l) = s.region

xmax = 0.003 * diff(range(latlon[,1]))
t.length.x = ifelse(xmax > l[1,] * 0.85, l[1,] * 0.85, xmax )
xl = x - t.length.x/2 - (t.length.x/2)*0.5
xr = x + t.length.x/2 - (t.length.x/2)*0.5

ymax = 0.2 * diff(range(latlon[,2]))
t.length.y = ifelse(ymax > l[2,] * 0.85, l[2,] * 0.85, ymax )
yb = y + 0.002 * diff(range(latlon[,2]))
yt = ymax * data.trans(dataIn$Exports) + y

a = cbind(xl,xr,yb,yt)
b = a[rep(c(1,2,2,1,1,3,3,4,4,3),dim(a)[1])]
dim(b) = c(length(b)/2,2)
ee1 = rep(5,length(b)/5/2)
e = a[rep(1:length(x),each = 5) + c(0,length(x),length(x),0,0)]
f = a[rep(1:length(x),each = 5) + c(2,2,3,3,2)*rep(length(x),5)]
d1 = cbind(e,f)
