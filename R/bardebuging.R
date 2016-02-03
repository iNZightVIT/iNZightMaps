latlon <<- obj$latlon
each <<- obj$each
col.index <<- obj$col.index
region <<- obj$region
var = c('BodyMassIndex_F','ChildrenPerWoman','Populationtotal','Populationdensity')
data = data.2
xmax = 1.08
ymax =  0.2 *90


a = rep(rep(1:length(region),col.index),each)
b = rep(rep(region %in% data$Country,col.index),each)
bar.col = c('#E0FFFF','#FAFAD2','#FFA07A','#C71585')

s.region = rownames(table(as.character(region[as.numeric(rownames(table(a[b])))])))
region.lim = do.call(rbind,lapply(s.region,region.bbox1,obj = obj))
ind = match(s.region,obj$center.region$i.region)
x = obj$center.region$lon.x[ind]
y = obj$center.region$lat.y[ind]

data = data.1
bar.obj = bar.coor(var = var, data = data, x = x, y = y, xmax = xmax, ymax = ymax)




xmax > abs(apply(l,2,diff))
