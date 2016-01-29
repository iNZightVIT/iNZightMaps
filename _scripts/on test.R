name = as.character(center$i.region)
bbox = sapply(name,region.bbox,obj = obj)
center = obj$center.region
lon.ori = center[,1]
lat.ori = center[,2]

var = data.1$Cellphones
percent.data = data.trans(var)
lat.max = 0.1 * diff(range(obj$latlon[,2]))
lat.length = lat.max * percent.data
lat.upper = as.numeric(lat.ori) + lat.length[order.match(name,d.region)]
lat.lower = lat.ori

lon.length = 0.01 * diff(range(obj$latlon[,1]))
lon.left = lon.ori -  lon.length
lon.right = lon.ori + lon.length

bar.polygon = function(var,d.region,col = 'blue', center,obj)
{
  lat.ori = center[,2]
  lon.ori = center[,1]
  name = as.character(center$i.region)
  sapply(name,region.bbox,obj = obj)
  
  x.orderd = order.match(d.region,obj$region)
  
  
  percent.data = data.trans(var)
  lat.max = 0.1 * diff(range(obj$latlon[,2]))
  lat.length = lat.max * percent.data
  lat.upper = lat.ori + lat.length
  print(lat.upper)
  
}



