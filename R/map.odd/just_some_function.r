
###works from eric's function
getLon = function(data) { colnames(data)[grepl("^[Ll][Oo][Nn].*$", colnames(data))] }
getLat = function(data) { colnames(data)[grepl("^[Ll][Aa][Tt].*$", colnames(data))] }




###getting the range 
map.bbox = function(data)
{
  lo.lim = range(data[getLon(data)],na.rm = TRUE)
  la.lim = range(data[getLat(data)],na.rm = TRUE)
  cbind(lo.lim,la.lim)
}


###download and drawing the maps, also changing the axis into the lat/lon scale
mapping = function(map.box,plot.p = FALSE,window.l,window.h, SCALE = 1,maptype = 'terrain',zoom = 5)
{
  layout(mat = 1,width = (window.l), height = (window.h))
  mar = c(0,0,0,0)
  ###getting the maps
  MyMap = GetMap.bbox(map.box[,1],map.box[,2],MINIMUMSIZE = TRUE,SCALE = SCALE,zoom = zoom,maptype = maptype)
  if(plot.p == TRUE){
    PlotOnStaticMap(MyMap, lat = data$Latitude, lon = data$Longitude, 
                    axes = FALSE, mar = mar,col = 'red')
  }else{

  }
  #axis(2, at=axTicks(2), yticklabs, cex.axis=0.8, las=1)
  #box()
  MyMap
  
}

###transform the latitude and longitude of the data set into the xy-scale in the plot.
latlon.xy = function(data,map)
{
	zoom = map$zoom
	LatLon2XY.centered(map, data[,1], data[,2], zoom = zoom)
}

in.maps.range = function(extra = 0)
{
	latlon.range = cbind(maps$map$BBOX$ll,maps$map$BBOX$ur)
	range.odd = LatLon2XY.centered(maps$map,lat = latlon.range[c(1,3)],lon = latlon.range[c(2,4)],zoom = maps$map$zoom)
	range.newX = range.odd$newX + extra * range.odd$newX
	range.newY = range.odd$newY + extra * range.odd$newY
	c(range.newX,range.newY)
}

