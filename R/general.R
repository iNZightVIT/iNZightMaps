####wrap function for LatLon2XY
##' desc
##'
##' det
##' @title title
##' @param data 
##' @param map 
##' @return something
##' @author tell029
latlon.xy = 
function(data,map)
{
        zoom = map$zoom
        LatLon2XY.centered(map, data[,1], data[,2], zoom = zoom)
}


####gat a newmap and assign to global env
##' desc
##'
##' det
##' @title title
##' @param xlim 
##' @param ylim 
##' @param SCALE 
##' @param type 
##' @param zoom 
##' @return something
##' @author tell029
getNewMap <- function(xlim, ylim, SCALE, type,zoom) {	

	map <- GetMap(center = c(mean(ylim),mean(xlim)), size = Get.map.size(xlim,ylim)$size,zoom = zoom,maptype = type,SCALE =SCALE)
	global.objects$maps$map= map
	assign("global.objects", global.objects, envir = .GlobalEnv)
}



####return TRUE if we something changed or firt plotting
####FALSE if nothing is changed 
##' desc
##'
##' det
##' @title title
##' @param bbox 
##' @param window 
##' @param size 
##' @param SCALE 
##' @param type 
##' @return something
##' @author tell029
needNewMap <- function(bbox,window,size,SCALE,type)
{
	need = FALSE
	map.odd = global.objects$maps$map
	##check for the map object
	if(any(map.odd == NULL))
	{
		#print('no map object!!!')
		need = TRUE
	}
	else
	{
		##check if any null
		if( is.null(global.objects$maps$map.detail$bbox) ||  is.null(global.objects$maps$map.detail$size) || 
			is.null(global.objects$maps$map.detail$scale) || is.null(global.objects$maps$map.detail$type) || 
			is.null(global.objects$maps$map.detail$window))
		{
			#print('something is null~~~~')	
			need = TRUE
		}else
		{
			##individual checking
			a = global.objects$maps$map.detail$bbox
			b = bbox
			if(any(abs(a - b) > 0.5) )
			{
				global.objects$maps$map.detail$bbox = bbox
				#print('BBOX changed!')
				need[1] = TRUE					
			}else
			{
				need[1] = FALSE
			}
		
			if(any(global.objects$maps$map.detail$size != size))
			{
				global.objects$maps$map.detail$size = size
				#print('size changed!')
				need[2] = TRUE
				
			}else
			{
				need[2] = FALSE
			}
			if(global.objects$maps$map.detail$scale != SCALE)
			{
				global.objects$maps$map.detail$scale = SCALE
				#print('scale changed!')
				need[3] = TRUE
				
			}else
			{
				need[3] = FALSE
			}
			
			if(type != global.objects$maps$map.detail$type)
			{
				#print('type changed!')
				global.objects$maps$map.detail$type = type
				need[4] = TRUE
			}else
			{
				need[4] = FALSE
			}
			
			if(any(global.objects$maps$map.detail$window != window))
			{
				#print('window changed!')
				global.objects$maps$map.detail$window = window
				need[5] = TRUE
			}else
			{
				need[5] = FALSE	
			}
		}
		any(need)
	}
}


###return the size of map that we request the map from google
###get the range of latitude/longitude, then transform it into resolution unit, 
###then make it into the same ratio as the window's
###also make sure the size lie on the interval of [0,640]
##' desc
##'
##' det
##' @title title
##' @param latR.odd 
##' @param lonR.odd 
##' @param SCALE 
##' @return something
##' @author tell029
Get.map.size = function(latR.odd,lonR.odd,SCALE)
{
	if(missing(latR.odd) || missing(lonR.odd))
	{
		latR.odd = current.viewport()$xscale
		lonR.odd = current.viewport()$yscale
	}
	if(missing(SCALE))
	{
		SCALE = global.objects$maps$map.detail$scale
	}
	win.size= c(
				convertWidth(current.viewport()$width, "mm", TRUE), 
				convertHeight(current.viewport()$height, "mm", TRUE)
				)


	latR.odd = current.viewport()$yscale  
	lonR.odd = current.viewport()$xscale

	###give an origin size that helps to compute the zoom
	if(win.size[1] > win.size[2])
	{
	size.1 = round(c(640 ,640 * win.size[2] / win.size[1]))
	}else
	{
	size.1 = round(c(640 * win.size[1] / win.size[2], 640))
	}

	####stuff from 'Getmap' and 'Getmap.bbox' from 'rgooglemaps' package
	###Overall it transform the latitude/longitude into the size of resolution
	lat.center = mean(latR.odd)
	lon.center = mean(lonR.odd)
	zoom <- min(MaxZoom(latR.odd, lonR.odd, size.1))
	ll <- LatLon2XY(latR.odd[1], lonR.odd[1], zoom)
	ur <- LatLon2XY(latR.odd[2], lonR.odd[2], zoom)
	cr <- LatLon2XY(lat.center, lon.center, zoom)
	ll.Rcoords <- Tile2R(ll, cr)
	ur.Rcoords <- Tile2R(ur, cr)
	size = 0
	size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X))))
	size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y))))
	###first get a square with the maximum length
	size = c(max(size),max(size))
	###transform the size ratio to be the same as window.size's ratio
	if(win.size[1] > win.size[2])
	{
		size[1] = round(size[2]*(win.size[1]/win.size[2]),0)
		size[2] = size[2]	
	}else
	{
		size[2] = round(size[1]*(win.size[2]/win.size[1]),0)
		size[1] = size[1]
	}
	
	##rearrange the ratio if any > 640
	size.final = size
	if(size[1] > 640) {size.final = round(c(640,640 * win.size[2]/win.size[1]))}
	if(size[2] > 640) {size.final = round(c(640 * win.size[1]/win.size[2],640))}
	

	ZoomSize = list(zoom = zoom, size = size.final)
	#print(ZoomSize)
	ZoomSize
	###hence the we will get the map with this zoom and size

}

##return the range of window and also the scale of the map
##' desc
##'
##' det
##' @title title
##' @return something
##' @author tell029
map.xylim = function()
{	
	ZoomSize = Get.map.size()
	scale = global.objects$maps$map$SCALE * 2
	size = global.objects$maps$map$size
	offset = 1
	window.xlim = c(-size[1] + offset, size[1] - offset)/(scale)
	window.ylim = c(-size[2] + offset, size[2] - offset)/(scale)
	window.lim = c(window.xlim,window.ylim)
	global.objects$maps$map.detail$xylim = window.lim
	assign("global.objects", global.objects, envir = .GlobalEnv)
	list(window.lim = window.lim)
}
