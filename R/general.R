##' Transform the latitude and longitude into xy
##'
##' wrap function for LatLon2XY from 'RgoogleMaps' package
##' @title latlon.xy
##' @param data  the data set that use for plotting, the first cloumn needs to be Longitude and the second column needs to be Latitude.
##' @param map   a map object from RgoogleMaps package.
##' @return a list that contain the tramsformed x and y.
##' @author Jason Wen
##' @export
latlon.xy = 
function(data,map)
{
        zoom = map$zoom
        LatLon2XY.centered(map, data[,1], data[,2], zoom = zoom)
}


####gat a new map object and assign to global env
##' Get a new map and save it into Global Env
##'
##' det
##' @title Get an new map object
##' @param xlim the range of latitude
##' @param ylim the range of longitude
##' @param SCALE variable from GetMap, use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map 
##' @param variable from GetMap, type defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile.
##' @param zoom variable from GetMap, Google maps zoom level.
##' @return a map object
##' @author Jason Wen
getNewMap <- function(xlim, ylim, SCALE, type,zoom) {	

	map <- GetMap(center = c(mean(xlim),mean(ylim)), size = Get.map.size(xlim,ylim)$size,zoom = zoom,maptype = type,SCALE =SCALE)
	global.objects$maps$map= map
	assign("global.objects", global.objects, envir = .GlobalEnv)
}



####return TRUE if we something changed or firt plotting
####FALSE if nothing is changed 
##' Do we need a new map or not
##'
##' A function for checking wheather we need to download a new map from google or not, it checks the input with the global inzight objects, if something if not matched, then return TRUE, otherwise FALSE.
##' @title Need New Map
##' @param bbox a numeric vector of length 4, the range of the pervious latitude and longitude
##' @param window a numeric vector of length 2, the size of the pervious window
##' @param size a numeric vector of length 2, the size of the pervious map 
##' @param SCALE a numeric vector of length 1, the scale of the pervious map 
##' @param a character vector of length 1, type the type of the pervious map 
##' @return Logical value TRUE/FALSE TRUE = something are not matched, FALSE = the pervious map is ok for re-use.
##' @author Jason Wen
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
##' compute the size and the zoom level that needs for request a new map.
##'
##' since the size of the map should not greater than 640, the size and the zoom needs to be transform before pass to the getNewMap function. 
##' @title Get the size of the map in pixels
##' @param latR.odd a numeric vector of length 2, the range of Latitude
##' @param lonR.odd a numeric vector of length 2, the range of Longitude
##' @param SCALE variable from GetMap, use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map 
##' @return a list that contain the size of the map(in pixels), and the zoom level
##' @author Jason Wen
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


	latR.odd = current.viewport()$xscale  
	lonR.odd = current.viewport()$yscale

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

##' return the limit of x-axis and y-axis of the plot.
##'
##' the map.xylim function does not contain any arguments, and it returns the limit of x-axis and y-axis after the plot called.
##' @title get the limit of x-axis and y-axis
##' @return return a list that contain the limit of x-axis and y-axis 
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
