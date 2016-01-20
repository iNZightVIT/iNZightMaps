##' Transform the latitude and longitude into xy
##'
##' wrap function for \link{LatLon2XY} from 'RgoogleMaps' package
##' @title latlon.xy
##' @param data the data set that use for plotting, the first cloumn needs to be Longitude and the second column needs to be Latitude.
##' @param map a map object from RgoogleMaps package.
##' @return a list that contain the tramsformed x and y.
##' @author Jason Wen
##' @export
latlon.xy = 
function(data,map)
{	
	#data[,2] = lon.rescale(data[,2])
    zoom = map$zoom
    LatLon2XY.centered(map, data[,1], data[,2], zoom = zoom)
}


####gat a new map object and assign to global environment
##' Get a new map and save it into Global Env
##'
##' a wrap function of \link{GetMap} from package 'RgoogleMaps'
##' @title Get an new map object
##' @param lat.lim the range of longitude
##' @param lon.lim the range of latitude
##' @param SCALE variable from \link{GetMap}, use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map 
##' @param variable from \link{GetMap}, type defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile.
##' @param zoom variable from \link{GetMap}, Google maps zoom level.
##' @return a map object
##' @author Jason Wen
getNewMap <- function(lat.lim, lon.lim, SCALE, type,zoom)
{	
	lat.mean = mean(lat.lim)
	lon.mean = mean(lon.lim)
	center = c(lat.mean,lon.mean)
    map <<- GetMap(center = center, size = Get.map.size(lat.lim,lon.lim)$size,zoom = zoom,maptype = type,SCALE =SCALE)
    global.objects$maps$map= map
    assign("global.objects", global.objects, envir = .GlobalEnv)
}



####a simple tranformation for longitude
##' transform latitude inorder to get the biggest map as possibile
##'
##' transform the longitude
##' @title transform the latitude
##' @param lon a numeric value or a numeric vector of longitude
##' @return a new longitude value or vector that used for plotting in map.
##' @author Jason Wen
lon.rescale = function(lon)
{
	lon.range = range(lon,na.rm = TRUE)
	if(lon.range[1] < 0 & lon.range[2] > 0)
	{
		lon = ifelse(lon < -135,lon + 360,lon)
	}
	mean.lon = mean(lon)
	if(mean.lon > 180)
	{
		lon = lon - 360
	}
	lon
}


####return TRUE if we something changed or firt plotting
####FALSE if nothing is changed 
##' Do we need a new map or not
##'
##' A function for checking wheather we need to download a new map from google or not, it checks the input with the global inzight objects, if something if not matched, then return TRUE, otherwise FALSE.
##' @title Need New Map
##' @param bbox a numeric vector of length 4, the range of the pervious latitude and longitude
##' @param window a numeric vector of length 2, the size of the pervious window
##' @param sized a numeric vector of length 2, the size of the pervious map 
##' @param SCALE a numeric vector of length 1, the scale of the pervious map 
##' @param type a character vector of length 1, type the type of the pervious map 
##' @return Logical value TRUE/FALSE TRUE = something are not matched, FALSE = the pervious map is ok for re-use.
##' @author Jason Wen
needNewMap <- function(bbox,window,size,SCALE,type)
{
    need = FALSE
    map.odd = global.objects$maps$map
    ##check for the map object
    if(any(map.odd == NULL))
    {
        need = TRUE
    }
    else
    {
        ##check if any null
        if( is.null(global.objects$maps$map.detail$bbox) ||  is.null(global.objects$maps$map.detail$size) || 
        is.null(global.objects$maps$map.detail$scale) || is.null(global.objects$maps$map.detail$type) || 
        is.null(global.objects$maps$map.detail$window))
        {
            print('something is null~~~~')	
            need = TRUE
        }else
        {
            ##individual checking
            a = global.objects$maps$map.detail$bbox
            b = bbox
            if(any(abs(a - b) != 0) )
            {
            global.objects$maps$map.detail$bbox = bbox
            need[1] = TRUE					
            }else
            {
                need[1] = FALSE
            }

            if(any(global.objects$maps$map.detail$size != size))
            {
                global.objects$maps$map.detail$size = size
                need[2] = TRUE

            }else
            {
                need[2] = FALSE
            }
            if(global.objects$maps$map.detail$scale != SCALE)
            {
                global.objects$maps$map.detail$scale = SCALE
                need[3] = TRUE

            }else
            {
                need[3] = FALSE
            }
            if(type != global.objects$maps$map.detail$type)
            {
                global.objects$maps$map.detail$type = type
                need[4] = TRUE
            }else
            {
                need[4] = FALSE
            }

            if(any(global.objects$maps$map.detail$window != window))
            {
                global.objects$maps$map.detail$window = window
                need[5] = TRUE
            }
			else
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
##' @param latR a numeric vector of length 2, the range of Latitude
##' @param lonR a numeric vector of length 2, the range of Longitude
##' @param SCALE variable from GetMap, use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map 
##' @return a list that contain the size of the map(in pixels), and the zoom level
##' @author Jason Wen
Get.map.size = function(latR,lonR,SCALE)
{

    if(missing(SCALE))
    {
        SCALE = global.objects$maps$map.detail$scale
    }
    win.size= c(
        convertWidth(current.viewport()$width, "mm", TRUE), 
        convertHeight(current.viewport()$height, "mm", TRUE)
    )

    ###give an origin size that helps to compute the zoom
    if(win.size[1] > win.size[2])
    {
        size.1 = round(c(640 ,640 * win.size[2] / win.size[1]))
    }else
    {
        size.1 = round(c(640 * win.size[1] / win.size[2], 640))
    }
    
	lat.range = range(latR)
	lon.range = range(lonR)
	if(lon.range[1] < 0 & lon.range[2] > 0)
	{
		lon.range[1] = lon.range[1] + 360 
	}
	zoom <- min(MaxZoom(lat.range, lon.range, size.1))

	
    ll <- LatLon2XY(latR[1], lonR[1], zoom)
    ur <- LatLon2XY(latR[2], lonR[2], zoom)
    cr <- LatLon2XY(mean(latR), mean(lonR), zoom)
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

	if(size[1] > size[2])
	{
		size.final = round(c(640,640 * win.size[2]/win.size[1]))
	}else
	{
		size.final = round(c(640 * win.size[1]/win.size[2],640))
	}
	
    ZoomSize = list(zoom = zoom, size = size.final)
    ZoomSize
    ###hence the we will get the map with this zoom and size

}

##' return the limit of x-axis and y-axis of the plot.
##'
##' the map.xylim function does not contain any arguments, and it returns the limit of x-axis and y-axis after the plot called.
##' @title get the limit of x-axis and y-axis
##' @return return a list that contain the limit of x-axis and y-axis 
##' @author tell029
map.xylim = function(latR,lonR,SCALE)
{	
    ZoomSize = Get.map.size(latR = latR,lonR = lonR,SCALE = SCALE)
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