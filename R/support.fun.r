####wrap function for LatLon2XY
latlon.xy = 
function(data,map)
{
        zoom = map$zoom
        LatLon2XY.centered(map, data[,1], data[,2], zoom = zoom)
}


####gat a newmap and assign to global env
getNewMap <- function(xlim, ylim, SCALE, type,zoom) {	

	map <- GetMap(center = c(mean(ylim),mean(xlim)), size = Get.map.size()$size,zoom = zoom,maptype = type,SCALE =SCALE)
	assign("global.objects", list(map = map), envir = .GlobalEnv)
}



####return TRUE if we something changed or firt plotting
####FALSE if nothing is changed 
needNewMap <- function(bbox,window,size,SCALE,type)
{
	need = FALSE	
	map.odd = global.objects$map
	if(any(map.odd == NULL))
		need = TRUE
	else{
		##checking for latitude/longitude range of the map
		if( is.null(global.objects$map.detail$bbox) ||  is.null(global.objects$map.detail$size) || 
			is.null(global.objects$map.detail$scale) || is.null(global.objects$map.detail$type) || is.null(global.objects$map.detail$window))
		{
			print('something is null~~~~')	
			need = TRUE
		}else
		{
			a = global.objects$map.detail$bbox
			b = bbox
			if(any(abs(a - b) > 0.5) )
			{
				global.objects$map.detail$bbox = bbox
				print('BBOX changed!')
				need[1] = TRUE					
			}else
			{
				need[1] = FALSE
			}
		
			if(any(global.objects$map.detail$size != size))
			{
				global.objects$map.detail$size = size
				print('size changed!')
				need[2] = TRUE
				
			}else
			{
				need[2] = FALSE
			}
			if(global.objects$map.detail$scale != SCALE)
			{
				global.objects$map.detail$scale = SCALE
				print('scale changed!')
				need[3] = TRUE
				
			}else
			{
				need[3] = FALSE
			}
			
			if(type != global.objects$map.detail$type)
			{
				print('type changed!')
				global.objects$map.detail$type = type
				need[4] = TRUE
			}else
			{
				need[4] = FALSE
			}
			
			if(any(global.objects$map.detail$window != window))
			{
				print('window changed!')
				global.objects$map.detail$window = window
				need[5] = TRUE
			}else
			{
				need[5] = FALSE	
			}
		}
		any(need)
	}
}

#####return the limit in plot scale because the plot scale != latitude/longitude
in.maps.range = 
function(extra = 0)
{
        latlon.range = cbind(global.objects$map$BBOX$ll,global.objects$map$BBOX$ur)
        range.odd = LatLon2XY.centered(global.objects$map,lat = latlon.range[c(1,3)],lon = latlon.range[c(2,4)],
					zoom = global.objects$map$zoom)
        range.newX = range.odd$newX + extra * range.odd$newX
        range.newY = range.odd$newY + extra * range.odd$newY
        c(range.newX,range.newY)
}



####haven't finished yet, do not use~~
maps.actual.latlon = function(latR,lonR){
	lat.center <- mean(latR)
	lon.center <- mean(lonR)
	size = c(640,640)
	zoom <- min(MaxZoom(latR, lonR, size))
	print(zoom)
	cr <- LatLon2XY(lat.center, lon.center, zoom)
	ll <- LatLon2XY(latR[1], lonR[1], zoom)
	ur <- LatLon2XY(latR[2], lonR[2], zoom)
	cr <- LatLon2XY(lat.center, lon.center, zoom)
	ll.Rcoords <- Tile2R(ll, cr)
	ur.Rcoords <- Tile2R(ur, cr)
	size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X)))) + 
	1
	size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y)))) + 
	1
	#size[1] = 630
	size[2] = 778
	MyMap <- list(lat.center = lat.center, lon.center = lon.center, 
	zoom = zoom, SCALE = 2)

	BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, 
	-size[2]/2 - 0.5), ur = XY2LatLon(MyMap, size[1]/2 + 
	0.5, size[2]/2 - 0.5))
	latR = range(data.1$Latitude)
	lonR = range(data.1$Longitude)
	ll <- LatLon2XY(latR[1], lonR[1], zoom)
	ur <- LatLon2XY(latR[2], lonR[2], zoom)
	cr <- LatLon2XY(lat.center, lon.center, zoom)
	ll.Rcoords <- Tile2R(ll, cr)
	ur.Rcoords <- Tile2R(ur, cr)
	size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X)))) + 1
	size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y)))) + 1
	zoom <- min(MaxZoom(latR, lonR, size))
	a = GetMap(center = c(lat.center, lon.center),zoom = zoom,size = size)
	
	
	
	BBOX}



###return the size of map that we request the map from google
###issue::
####as long as we transformed a sphere-like coordinate into x-y-like coordinate, hence the transformation is not linear
####hence I can not capture the real-transform ratio(formula) from latitude/longitude into the resolution coordinate
####the good news is, the transformation ratio seems quite close to 1.38, hence I multiple 1.38 for latitude and longitude before transform them	
Get.map.size = function(){
	win.size= c(
				convertWidth(current.viewport()$width, "mm", TRUE), 
				convertHeight(current.viewport()$height, "mm", TRUE)
				)

	latR.odd = range(data.1$Latitude)
	lonR.odd = range(data.1$Longitude)

	lat.length.odd = latR.odd[2] - latR.odd[1]
	lon.length.odd = lonR.odd[2] - lonR.odd[1]
	size = 0
	
	####transformation of the latitude and longitude, make sure the ratio are the same as the plot window size
	####for example, if x > y, then we keep x remain the same, and then transform y to make sure that r = x/y = x.window/y.window
	####then get the new coordinates limit for y
	if (win.size[2] > win.size[1])
	{
		lonR.new = lonR.odd
		ratio = win.size[1]/win.size[2]

		lat.length.new = lon.length.odd / ratio #* 1.38
		latR.new = c(latR.odd[1] - (lat.length.new - lat.length.odd)/2,latR.odd[2] + (lat.length.new - lat.length.odd)/2)
	}
	else
	{
		latR.new = latR.odd
		ratio = win.size[1]/win.size[2]

		lon.length.new = lat.length.odd * ratio #* 1.38
		lonR.new = c(lonR.odd[1] - (lon.length.new - lon.length.odd)/2,lonR.odd[2] + (lon.length.new - lon.length.odd)/2)

	}	

    win.width <- convertWidth(current.viewport()$width, "mm", TRUE)
    win.height <- convertHeight(current.viewport()$height, "mm", TRUE)
    
	####sicne the 'rgoogle' package cannot have a size > 640 hence the transformation is needed
	###here we need to set the pre-map resolution for the map that we request first inorder to calculate the zoom(otherwise the zoom will remains the same)
    if (win.width > win.height)
        size.1 <- round(c(640, 640 * win.height / win.width))
    else
        size.1 <- round(c(640 * win.width / win.height, 640))

	####stuff from 'Getmap' and 'Getmap.bbox' from 'rgooglemaps' package
	###overall, it does the transformation, by give a lititude and longitude and transform to the map resolution
	lat.center = mean(latR.odd)
	lon.center = mean(lonR.odd)
	zoom <- min(MaxZoom(latR.new, lonR.new, size.1))
	ll <- LatLon2XY(latR.new[1], lonR.new[1], zoom)
	ur <- LatLon2XY(latR.new[2], lonR.new[2], zoom)
	cr <- LatLon2XY(lat.center, lon.center, zoom)
	ll.Rcoords <- Tile2R(ll, cr)
	ur.Rcoords <- Tile2R(ur, cr)
	size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X)))) + 1
	size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y)))) + 1


	if(size[1] > size[2])
	{	
		size[2] = round(640*(size[2]/size[1]),0)
		size[1] = 640
	}else
	{
		size[1] = round(640*(size[1]/size[2]),0)
		size[2] = 640	
	}
		
	
	print(list(zoom = zoom, size = size))
	###hence the we will get the map with this zoom and size
	list(zoom = zoom, size = size)

}

