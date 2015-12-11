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
