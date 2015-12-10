drawMap = function(location, zoom = 10, maptype = "terrain", src = "google",
                   colour = "color") {
    # Retrieve maps from GIS services.
    # Its main use is to set a base map on which data can be drawn.
    # Args:
    #   location: character or object from getBB()
    #   zoom, maptype & src: See ?get_map
    #   colour: "color" by default. Could be "bw".
    # Return:
    #   map of ggplot object
    
    ## save location in the right format
    if (inherits(location, "geoBBox")) {
        loc = c(lon = location$lon, lat = location$lat)
        if (location$type == "country" & zoom == 10) { zoom = 5 }
    } else if (is.character(location)) {
        locBB = getBB(location)
        is.data.frame(locBB)
        loc = c(lon = locBB$lon, lat = locBB$lat)
        if (locBB$type == "country" & zoom == 10) { zoom = 5 }
        if (is.null(loc)) { stop("location not found") }
    } else if (length(location) == 2) {
        loc = c(loc = location[1], lat = location[2])
    } else if (length(location) == 4) {
        loc = c(location$west, location$south, location$east, location$north)
    }
    
    cat("retrieving map... ")
    currentTime = Sys.time()
    
    ## retrieve a map based on the arugments and the location (loc)
    baseMapRaster = suppressWarnings(suppressMessages(
        get_map(location = loc, zoom = zoom, maptype = maptype, 
                source = src, color = colour)))
    
    completeTime = Sys.time()
    
    ## save the raster map as a ggplot object
    baseMap = ggmap(baseMapRaster)
    
    ## display the map on the R graphics device
    timeCat(currentTime, completeTime)
    return(baseMap)
}
