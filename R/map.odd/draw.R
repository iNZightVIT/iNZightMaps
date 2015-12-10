draw = function(data, lon = NULL, lat = NULL, factor_by = NULL,
                colour_by = NULL, colour_by_cond = NULL,
                col = NULL, col_low = NULL, col_high = NULL,
                size_by = NULL, size_by_cond = NULL,
                size = NULL, size_low = NULL, size_high = NULL,
                location, zoom = 10, maptype = "terrain",
                src = "google", colour = "color", title = NULL,
                type, mode = NULL, geom = NULL, bins = NULL,
                grid = NULL, cols = NULL, solid = FALSE) {
    
    ## save all arguments & manipulate them
    allArgs = as.list(match.call())[-1]
    varSubsetArgs = c("data", "lon", "lat", "factor_by",
                      "colour_by", "colour_by_cond",
                      "size_by", "size_by_cond")
    arg = allArgs[which(names(allArgs) %in% varSubsetArgs)]
    
    ## find location
    if (is.character(location)) {
        arg$loc = getBB(location)
    } else {
        cat("geocoding location... ")
        currentTime = Sys.time()
        arg$loc = data.frame( north = location[1], east = location[2],
                              south = location[3], west = location[4])
        completeTime = Sys.time()
        timeCat(currentTime, completeTime)
    }
    
    ## subset data based on the arguments
    dat = do.call(varSubset, arg)
    
    ## generate a line of (gg) code based on the specified arugments.
    l = generateLine(type, mode, geom, grid, col, col_low, col_high,
                     size, size_low, size_high, factor_by, solid, cols)
    
    ## retrieve a basemap on which data points can be plotted
    baseMap = drawMap(arg$loc, zoom, maptype, src, colour)
    
    ## change x and y labs
    var = as.character(arg$var)
    cmdLine = paste("baseMap", l,
                    paste("labs(title = title,", "x = \"Longitude\",",
                          "y = \"Latitude\")"), sep = " + ")
    
    cat("mapping data... ")
    currentTime = Sys.time()
    if (is.null(colour_by))  { colour_by = size_by }
    if (is.null(size_by))    { size_by = colour_by }
    if (!is.null(factor_by)) {
        colour_by = factor_by
        n = length(unique(dat[[factor_by]]))
    }
    
    ## evaluation of the generated line of code
    map = suppressMessages(eval(parse(text = cmdLine)))
    completeTime = Sys.time()
    timeCat(currentTime, completeTime)
    
    return(map)
}
