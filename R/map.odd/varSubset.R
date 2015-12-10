varSubset = function(data, lon = NULL, lat = NULL, factor_by = NULL,
                     colour_by = NULL, colour_by_cond = NULL, 
                     size_by = NULL, size_by_cond = NULL, location) {
    # Subset data set according to variable(s) and condition(s) specified.
    # Rows of longitude and latitude outside the bounding box of location are
    # dropped.
    # Args:
    #   data: data.frame
    #   var: variable
    #   var.cond: condition
    #   loc: location
    # Return:
    #   subsetted data frame
    # Example:
    #   > varSubset(eq.df, MAG, 4 <= eq.df$MAG & eq.df$MAG <= 5, loc = getBB("NZ"))
    
    lon = substitute(lon)
    lat = substitute(lat)
    if (!is.null(factor_by)) { factor_by = substitute(factor_by) }
    if (!is.null(colour_by)) { colour_by = substitute(colour_by) }
    if (!is.null(size_by))   { size_by = substitute(size_by)     }
    
    if (!is.character(lon)) { lon = deparse(lon) }
    if (!is.character(lat)) { lat = deparse(lat) }
    
    if (lon == "NULL") { lon = getLon(data) }
    if (lat == "NULL") { lat = getLat(data) }
    
    ## ggmap uses [0, 360] range for longitude, instead of [-180, 180]
    data[data$lon < 0, lon] = data[data$long < 0, lon] + 360
    
    ## find which rows have complete entries
    keepCols = c(lon, lat, colour_by, size_by, factor_by)
    completeRows = complete.cases(data[, keepCols])
    
    if (is.null(colnames(location))) {
        colnames(location) = c("north", "east", "south", "west")
    }
    
    ## bounding box conditions
    westCond = location$west <= data[, lon]
    eastCond = data[, lon] <= location$east
    southCond = location$south <= data[, lat]
    northCond = data[, lat] <= location$north
    
    ## specify the condition for subsetting
    location_cond = westCond & eastCond & southCond & northCond
    cond = completeRows & location_cond
    
    ## the condition is finalised based on which representative variable 
    ## conditions are specified
    if (!is.null(colour_by_cond)) {
        if (is.character(colour_by_cond))
            colour_by_cond = eval(parse(text = colour_by_cond))
        cond = cond & colour_by_cond
    }
    if (!is.null(size_by_cond)) {
        if (is.character(size_by_cond))
            size_by_cond = eval(parse(text = size_by_cond))
        cond = cond & size_by_cond
    }
    
    ## logically subset the data
    dat = data[cond, keepCols]
    
    ## display warning if there are no rows in the data
    if (nrow(dat) == 0)
        stop("No observations in data frame.")
    
    ## the first two columns are named "lon" and "lat"
    ## which are consistently used in other functions
    colnames(dat)[1:2] = c("lon", "lat")
    
    return(dat)
}
