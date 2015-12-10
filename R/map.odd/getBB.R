## GETTER for bounding box
getBB = function(location) {
    # Retrieve the bounding box (ie, boundaries) of location specified.
    # Its main use is to check whether a data set has any location values 
    # outside the boundaries of the location.
    # Args:
    #   location: character
    # Return:
    #   data frame
    # Example:
    #   > getBB("Auckland University")
    cat("geocoding location... ")
    currentTime = Sys.time()
    op = options(warn = 2) # upgrade warnings to errors to try()
    
    ## geocode location to retrieve its bounding box
    loc = try(suppressMessages(geocode(location, output = "more")),
              silent = TRUE)
    options(op)               ## reset the option
    completeTime = Sys.time()
    
    if (inherits(loc, "try-error"))
        return(cat("fail.\n"))
    
    timeCat(currentTime, completeTime)
    class(loc) = c(class(loc), "geoBBox")
    return(loc)
}
