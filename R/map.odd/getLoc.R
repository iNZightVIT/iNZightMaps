getLoc = function(data) {
    ## get longitude and latitude variables in data
    lon = getLon(data)
    lat = getLat(data)
    
    ## find the median
    lonMedian = median(data[!is.na(data[lon]), lon])
    latMedian = median(data[!is.na(data[lat]), lat])
    
    ## find the address by reverse geocoding the median position
    address = suppressMessages(
        revgeocode(location = c(lonMedian, latMedian), output = "more")
    )
    
    ## return the country
    return(as.character(address$country))
}
