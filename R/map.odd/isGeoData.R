## CHECKS for geographic data
isGeoData = function(data) {
    ret = TRUE
    if (hasLon(data) & hasLat(data)) {
        if (isLon(data) & isLat(data))
            return
    } else {
        # warning("No longitude and latitude in data")
        ret = FALSE
    }
    return(ret)
}

## GETTERS for variable names for longitude and latitude
getLon = function(data) { colnames(data)[grepl("^[Ll][Oo][Nn].*$", colnames(data))] }
getLat = function(data) { colnames(data)[grepl("^[Ll][Aa][Tt].*$", colnames(data))] }

## CHECKS whether data contains longitude and latitude
hasLon = function(data) { length(getLon(data)) > 0 }
hasLat = function(data) { length(getLat(data)) > 0 }

## CHECKS for longitutde and latitude values
isLon = function(data) {
    values = data[!is.na(data[[getLon(data)]]), getLon(data)]
    ret = TRUE
    if(any(!is.finite(values)) | any(values < -180) | any(values > 180)) {
        ret = FALSE
        stop("Longitude must be between -180 and 180.")
    }
    return(ret)
}

isLat = function(data) {
    values = data[!is.na(data[[getLat(data)]]), getLat(data)]
    ret = TRUE
    if(any(!is.finite(values)) | any(values < -90) | any(values > 90)) {
        ret = FALSE
        stop("Latitude must be between -90 and 90.")
    }
    return(ret)
}
