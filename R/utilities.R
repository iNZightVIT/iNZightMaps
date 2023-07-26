##' @title Retrieve a map for use in iNZightMapPlot
##' @param filename Filename of the map
##' @return An sf object containing the map
##' @export
retrieveMap <- function(filename) {
    if(grepl(".rds$", filename)) {
        readRDS(filename)
    } else {
        sf::st_read(filename)
    }
}

##' @title Find 'best-matching' pair of variables from map and data
##' @param data Data frame of the input data
##' @param map.data sf object of the map data
##' @return Two-element vector of form (data variable, map variable)
##' @export
findBestMatch <- function(data, map.data) {
    ## Eliminate variables that are associated with multiple regions
    ## in the map
    map.data <- map.data[, !(apply(map.data, 2, anyDuplicated, incomparables = c(NA, ""))), drop = FALSE]

    ## Only count the number of unique matches (prevents situations
    ## where one particular matched region may have so many matches it
    ## obfuscates the fact that few other regions were matched with
    ## that variable)
    match.sums <- vapply(data, function(x) {
        vapply(map.data, function(y) {
            sum(y %in% unique(x))
        }, numeric(1))
    }, numeric(ncol(map.data)))
    ## print(match.sums)
    match.sums <- matrix(match.sums, nrow = ncol(map.data), ncol = ncol(data))
    ## match.sums <- matrix(mapply(function(x, y) sum(y %in% unique(x)), data, map.data), nrow = ncol(map.data))

    ## Find indices of the best matching pair and return
    best.match <- arrayInd(which.max(match.sums), dim(match.sums))
    best.match.vars <- c(colnames(data)[best.match[2]], colnames(map.data)[best.match[1]])
    best.match.vars
}

##' @title Match two vectors
##' @param data.vect Vector containing the dataset variable
##' @param map.vect Vector containing the map object variable
##' @return A list containing: the dataset variable vector with duplicates removed, which values in the dataset and map variable had a match, how many matches occured overall and if there was multiple observations for any region.
##' @export
matchVariables <- function(data.vect, map.vect) {
    data.is.na <- is.na(data.vect)
    data.n.obs <- table(data.vect)
    data.vect <- as.character(unique(data.vect[!data.is.na]))
    map.vect <-  as.character(map.vect)

    matches <- match(data.vect, map.vect)
    data.matched <- !is.na(matches)
    map.matched <- logical(length(map.vect))
    map.matched[matches] <- TRUE

    list(data.vect = data.vect,
         data.matched = data.matched,
         map.matched = map.matched,
         matches = matches,
         multiple.obs = isTRUE(any(data.n.obs > 1)))
}

##' @title Get available map projections
##' @return Data frame of map projections
##' @export
iNZightMapProjections <- function() {
    return(proj.df)
}

##' @title Get list of country ISO codes
##' @return Data frame with country names and ISO codes
##' @export
iNZightMapCountryISO <- function() {
    return(country.isos)
}

##' @title Read shapefile metadata from file
##' @param shapefileDir Directory containing the shapefiles for iNZight
##' @return Matrix of filepaths, filenames and descriptions of each shapefile available in `shapefileDir`
##' @export
read.mapmetadata <- function(shapefileDir) {
    if (!file.exists(file.path(shapefileDir, "metadata.gz"))) {
        tryCatch({
            utils::download.file("https://www.stat.auckland.ac.nz/~wild/data/shapefiles/metadata",
                          file.path(shapefileDir, "metadata.gz"), mode = "wb")
            },
                 error = function(e) stop("Cannot download metadata file.")
                 )
    }

    metadata <- scan(file.path(shapefileDir, "metadata.gz"),
                     what = rep("character", 3), fill = TRUE,
                     comment.char = ";", sep = "\t",
                     fileEncoding = "UTF-8")


    metadata <- matrix(metadata, ncol = 3, byrow = TRUE)
    colnames(metadata) <- c("filepath", "tidy_filename", "description")
    metadata
}

##' @title Download shapefiles from remote repository
##' @param dirURL URL of the directory containing shapefiles
##' @param currPath Current path to save to
##' @export
download.shapefiles <- function(dirURL, currPath) {
    message("Searching... ", dirURL)
    curr.links <- XML::getHTMLLinks(rawToChar(curl::curl_fetch_memory(dirURL)$content))
    curr.dirs <- curr.links[grep("/$", curr.links)]
    curr.files <- curr.links[grep("\\.(rds|shp)", curr.links)]
    if (!dir.exists(currPath) && !dir.create(currPath))
        stop("Failed to create directory")
    for (filename in curr.files) {
        if (!file.exists(file.path(currPath, filename))) {
            utils::download.file(paste0(dirURL, filename),
                          file.path(currPath, filename),
                          mode = "wb")
        }
    }

    for (dir in curr.dirs[-1]) {
        download.shapefiles(paste0(dirURL, dir), file.path(currPath, dir))
    }
}

##' @title Convert map filepaths to "tidy" filepaths
##' @param mapdir.mat Matrix of metadata
##' @export
decodeMapDir <- function(mapdir.mat) {
    have.tidy <- !is.na(mapdir.mat[, "tidy_filename"])

    dir.vect <- as.character(mapdir.mat[, "x"])

    for (i in which(have.tidy)) {
        dir.vect[i] <- sub("/[-_\\df.A-z0-9]+\\.[A-z]+$",
                           paste0("/",mapdir.mat[i, "tidy_filename"]),
                           dir.vect[i])
    }

    for (i in which(!have.tidy)) {
        dir.vect[i] <- sub("\\.[A-z]+$", "", dir.vect[i])
    }
    which.countries <- which(grepl("^countries/", dir.vect))

    for (i in which.countries) {
        curr.code.ind <- regexpr("/(...)/", dir.vect[i])
        curr.code <- substr(dir.vect[i], curr.code.ind + 1, curr.code.ind + 3)
        curr.name <- country.isos[country.isos$iso == toupper(curr.code), "name"]
        dir.vect[i] <- sub("/(...)/", paste0("/", curr.name, "/"), dir.vect[i])
    }

    dir.vect <- sub("^([A-z])([A-z0-9]*)/", "\\U\\1\\E\\2/", dir.vect, perl = TRUE)

    mapdir.mat[, "tidy_filepath"] <- dir.vect
    mapdir.mat
}

##' @title Get the range of a set of variables while ignoring categorical variables
##' @param obj iNZightMapPlot object
##' @param vars Vector of variables to calculate range of
##' @return Vector of ranges for each variable
##' @export
getMinMax <- function(obj, vars) {
    if (length(vars) == 0) {
        return()
    }

    selected.data <- as.data.frame(obj$region.data)[, vars, drop = FALSE]
    range(selected.data[, sapply(selected.data, is.numeric)], na.rm = TRUE)
}

##' @title Get number of polygons for each region of a map
##' @param obj iNZightMapPlot object
##' @return Vector containing the number of polygons for each region of the map
##' @export
polygons_per_region <- function(obj) {
    data.to.use <- ifelse(obj$multiple.obs, "region.aggregate", "region.data")

    geo_types <- sf::st_geometry_type(obj[[data.to.use]])
    n_polygons <- numeric(length = length(geo_types))
    n_polygons[geo_types == "POLYGON"] <- 1
    n_polygons[geo_types == "MULTIPOLYGON"] <- lengths(sf::st_geometry(obj[[data.to.use]]))[geo_types == "MULTIPOLYGON"]

    n_polygons
}
