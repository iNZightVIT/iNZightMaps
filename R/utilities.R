##' description
##'
##' details
##' @title
##' @param filename
##' @return
##' @export
##' @author
retrieveMap <- function(filename) {
    if(grepl(".rds$", filename)) {
        readRDS(filename)
    } else {
        sf::st_read(filename)
    }
}

##' Find the pair of variables from the map and data that have the
##' highest number of matches. This function returns in form: (data
##' variable, map variable)
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

##' @export
iNZightMapProjections <- function() {
    return(proj.df)
}

##' @export
iNZightMapCountryISO <- function() {
    return(country.isos)
}

##' @export
read.mapmetadata <- function(shapefileDir) {
    if (!file.exists(file.path(shapefileDir, "metadata.gz"))) {
        tryCatch({
            download.file("https://www.stat.auckland.ac.nz/~wild/data/shapefiles/metadata",
                          file.path(shapefileDir, "metadata.gz"), mode = "wb")
            },
                 error = function(e) error("Cannot download metadata file.")
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

##' @export
download.shapefiles <- function(dirURL, currPath, shapefileDir) {
    message("Searching... ", dirURL)
    ## curr.links <- XML::getHTMLLinks(RCurl::getURL(dirURL, dirlistonly = TRUE))
    curr.links <- XML::getHTMLLinks(rawToChar(curl::curl_fetch_memory(dirURL)$content))
    curr.dirs <- curr.links[grep("/$", curr.links)]
    curr.files <- curr.links[grep("\\.(rds|shp)", curr.links)]
    if (!dir.exists(currPath) && !dir.create(currPath))
        stop("Failed to create directory")
    for (filename in curr.files) {
        if (!file.exists(file.path(currPath, filename))) {
            download.file(paste0(dirURL, filename),
                          file.path(currPath, filename),
                          mode = "wb")
        }
    }

    for (dir in curr.dirs[-1]) {
        download.shapefiles(paste0(dirURL, dir),
                            file.path(currPath, dir),
                            shapefileDir)
    }
}

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
