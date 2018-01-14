##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
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
