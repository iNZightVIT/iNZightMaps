.onLoad <- function(libname, pkgname) {
    if (!ggmap::has_stadiamaps_key()) {
        cat("Please register a Stadia Maps API key, then run:\n")
        cat('ggmap::register_stadiamaps("PASTE_KEY_HERE", write = TRUE)\n')
    }
}
