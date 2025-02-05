.onAttach <- function(libname, pkgname) {
    if (!ggmap::has_stadiamaps_key()) {
        packageStartupMessage("Please register a Stadia Maps API key, then run:\n")
        packageStartupMessage('ggmap::register_stadiamaps("PASTE_KEY_HERE", write = TRUE)\n')
    }
}
