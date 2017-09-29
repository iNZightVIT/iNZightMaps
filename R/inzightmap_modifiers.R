#' Add a ggplot layer to an iNZightMapPlot object
#'
#' @param mapplot.obj An iNZightMapPlot object.
#' @param layer.set Which set of layers to add to --- \code{"map"} or \code{"region"}. 
#' @param layer.name Name to give to the added layer.
#' @param layer.obj The layer object to add.
#'
#' @return An iNZightMapPlot object with \code{layer.obj} stored within it.
#' @export
#'
#' @examples
#' 

addLayer.iNZightMapPlot <- function(mapplot.obj, layer.set, layer.name, layer.obj) {
  layer.set <- layersetHelper(layer.set)
  mapplot.obj[[layer.set]][[layer.name]] <- layer.obj
  mapplot.obj
}

#' Remove a ggplot layer from an iNZightMapPlot object
#'
#' @param mapplot.obj An iNZightMapPlot object.
#' @param layer.set Which set of layers the layer is contained in --- \code{"map"} or \code{"region"}.
#' @param layer.name Name of the layer to be removed.
#'
#' @return
#' @export
#'
#' @examples
#' 

removeLayer.iNZightMapPlot <- function(mapplot.obj, layer.set, layer.name) {
  layer.set <- layersetHelper(layer.set)
  mapplot.obj[[layer.set]][[layer.name]] <- NULL
  mapplot.obj
}

layersetHelper <- function(layer.set) {
  switch(layer.set, 
         map = "map.layers",
         point = "point.layers")
}

#' Set an aesthetic mapping to a layer in an iNZightMapPlot object
#' 
#' This function modifies an aesthetic mapping to an already existing ggplot
#' layer in an iNZightMapPlot object. By replacing the existing layer within the
#' iNZightMapPlot object with a new layer that is identical aside from the new
#' mapping, multiple mapping adjustments will not inflate the iNZightMapPlot
#' object size nor leave unnecessary layers that would slow down plotting.
#' 
#' @param mapplot.obj An iNZightMapPlot object.
#' @param layer.set Which set of layers the layer is contained in ---
#'   \code{"map"} or \code{"region"}.
#' @param layer.name Name of the layer to be modified. Must be a
#'   \code{geom_sf()} layer.
#' @param aes.name The name of the aesthetic to set
#' @param aes.var The variable to map to the aesthetic
#'   
#' @return An iNZightMapPlot object with the desired changes to
#'   \code{layer.name}.
#' @export
#' 
#' @example

setMapping.iNZightMapPlot <- function(mapplot.obj, 
                                      layer.set, layer.name = "baselayer", 
                                      aes.name, aes.var) {
  layer.set <- layersetHelper(layer.set)
  orig.geom <- mapplot.obj[[layer.set]][[layer.name]]
  
  # Extract the mapping that is already there
  orig.mapping <- as.character(orig.geom[[1]]$mapping)
  
  # Add in the new aesthetic (or replace an old one)
  if(!is.null(aes.var)) {
    orig.mapping[aes.name] <- aes.var
  } else {
    orig.mapping <- orig.mapping[names(orig.mapping) != aes.name]
  }
  
  # Construct the new mapping based on the modified original
  new.aes <- eval(parse(text = paste0("ggplot2::aes_(", paste0(names(orig.mapping), " = ~", orig.mapping, collapse = ", "), ")")))
  
  # Overwrite the original layer with a newly constructed one with the new mapping
  mapplot.obj[[layer.set]][[layer.name]] <- ggplot2::geom_sf(data = orig.geom[[1]]$data, mapping = new.aes)
  
  mapplot.obj
}
