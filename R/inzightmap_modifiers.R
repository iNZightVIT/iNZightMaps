
## Adds a layer into an iNZightMapPlot object
addLayer.iNZightMapPlot <- function(mapplot.obj, layer.set, layer.name, layer.obj) {
  layer.set <- layersetHelper(layer.set)
  mapplot.obj[[layer.set]][[layer.name]] <- layer.obj
  mapplot.obj
}

## Removes a layer into an iNZightMapPlot object
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

## Modifies a mapping of a layer already contained in an iNZightMapPlot object.
## Since this function replaces the original mapping, this has two benefits:
## - the iNZightMapPlot object remains smaller in memory
## - the iNZightMapPlot object remains fast to plot, as only the required layers are present

## The layer given by layer.name MUST be a geom_sf() - it CANNOT be another
## type of layer (but the modification is simple)
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
