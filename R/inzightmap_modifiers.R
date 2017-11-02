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
  if(is.null(aes.var) || aes.var == "") {
    orig.mapping <- orig.mapping[names(orig.mapping) != aes.name]
  } else {
    orig.mapping[aes.name] <- aes.var
    
  }
  
  # Construct the new mapping based on the modified original
  new.aes <- eval(parse(text = paste0("ggplot2::aes_(", paste0(names(orig.mapping), " = ~", orig.mapping, collapse = ", "), ")")))
  
  arg.list <- c(list(data = orig.geom[[1]]$data, 
                     mapping = new.aes),
                orig.geom[[1]]$aes_params)
  
  # Overwrite the original layer with a newly constructed one with the new mapping
  mapplot.obj[[layer.set]][[layer.name]] <- do.call(ggplot2::geom_sf, arg.list)
  
  mapplot.obj
}

setConstant.iNZightMapPlot <- function(mapplot.obj,
                                       layer.set, layer.name = "baselayer",
                                       aes.name, aes.val) {
  layer.set <- layersetHelper(layer.set)
  orig.geom <- mapplot.obj[[layer.set]][[layer.name]]
  orig.mapping <- orig.geom[[1]]$mapping
  
  arg.list <- c(list(data = orig.geom[[1]]$data, 
                     mapping = orig.mapping),
                orig.geom[[1]]$aes_params)
  arg.list[[aes.name]] <- aes.val

  mapplot.obj[[layer.set]][[layer.name]] <- do.call(ggplot2::geom_sf, arg.list)
  
  mapplot.obj
}

#' Title
#'
#' @param mapplot.obj 
#'
#' @return
#' @export
#'
#' @examples
regionPoints.iNZightMapPlot <- function(mapplot.obj) {
  orig.data <- sf::st_as_sf(mapplot.obj[["map.layers"]][["baselayer"]][[1]]$data)
  orig.data <- orig.data[!is.na(sf::st_dimension(orig.data)), ]

  region.centres <- sf::st_centroid(orig.data)

  region.mapping <- as.character(mapplot.obj$map.layers$baselayer[[1]]$mapping[-1])["fill"]

  reg.mapplot <- addLayer.iNZightMapPlot(mapplot.obj,
                                         "point",
                                         "baselayer",
                                         ggplot2::geom_sf(data = region.centres,
                                                          mapping = ggplot2::aes_string(colour = region.mapping)))

  reg.mapplot <- setMapping.iNZightMapPlot(reg.mapplot,
                                           "map", "baselayer",
                                           "fill", NULL)
  reg.mapplot$type <- "point"
  reg.mapplot
}

regionLabels.iNZightMapPlot <- function(mapplot.obj) {
  addLayer.iNZightMapPlot(mapplot.obj, "point.layers", "regionlabels", geom_t)
}

#' Add a facet layer to an iNZightMapPlot object
#'
#' @param mapplot.obj An iNZightMapPlot object.
#' @param facet.var Which variable to facet over.
#'
#' @return An iNZightMapPlot object with a \code{facet} layer added to it
#' @export
#'
#' @examples
addFacet <- function(mapplot.obj, facet.var) {
  where.to.store <- if(mapplot.obj$type == "region") "map" else "point"

  if(!is.null(mapplot.obj[[where.to.store]]$facet)) {
    mapplot.obj <- removeLayer.iNZightMapPlot(mapplot.obj, where.to.store, "facet")
  }
  return.map <- addLayer.iNZightMapPlot(mapplot.obj, 
                                        layer.set = where.to.store, 
                                        layer.name = "facet", 
                                        ggplot2::facet_wrap(as.formula(paste0("~", facet.var))))
  
  return.map$facet.var <- facet.var
  
  return.map
}

changeZoom <- function(mapplot.obj, zoom.level) {
  
}
