generateLine = function(type, mode = NULL, geom = "polygon", grid = NULL,
                        col = NULL, col_low = NULL, col_high = NULL,
                        size = NULL, size_low = NULL, size_high = NULL,
                        factor_by = NULL, solid = FALSE, cols = NULL) {
    # Helper function to generate lines of command to draw maps.
    # Returns lines as strings which are parsed as text in drawPoint() and
    # drawContour().
    
    l = "(aes_string(x = \"lon\", y = \"lat\"), data = dat, na.rm = TRUE)"
    re = "(^.+aes_string[(].+)[)](,.*data = dat.+$)"
    
    ## POINT ##
    if (type == "point") {
        pLine = paste0("geom_point", l)
        
        ## mode = "colour"
        if (any(grepl("colour", mode))) {
            pLine = gsub(re, "\\1, colour = colour_by)\\2", pLine)
            if (is.null(factor_by)) {
                pLine = paste(
                    pLine, "guides(colour = guide_colourbar(reverse = TRUE))",
                    sep = " + ")
            }
        }
        
        ## mode = "size"
        if (any(grepl("size", mode))) {
            pLine = gsub(re, "\\1, size = size_by)\\2", pLine)
            pLine = paste(pLine, "guides(size = guide_legend(size_by))",
                      sep = " + ")
        }
        
        ## mode = "alpha"
        if (any(grepl("alpha", mode))) {
            pLine = gsub(re, "\\1, alpha = colour_by)\\2", pLine)
            pLine = paste(pLine, "guides(alpha = FALSE)", sep = " + ")
        }
        
        ## mode = "shape"
        if (!is.null(factor_by) | any(grepl("shape", mode))) {
            pLine = gsub(re, "\\1, shape = factor_by)\\2", pLine)
            pLine = paste(
                pLine, "scale_shape_manual(values = 1:n)", sep = " + ")
            if (solid) {
                pLine = paste(
                    pLine, "scale_shape(solid = TRUE)", sep = " + ")
            }
        }
        
        ## col_low & col_high
        if (!is.null(col_low) & !is.null(col_high)) {
            pLine = paste(
                pLine, "scale_colour_gradient(low = col_low, high = col_high)",
                sep = " + ")
        }
        
        ## col
        if (!is.null(col)) {
            pLine = gsub(re, "\\1), colour = col\\2", pLine)
        }
        
        ## size
        if (!is.null(size) & !any(grepl("size", mode))) {
            pLine = gsub(re, "\\1), size = size\\2", pLine)
        }
        
        ## size_low & size_high
        if (!is.null(size_low) & !is.null(size_high) &
                any(grepl("size", mode))) {
            pLine = paste(pLine, "scale_size(range = c(size_low, size_high))",
                          sep = " + ")
        }
        
        ## grid
        if (!is.null(grid)) {
            if (factor) {
                if (length(grid) == 2)
                    pLine = paste(pLine, sep = " + ",
                                  paste("facet_wrap( ~ facetVariable,",
                                        "nrow = grid[1],", "ncol = grid[2])"))
                else
                    pLine = paste(pLine,
                                  paste0("facet_wrap( ~ facetVariable)"),
                                  sep = " + ")
            } else
                stop("Variable must be a factor to use grid!")
        }
        
        ## cols
#         if (!is.null(cols)) {
#             if (factor)
#                 pLine = paste(pLine, "scale_colour_manual(values = cols)",
#                               sep = " + ")
#             else
#                 stop ("Variable must be a factor to use cols!")
#         } else if (!is.null(cols.p)) {
#             if (factor)
#                 pLine = paste(pLine, "scale_colour_manual(values = cols.p)",
#                               sep = " + ")
#             else
#                 stop ("Variable must be a factor to use cols!")
#         }
        return(pLine)
    }
    
    ## CONTOUR ##
    if (type == "contour") {
        cLine = gsub(re, "stat_density2d\\1), bins = bins\\2", l)
        
        ## mode = "colour"
        if (any(grepl("colour", mode))) {
            cLine = gsub(re, "\\1, colour = \"..level..\")\\2", cLine)
            cLine = paste(
                cLine, "guides(colour = guide_colourbar(reverse = TRUE))",
                sep = " + ")
        }
        
        ## mode = "alpha"
        if (any(grepl("alpha", mode))) {
            cLine = gsub(re, "\\1, alpha = \"..level..\")\\2", cLine)
            cLine = paste(pLine, "guides(alpha = FALSE)", sep = " + ")
        }
        
        ## size
        if (!is.null(size) & !any(grepl("size", mode))) {
            cLine = gsub(re, "\\1), size = size\\2", cLine)
            cLine = paste(cLine, "guides(size = guide_legend(size_by))",
                          sep = " + ")
        }
        
        ## geom = "polygon"
        if (grepl("polygon", geom)) {
            cLine = gsub(re, "\\1), geom = \"polygon\"\\2", cLine)
            
            ## mode = "fill"
            if (any(grepl("fill", mode))) {
                cLine = gsub(re, "\\1, fill = \"..level..\")\\2", cLine)
                cLine = paste(cLine, "guides(fill = guide_legend(colour_by))",
                              sep = " + ")
            }
            
            ## col_low & col_high
            if (!is.null(col_low) & !is.null(col_high)) {
                cLine = paste(cLine, 
                              paste0("scale_fill_gradient",
                                     "(low = col_low, high = col_high)"),
                              sep = " + ")
            }
            
        ## geom = "density2d"
        } else if (grepl("density2d", geom)) {
            cLine = gsub(re, "\\1), geom = \"density2d\"\\2", cLine)
            
            ## col_low & col_high
            if (!is.null(col_low) & !is.null(col_high)) {
                if (all(!grepl("colour", mode))|all(!grepl("colour", mode))) {
                    cLine = gsub(re, "\\1, colour = \"..level..\")\\2", cLine)
                }
                l = paste0("scale_colour_gradient",
                           "(low = col_low, high = col_high)")
                cLine = paste(cLine, l, sep = " + ")
            }
        }
        
        ## col
        if (!is.null(col)) {
            cLine = gsub(re, "\\1), colour = col\\2", cLine)
        }
        
        ## size
        if (!is.null(size)) {
            cLine = gsub(re, "\\1), size = size\\2", cLine)
        }
        
        ## grid
        if (!is.null(grid)) {
            if (factor) {
                if (length(grid) == 2)
                    cLine = paste(cLine, sep = " + ",
                                  paste("facet_wrap( ~ facetVariable,",
                                        "nrow = grid[1],", "ncol = grid[2])"))
                else
                    cLine = paste(cLine,
                                  paste0("facet_wrap( ~ facetVariable)"),
                                  sep = " + ")
            } else
                stop("Variable must be a factor to use grid!")
        }
        
        return(cLine)
    }
    
    ## NO MATCH ##
    if (type != "point" | type != "contour" | type != "both")
        stop("Type must be either \"point\", \"contour\ or \"both\".")
}
