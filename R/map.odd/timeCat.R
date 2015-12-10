timeCat = function(currentTime, completeTime) {
    ## display computation time
    cat(paste0("done!", " (",
               round(completeTime - currentTime, digits = 2),
               " sec)", "\n"))
}
