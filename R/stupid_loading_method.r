###loading
source("C:/Users/jason/Desktop/inzight/just_some_function.r")
library(grid)
setwd('C:/Users/jason/Desktop/inzight/iNZightPlots-dev-plotmethods/R')
a = list.files('C:/Users/jason/Desktop/inzight/iNZightPlots-dev-plotmethods/R')
for(i in 1:length(a)){source(a[i])}
setwd('C:/Users/jason/Desktop/inzight/iNZightMaps-dev-integrateplots/R')
b = list.files('C:/Users/jason/Desktop/inzight/iNZightMaps-dev-integrateplots/R')
for(ii in 1:length(b)){source(b[ii])}
###loading