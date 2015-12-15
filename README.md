# iNZightMaps (beta)

A package for interacting with and visualising geographic data.


## Installation

Currently the package is only available on Github in beta. However, it has dependencies that are hosted at http://docker.stat.auckland.ac.nz/R.
```{r}
devtools::install_github("iNZightVIT/iNZightPlots@dev-plotmethods",
      repos = c("http://docker.stat.auckland.ac.nz/R", "http://cran.stat.auckland.ac.nz"))
devtools::install_github("iNZightVIT/iNZightMaps@dev-intergrateplots",
      repos = "http://cran.stat.auckland.ac.nz")
```

## Basic Usage

The package is a simple wrapper for the `iNZightPlots` function.

```{r}
library(iNZightMaps)
data(nzquakes)
quakes.obj <- iNZightMap(lat = ~Latitude, lon = ~Longitude, data = nzquakes)
plot(quakes.obj, sizeby = Magnitude, colby = Felt)
```
