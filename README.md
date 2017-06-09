# iNZightMaps

|master|dev|
|:---|:---|
| [![Travis-CI Build Status](https://travis-ci.org/iNZightVIT/iNZightMaps.svg?branch=master)](https://travis-ci.org/iNZightVIT/iNZightMaps)|[![Travis-CI Build Status](https://travis-ci.org/iNZightVIT/iNZightMaps.svg?branch=dev)](https://travis-ci.org/iNZightVIT/iNZightMaps)|


A package for interacting with and visualising geographic data.
=======



## Basic Usage

The package is a simple wrapper for the `iNZightPlots` function.

```{r}
library(iNZightMaps)
data(nzquakes)
quakes.obj <- iNZightMap(lat = ~Latitude, lon = ~Longitude, data = nzquakes)
plot(quakes.obj, sizeby = Magnitude, colby = Felt)
```


## More Advanced Stuff

To get a satellite image and scale opacity by `Depth`:

```{r}
plot(quakes.obj, sizeby = Magnitude, colby = Felt, extra.vars = "Depth",
     plot.features = list(maptype = "satellite", opacity = "Depth"))
```
>>>>>>> origin/dev-integrateplots
