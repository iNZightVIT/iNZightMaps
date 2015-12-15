\name{latlon.xy}
\alias{latlon.xy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform the Latitude and Longitude into xy
}
\description{
Transform the Latitude and Longitude into xy-coordinate of the R plot.
}
\usage{
latlon.xy(data, map)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  the data set that use for plotting, the first cloumn needs to be Longitude and the second column needs to be Latitude.
  } 
  \item{map}{
  a map object from RgoogleMaps package.
  }
}
\details{
The map object can be generated from 'GetMap' and 'GetMap.bbox' functino from the package called 'RgoogleMaps'.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jason Wen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
library(RgoogleMap)
library(iNZightMaps)
lon = runif(10,0,100)
lat = runif(10,0,100)
data = data.frame(lon = lon,lat = lat)
center = c(50,50)
Map = GetMap(center = center,zoom = 2)
latlon.xy(data,Map)
iNZightPlot(lon, lat, data = data, plottype = "map", xlab="", ylab="")




