% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shade.fun.r
\name{lim.inside}
\alias{lim.inside}
\title{identify the which set of latitude and longitude are within the limit.}
\usage{
lim.inside(x, lim)
}
\arguments{
\item{x}{a numeric matrix of n*2 dimension.}

\item{lim}{a numeric vector of length 4.}
}
\value{
a logical vector of length of n, indicates which row in x is within in the limit.
}
\description{
is the latitude and longitude within the limit?
}
\author{
Jason Wen
}
