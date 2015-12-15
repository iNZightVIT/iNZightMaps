\name{needNewMap}
\alias{needNewMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
needNewMap(bbox, window, size, SCALE, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbox}{
%%     ~~Describe \code{bbox} here~~
}
  \item{window}{
%%     ~~Describe \code{window} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{SCALE}{
%%     ~~Describe \code{SCALE} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (bbox, window, size, SCALE, type) 
{
    need = FALSE
    map.odd = global.objects$maps$map
    if (any(map.odd == NULL)) {
        need = TRUE
    }
    else {
        if (is.null(global.objects$maps$map.detail$bbox) || is.null(global.objects$maps$map.detail$size) || 
            is.null(global.objects$maps$map.detail$scale) || 
            is.null(global.objects$maps$map.detail$type) || is.null(global.objects$maps$map.detail$window)) {
            need = TRUE
        }
        else {
            a = global.objects$maps$map.detail$bbox
            b = bbox
            if (any(abs(a - b) > 0.5)) {
                global.objects$maps$map.detail$bbox = bbox
                need[1] = TRUE
            }
            else {
                need[1] = FALSE
            }
            if (any(global.objects$maps$map.detail$size != size)) {
                global.objects$maps$map.detail$size = size
                need[2] = TRUE
            }
            else {
                need[2] = FALSE
            }
            if (global.objects$maps$map.detail$scale != SCALE) {
                global.objects$maps$map.detail$scale = SCALE
                need[3] = TRUE
            }
            else {
                need[3] = FALSE
            }
            if (type != global.objects$maps$map.detail$type) {
                global.objects$maps$map.detail$type = type
                need[4] = TRUE
            }
            else {
                need[4] = FALSE
            }
            if (any(global.objects$maps$map.detail$window != 
                window)) {
                global.objects$maps$map.detail$window = window
                need[5] = TRUE
            }
            else {
                need[5] = FALSE
            }
        }
        any(need)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
