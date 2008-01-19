\name{as.local.pimg}
\alias{as.local.pimg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
as.local.pimg(pimg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pimg}{ ~~Describe \code{pimg} here~~ }
}
\details{
}
\value{

 
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ }
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(pimg) {

  img <- coords.pimg(pimg)
  img$x <- img$x[pimg$offset[1]:(pimg$offset[1] + nrow(pimg$image)-1)]
  img$y <- img$y[pimg$offset[2]:(pimg$offset[2] + ncol(pimg$image)-1)]
  img$z <- pimg$image
  img
  }
}
\keyword{ manip }% at least one, from doc/KEYWORDS

