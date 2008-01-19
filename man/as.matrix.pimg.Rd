\name{as.matrix.pimg}
\alias{as.matrix.pimg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{as.matrix}{pimg}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{pimg} here~~ }
  \item{\dots}{}
}
\details{
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(pimg) {

  img <- matrix(0,pimg$xbound[3],pimg$ybound[3])
  if(!is.null(pimg$image)) {
    off <- pimg$offset
    img[off[1]:(off[1]+nrow(pimg$image)-1),
        off[2]:(off[2]+ncol(pimg$image)-1)] <- pimg$image
  }
  img
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}