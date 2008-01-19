\name{behav.bin}
\alias{behav.bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
behav.bin(z, pimgs, weights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{pimgs}{ ~~Describe \code{pimgs} here~~ }
  \item{weights}{ ~~Describe \code{weights} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
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
function (z, pimgs, weights = NULL)
{
    if (is.null(weights) && attr(pimgs, "Z"))
        weights <- c(diff(unclass(attr(pimgs, "Xtimes"))/3600))
    if (is.null(weights)) weights <- rep(1, length(pimgs))
    
    
    if (!(length(weights) == length(pimgs))) stop("length of weights do not match length of p-img list")
    if (nrow(z) != length(pimgs))
        stop("dimensions of chain do not match length of p-img list")
#dm <- dim(z)

 
    for (k in 1:length(weights)) {
        pimgs[[k]] <- bin.pimg(pimgs[[k]], t(z[k, 1:2, ]), w = weights[k])
    }
    pimgs
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}