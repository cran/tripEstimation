\name{combine}
\alias{combine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
combine(pimgs, subset = 1:length(pimgs))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pimgs}{ ~~Describe \code{pimgs} here~~ }
  \item{subset}{ ~~Describe \code{subset} here~~ }
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
function (pimgs, subset = 1:length(pimgs)) 
{
    bad <- unlist(lapply(pimgs, function(x) is.null(x$image)))[subset]
    subset <- subset[!bad]
    res <- as.image.pimg(pimgs[[subset[1]]])
    if (length(subset) == 1) 
        return(res)
    for (i in subset[-1]) {
        img <- pimgs[[i]]
        Xpos <- img$offset[1]
        Ypos <- img$offset[2]
        Xind <- Xpos:(Xpos + dim(img$image)[1] - 1)
        Yind <- Ypos:(Ypos + dim(img$image)[2] - 1)
        res$z[Xind, Yind] <- res$z[Xind, Yind] + img$image
    }
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
