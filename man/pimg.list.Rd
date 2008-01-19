\name{pimg.list}
\alias{pimg.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pimg.list(times, xlim, ylim, img.dim, Z = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{ ~~Describe \code{times} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{img.dim}{ ~~Describe \code{img.dim} here~~ }
  \item{Z}{ ~~Describe \code{Z} here~~ }
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
function(times, xlim, ylim, img.dim, Z = TRUE) {

  #epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
  #tm <- 12*3600*(round(unclass(times)/(12*3600))) + epoch
  tm <- times

  ## if these are Zs, then the times specify the X times
  n <- length(tm) - Z

  lst <- vector("list",length=n)
  for(i in 1:n) lst[[i]] <- pimg(xlim[1], xlim[2], img.dim[1], ylim[1], ylim[2], img.dim[2])
  
  if (Z) {
  	Ztimes <- tm[-length(tm)] + diff(unclass(tm))/2
  	attr(lst, "times") <- Ztimes
  	attr(lst, "Xtimes") <- tm
  } else {
  	attr(lst, "times") <- tm
  }
  attr(lst, "Z") <- Z
   
  class(lst) <- c("pimg.list", "list")
  lst
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
