\name{unzipper}
\alias{unzipper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
unzipper(px)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{px}{ ~~Describe \code{px} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{



}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ 
}
\seealso{ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(px) {


## px is a list of pimg.lists
epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
tms <- unlist(lapply(px, function(x) attr(x, "times"))) 

  tms <- 12*3600*(round(unclass(tms)/(12*3600))) + epoch

xb <- px[[1]]$xbound
yb <- px[[1]]$ybound
#unique.times <- unique(sort(unique(tms)) + epoch)


p <- pimg.list(tms, xb[1], xb[2], xb[3], yb[1], yb[2], yb[3])


l <- list()


for (ix in px) l <- c(l, ix)

for (ip in 1:length(l)) p[[ip]] <- l[[ip]]

names(p) <- names(tms)
p
#attr(l, "times") <- tms
## px is all the pimgs matching tms

  }
}
\keyword{ manip }% at least one, from doc/KEYWORDS
