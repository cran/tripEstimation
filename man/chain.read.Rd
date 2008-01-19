\name{chain.read}
\alias{chain.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chain.read(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ ~~Describe \code{filename} here~~ }
}
\details{}
\value{

}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }


\seealso{}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(filename) {
  con <- file(filename,open="rb")
  ## Read the dimensions of the array
  seek(con,0,origin="start",rw="r")
  dm <- readBin(con,"integer",3)
  ## Read the data and set dimensions
  n <- dm[1]*dm[2]*dm[3]
  A <- readBin(con,"double",n)
  if(length(A)!=n) stop("Read Error\n")
  close(con)
  array(A,dm)
  }
}
\keyword{ manip }% at least one, from doc/KEYWORDS

