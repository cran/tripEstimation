\name{chain.write}
\alias{chain.write}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chain.write(filename, A, append = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ ~~Describe \code{filename} here~~ }
  \item{A}{ ~~Describe \code{A} here~~ }
  \item{append}{ ~~Describe \code{append} here~~ }
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
function(filename,A,append=FALSE) {
  A <- as.array(A)
  if(!append) {
    ## Open a binary connection for writing, truncating or
    ## creating the file if necessary
    con <- file(filename,open="wb")
    ## Write array dimensions
    writeBin(as.integer(dim(A)),con)
    ## Write the data and close
    writeBin(as.double(A),con)
    close(con)
  } else {
    ## Open a binary connection for appending
    con <- file(filename,open="r+b")
    ## Read the dimensions of the array
    seek(con,0,origin="start",rw="r")
    dm <- readBin(con,"integer",3)
    ## Check the data to append has compatible dimensions
    if(length(dm)!=length(dim(A)) || any(dm[-3]!=dim(A)[-3]))
      stop("Incompatible dimensions for appending\n")
    ## Append the new data
    seek(con,0,origin="end",rw="w")
    writeBin(as.double(A),con)
    ## Write new dimensions and close
    dm[3] <- dm[3]+dim(A)[3]
    seek(con,0,origin="start",rw="w")
    writeBin(as.integer(dm),con)
    close(con)
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}