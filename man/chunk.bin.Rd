\name{chunk.bin}
\alias{chunk.bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chunk.bin(filename, pimgs, weights = NULL, chunk = 2000, proj = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ ~~Describe \code{filename} here~~ }
  \item{pimgs}{ ~~Describe \code{pimgs} here~~ }
  \item{weights}{ ~~Describe \code{weights} here~~ }
  \item{chunk}{ ~~Describe \code{chunk} here~~ }
  \item{proj}{ ~~Describe \code{proj} here~~ }
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
function (filename, pimgs, weights = NULL, chunk = 2000, proj = NULL)
{
    con <- file(filename, open = "rb")
    dm <- readBin(con, "integer", 3)
    cnt <- round(seq(0, 100, length = dm[3]/chunk))
    i <- 1
    cat("\n")
    repeat {
        n <- dm[1] * dm[2] * as.integer(chunk)
        A <- readBin(con, "double", n)
        m <- as.integer(length(A)/(dm[1] * dm[2]))
        if (m == 0)
            break
        A <- array(A, c(dm[-3], m))
	if (!is.null(proj)) A[,1:2,] <- apply(A[,1:2,], 3, function(x) project(x, proj))
	pimgs <- behav.bin(A, pimgs, weights = weights)
	cat(cnt[i], "...\n", sep = "")
	i <- i + 1
    }
    cat("\n")
    close(con)
    pimgs
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}e
